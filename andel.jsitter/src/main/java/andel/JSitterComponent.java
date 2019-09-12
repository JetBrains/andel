package andel;

import andel.text.Text;
import andel.text.TextZipper;
import jsitter.api.Tree;

import java.util.ArrayList;
import java.util.List;

public class JSitterComponent implements Component {

  public final Tree ast;

  public JSitterComponent(Tree ast) {
    this.ast = ast;
  }

  @Override
  public Component edit(Text before, Text after, Edit edit) {
    return new JSitterComponent(ast.adjust(asJSitterEdits(before, edit)));
  }

  public static List<jsitter.api.Edit> asJSitterEdits(Text before, Edit edit) {
    ArrayList<jsitter.api.Edit> res = new ArrayList<>();
    TextZipper zipper = before.zipper().asTransient();
    int bytesOffset = 0;
    for (Object op : edit.ops) {
      if (op instanceof Edit.Retain) {
        long len = ((Edit.Retain)op).count;
        long bytesOffsetBefore = zipper.charOffset() * 2;
        zipper = zipper.retain(len);
        long bytesRetained = (zipper.charOffset() * 2) - bytesOffsetBefore;
        bytesOffset += bytesRetained;
      }
      else if (op instanceof Edit.Delete) {
        String s = ((Edit.Delete)op).text;
        res.add(new jsitter.api.Edit(bytesOffset, bytesOffset + s.length() * 2, bytesOffset));
        zipper = zipper.retain(s.codePointCount(0, s.length()));
      }
      else if (op instanceof Edit.Insert) {
        String s = ((Edit.Insert)op).text;
        int newBytesOffset = bytesOffset + (s.length() * 2);
        res.add(new jsitter.api.Edit(bytesOffset, bytesOffset, newBytesOffset));
        bytesOffset = newBytesOffset;
      }
    }
    return res;
  }
}
