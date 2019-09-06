package andel;

import jsitter.api.Tree;

public class JSitterComponent implements Component {

    public final Tree ast;

    public JSitterComponent(Tree ast) {
        this.ast = ast;
    }

    @Override
    public Component edit(Text before, Text after, Edit edit) {
        return JSitterComponent(ast.adjust(asJSitterEdits(edit)))
    }

    public static List<jsitter.api.Edit> asJSitterEdits(Text before, Edit edit) {

    }
}
