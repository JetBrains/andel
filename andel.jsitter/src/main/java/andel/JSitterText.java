package andel;

import andel.text.RopeImpl;
import andel.text.TextRope;
import andel.text.TextMetrics;
import andel.text.Tree;
import jsitter.api.Encoding;
import org.jetbrains.annotations.NotNull;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public class JSitterText implements jsitter.api.Text {
    private final Tree<TextMetrics, String> text;

    public static final int CHUNK_BYTES_SUZE = 1024;
    public static final Charset UTF16LE = Charset.forName("UTF-16LE");

    public JSitterText(Tree<TextMetrics, String> text) {
        this.text = text;
    }

    @NotNull
    @Override
    public Encoding getEncoding() {
        return Encoding.UTF16;
    }

    @Override
    public void read(int startByte, @NotNull ByteBuffer byteBuffer) {
        int startChar = startByte / 2;
        if (TextRope.charsCount(text) == startChar) {
            return;
        }
        RopeImpl.Zipper loc = TextRope.scanToCharOffset(TextRope.zipper(text), startChar);
        long textCharsCount = TextRope.charsCount(text);
        TextRope.reduceText(loc, (int) Math.min(CHUNK_BYTES_SUZE / 2, textCharsCount - startChar),
                byteBuffer,
                (buf, leaf, from, to) -> {
                    byte[] bytes = leaf.getBytes(UTF16LE);
                    return buf.put(bytes, 2 * from, 2 * (to - from));
                });
    }
}
