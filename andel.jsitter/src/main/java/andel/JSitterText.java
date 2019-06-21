package andel;

import jsitter.api.Encoding;
import org.jetbrains.annotations.NotNull;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public class JSitterText implements jsitter.api.Text {
    private final Rope.Tree<Text.TextMetrics, String> text;

    public static final int CHUNK_BYTES_SUZE = 1024;
    public static final Charset UTF16LE = Charset.forName("UTF-16LE");

    public JSitterText(Rope.Tree<Text.TextMetrics, String> text) {
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
        Rope.Zipper loc = Text.scanToCharOffset(Text.zipper(text), startChar);
        long textCharsCount = Text.charsCount(text);
        Text.reduceText(loc, (int) Math.min(CHUNK_BYTES_SUZE / 2, textCharsCount - startChar),
                byteBuffer,
                (buf, leaf, from, to) -> {
                    byte[] bytes = leaf.getBytes(UTF16LE);
                    return buf.put(bytes, 2 * from, 2 * (to - from));
                });
    }
}
