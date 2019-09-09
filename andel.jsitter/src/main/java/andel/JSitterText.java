package andel;

import andel.text.Text;
import andel.text.TextZipper;
import jsitter.api.Encoding;
import org.jetbrains.annotations.NotNull;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public class JSitterText implements jsitter.api.Text {
    private final Text text;

    public static final int CHUNK_BYTES_SUZE = 1024;
    public static final Charset UTF16LE = Charset.forName("UTF-16LE");

    public JSitterText(Text text) {
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
        if (text.charsCount() == startChar) {
            return;
        }
        TextZipper loc = text.zipper().scanToCharOffset(startChar);
        loc.consume((int)Math.min(CHUNK_BYTES_SUZE / 2, text.charsCount() - startChar), (leaf, from, to) -> {
            byte[] bytes = leaf.getBytes(UTF16LE);
            byteBuffer.put(bytes, 2 * from, 2 * (to - from));
        });
    }
}
