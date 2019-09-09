package andel.text;

import andel.impl.text.TextImpl;
import andel.impl.text.Rope;
import andel.impl.text.TextMetrics;

@SuppressWarnings("unchecked")
public interface TextZipper {

  default TextZipper retain(long length) {
    return TextImpl.retain((Rope.Zipper<TextMetrics, String>)this, length);
  }

  default TextZipper insert(String text) {
    return TextImpl.insert((Rope.Zipper<TextMetrics, String>)this, text);
  }

  default TextZipper delete(long length) {
    return TextImpl.delete((Rope.Zipper<TextMetrics, String>)this, (int)length);
  }

  default TextZipper scanToCodepoint(long codePointOffset) {
    return TextImpl.scanToOffset((Rope.Zipper<TextMetrics, String>)this, codePointOffset);
  }

  default TextZipper scanToCharOffset(long charOffset) {
    return TextImpl.scanToCharOffset((Rope.Zipper<TextMetrics, String>)this, charOffset);
  }

  default TextZipper scanToLineStart(long lineNumber) {
    return TextImpl.scanToLineStart((Rope.Zipper<TextMetrics, String>)this, lineNumber);
  }

  default TextZipper scanToGeomOffset(long geomOffset) {
    return TextImpl.scanToGeomOffset((Rope.Zipper<TextMetrics, String>)this, geomOffset);
  }

  default long charOffset() {
    return TextImpl.charOffset((Rope.Zipper<TextMetrics, ?>)this);
  }

  default long codePointsOffset() {
    return TextImpl.offset((Rope.Zipper<TextMetrics, String>)this);
  }

  default long lineNumber() {
    return TextImpl.line((Rope.Zipper<TextMetrics, ?>)this);
  }

  default long geomOffset() {
    return TextImpl.geomOffset((Rope.Zipper<TextMetrics, String>)this);
  }

  default TextZipper asTransient() {
    return Rope.toTransient((Rope.Zipper)(this));
  }

  default TextZipper asPersistent() {
    return Rope.toPersistent((Rope.Zipper)this);
  }

  default Text makeText() {
    return new Text(TextImpl.root((Rope.Zipper<TextMetrics, String>)this));
  }

  default TextZipper consume(long codePointsLength, TextConsumer consumer) {
    return TextImpl.consumeText((Rope.Zipper<TextMetrics, String>)this, (int)codePointsLength, consumer);
  }

  default String text(long codePointsLength) {
    StringBuilder sb = new StringBuilder();
    this.consume(codePointsLength, sb::append);
    return sb.toString();
  }
}
