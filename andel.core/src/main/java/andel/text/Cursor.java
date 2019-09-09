package andel.text;

import andel.impl.text.CursorImpl;
import andel.impl.text.Rope;
import andel.impl.text.TextMetrics;

public interface Cursor {
  int codepoint();

  long offset();

  long charOffset();

  Cursor asTransient();

  boolean isTransient();

  Cursor asPersistent();

  Cursor next();

  Cursor prev();

  static Cursor atOffset(Text text, long offset, boolean isTransient) {
    return CursorImpl.createAtOffset(text.rope, offset, isTransient);
  }

  @SuppressWarnings("unchecked")
  static Cursor create(TextZipper zipper, boolean isTransient) {
    return CursorImpl.create((Rope.Zipper<TextMetrics, String>)zipper, isTransient);
  }
}
