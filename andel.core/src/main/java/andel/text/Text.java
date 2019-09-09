package andel.text;

import andel.impl.text.TextImpl;
import andel.impl.text.Rope;
import andel.impl.text.RopeCharSequence;
import andel.impl.text.TextMetrics;

import java.util.Objects;

public class Text {
  public static Text makeText(String text) {
    return new Text(TextImpl.makeText(text));
  }

  public final Rope.Tree<TextMetrics, String> rope;

  public Text(Rope.Tree<TextMetrics, String> rope) {
    this.rope = rope;
  }

  public long codePointsCount() {
    return TextImpl.length(this.rope);
  }

  public long linesCount() {
    return TextImpl.linesCount(this.rope);
  }

  public long charsCount() {
    return TextImpl.charsCount(this.rope);
  }

  public long maxLineLength() {
    return TextImpl.maxLineLength(this.rope);
  }

  public CharSequence charSequence() {
    return new RopeCharSequence(this.rope);
  }

  public CharSequence charSequence(int start, int end) {
    return new RopeCharSequence(this.rope, start, end);
  }

  public TextZipper zipper() {
    return TextImpl.zipper(this.rope);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Text text = (Text)o;
    return Objects.equals(rope, text.rope);
  }

  @Override
  public int hashCode() {
    return Objects.hash(rope);
  }
}
