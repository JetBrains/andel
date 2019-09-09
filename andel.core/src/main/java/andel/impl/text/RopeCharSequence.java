package andel.impl.text;

public class RopeCharSequence implements CharSequence {

  Rope.Tree<TextMetrics, String> root;
  Rope.Zipper<TextMetrics, String> zipper;
  final int fromChar, toChar;

  public RopeCharSequence(Rope.Tree<TextMetrics, String> root, int from, int to) {
    if (from < 0 || to > root.metrics.charsCount)
      throw new IllegalArgumentException("from " + from + ", to " + to + ", total " + root.metrics.charsCount);

    this.root = root;
    this.fromChar = from;
    this.toChar = to;
    Rope.Zipper<TextMetrics, String> z = Rope.toTransient(TextImpl.zipper(root));
    this.zipper = Rope.scan(z, TextImpl.byCharOffsetExclusive(from));
  }

  @SuppressWarnings("unused")
  public RopeCharSequence(Rope.Tree<TextMetrics, String> root) {
    this(root, 0, (int)(root.metrics).charsCount);
  }

  @Override
  public int length() {
    return toChar - fromChar;
  }

  @Override
  public char charAt(int index) {
    if (index > toChar - fromChar || index < 0)
      throw new IndexOutOfBoundsException("index:" + index + ", from:" + fromChar + ", to:" + toChar);

    int absoluteCharOffset = fromChar + index;
    int nodeCharOffset = (int)TextImpl.nodeCharOffset(zipper);
    String currentChunk = Rope.data(zipper);

    if (absoluteCharOffset < nodeCharOffset) {
      Rope.Zipper<TextMetrics, String> rootLoc = Rope.toTransient(TextImpl.zipper(root));
      Rope.Zipper<TextMetrics, String> offsetZipper = Rope.scan(rootLoc, TextImpl.byCharOffsetExclusive(absoluteCharOffset));
      assert offsetZipper != null;
      this.zipper = offsetZipper;
      String newChunk = Rope.data(offsetZipper);
      return newChunk.charAt(absoluteCharOffset - (int) TextImpl.nodeCharOffset(offsetZipper));
    }
    else if (absoluteCharOffset < nodeCharOffset + currentChunk.length()) {
      return currentChunk.charAt(absoluteCharOffset - nodeCharOffset);
    }
    else {
      Rope.Zipper<TextMetrics, String> offsetLoc = Rope.scan(zipper, TextImpl.byCharOffsetExclusive(absoluteCharOffset));
      assert offsetLoc != null;
      this.zipper = offsetLoc;
      String newChunk = Rope.data(offsetLoc);
      return newChunk.charAt(absoluteCharOffset - (int)TextImpl.nodeCharOffset(offsetLoc));
    }
  }

  @Override
  public CharSequence subSequence(int start, int end) {
    int length = toChar - fromChar;
    if (start > length || end > length)
      throw new IndexOutOfBoundsException("start " + start + ", end " + end + ", length " + length);
    if (start > end)
      throw new IllegalArgumentException("start " + start + " > end " + end);

    return new RopeCharSequence(root, fromChar + start, fromChar + end);
  }

  @Override
  public String toString() {
    Rope.Zipper<TextMetrics, String> fromLoc = TextImpl.scanToCharOffset(TextImpl.zipper(root), fromChar);
    Rope.Zipper<TextMetrics, String> toLoc = TextImpl.scanToCharOffset(fromLoc, toChar);
    return fromLoc.text((int)(TextImpl.offset(toLoc) - TextImpl.offset(fromLoc)));
  }

  public static boolean contentEquals(CharSequence one, CharSequence another) {
    if (one instanceof RopeCharSequence && another instanceof RopeCharSequence){
      RopeCharSequence o = (RopeCharSequence) one;
      RopeCharSequence a = (RopeCharSequence) another;
      if (o.root == a.root && o.fromChar == a.fromChar && o.toChar == a.toChar){
        return true;
      }
      // TODO fast path : check if leafs are identical
    }

    int n = one.length();
    if (n != another.length()) {
      return false;
    }
    for (int i = 0; i < n; i++) {
      if (one.charAt(i) != another.charAt(i)) {
        return false;
      }
    }
    return true;
  }
}
