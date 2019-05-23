package andel;
public class Cursor {


  static String leafText(Rope.Zipper zipper) {
    return (String)((Rope.Leaf)Rope.currentNode(zipper)).data;
  }

  public static class ImmutableCursor {

    Rope.Zipper zipper;
    long nodeCharOffset;
    long innerCharOffset;
    long offset;
    long textLength;
    long leafCharLength;

    public ImmutableCursor(Rope.Zipper zipper,
                           long nodeCharOffset,
                           long innerCharOffset,
                           long offset,
                           long textLength,
                           long leafCharLength) {
      this.zipper = zipper;
      this.nodeCharOffset = nodeCharOffset;
      this.innerCharOffset = innerCharOffset;
      this.offset = offset;
      this.textLength = textLength;
      this.leafCharLength = leafCharLength;
    }

    public ImmutableCursor(Rope.Node text, long _offset) {
      zipper = Text.scanToOffset(Text.zipper(text), offset);
      long _charOffset = Text.charOffset(zipper);
      nodeCharOffset = Text.nodeCharOffset(zipper);
      innerCharOffset = _charOffset - nodeCharOffset;
      offset = _offset;
      textLength =  Text.length(text);
      leafCharLength = leafText(zipper).length();
    }

    public int getChar() {
      return leafText(zipper).codePointAt((int)innerCharOffset);
    }

    public long getOffset() {
      return offset;
    }

    public long getCharOffset() {
      return nodeCharOffset + innerCharOffset;
    }
  }

  public static ImmutableCursor next(ImmutableCursor cursor) {
    long nextInnerCharOffset = Character.isSupplementaryCodePoint(cursor.getChar())
                               ? cursor.innerCharOffset + 2
                               : cursor.innerCharOffset + 1;

    if (nextInnerCharOffset < cursor.leafCharLength) {
      return new ImmutableCursor(
        cursor.zipper,
        cursor.nodeCharOffset,
        nextInnerCharOffset,
        cursor.offset + 1,
        cursor.textLength,
        cursor.leafCharLength
      );
    }

    if (Rope.hasNext(cursor.zipper)) {
      Rope.Zipper nextLeaf = Rope.nextLeaf(cursor.zipper);
      return new ImmutableCursor(
        nextLeaf,
        cursor.nodeCharOffset + cursor.leafCharLength,
        0,
        cursor.offset + 1,
        cursor.textLength,
        leafText(nextLeaf).length()
      );
    }

    return null;
  }

  public static ImmutableCursor prev(ImmutableCursor cursor) {

    if (0 < cursor.innerCharOffset) {
      long prevInnerCharOffset = Character.isHighSurrogate(leafText(cursor.zipper).charAt((int)(cursor.innerCharOffset - 1)))
               ? cursor.innerCharOffset - 2
               : cursor.innerCharOffset - 1;
      return new ImmutableCursor(
        cursor.zipper,
        cursor.nodeCharOffset,
        prevInnerCharOffset,
        cursor.offset - 1,
        cursor.textLength,
        cursor.leafCharLength
      );
    }

    if (Rope.hasPrev(cursor.zipper)) {
      Rope.Zipper prevLeaf = Rope.prevLeaf(cursor.zipper);
      String prevLeafText = leafText(prevLeaf);
      long prevLeafCharLength = prevLeafText.length();
      long prevInnerCharOffset = Character.isHighSurrogate(prevLeafText.charAt((int)(prevLeafCharLength - 1)))
                                 ? prevLeafCharLength - 2
                                 : prevLeafCharLength - 1;

      return new ImmutableCursor(
        prevLeaf,
        cursor.nodeCharOffset - prevLeafCharLength,
        prevInnerCharOffset,
        cursor.offset - 1,
        cursor.textLength,
        prevLeafCharLength
      );

    }

    return null;

  }




}
