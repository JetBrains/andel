package andel;
@SuppressWarnings("unused")
public class Cursor {

  static String leafText(Rope.Zipper<Text.TextMetrics, String> zipper) {
    return Rope.data(zipper);
  }

  public static abstract class AbstractCursor {
    public Rope.Zipper<Text.TextMetrics, String> zipper;
    public long nodeCharOffset;
    public long innerCharOffset;
    public long offset;
    public long leafCharLength;

    AbstractCursor(Rope.Zipper<Text.TextMetrics, String> zipper,
                   long nodeCharOffset,
                   long innerCharOffset,
                   long offset,
                   long leafCharLength) {
      this.zipper = zipper;
      this.nodeCharOffset = nodeCharOffset;
      this.innerCharOffset = innerCharOffset;
      this.offset = offset;
      this.leafCharLength = leafCharLength;
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

  public static class ImmutableCursor extends AbstractCursor {

    ImmutableCursor(Rope.Zipper<Text.TextMetrics, String> zipper,
                    long nodeCharOffset,
                    long innerCharOffset,
                    long offset,
                    long leafCharLength) {
      super(zipper, nodeCharOffset, innerCharOffset, offset, leafCharLength);
    }

    public static ImmutableCursor create(Rope.Zipper<Text.TextMetrics, String> zipper){
      if (Rope.isBranch(zipper))
        throw new IllegalArgumentException();

      long leafCharOffset = Text.nodeCharOffset(zipper);
      long innerCharOffset = Text.charOffset(zipper) - leafCharOffset;
      return new ImmutableCursor(zipper,
                                 leafCharOffset,
                                 innerCharOffset,
                                 Text.offset(zipper),
                                 Rope.data(zipper).length());
    }

    public static ImmutableCursor create(Rope.Tree<Text.TextMetrics, String> text, long offset){
      if (offset < 0 && offset >= Text.length(text)) {
        throw new IllegalArgumentException("offset: " + offset + ", text length: " + Text.length(text));
      }
      Rope.Zipper<Text.TextMetrics, String> leaf = Rope.scan(Text.zipper(text), Text.byOffsetExclusive(offset));
      assert leaf != null && Rope.isLeaf(leaf);

      long leafCharOffset = Text.nodeCharOffset(leaf);
      String s = Rope.data(leaf);
      int innerOffset = (int)(offset - Text.nodeOffset(leaf));
      return new ImmutableCursor(leaf, leafCharOffset, s.offsetByCodePoints(0, innerOffset), offset, s.length());
    }
  }

  public static ImmutableCursor next(ImmutableCursor cursor) {
    long nextInnerCharOffset = Character.isSupplementaryCodePoint(cursor.getChar())
                               ? cursor.innerCharOffset + 2
                               : cursor.innerCharOffset + 1;

    if (nextInnerCharOffset < cursor.leafCharLength) {
      return new ImmutableCursor(cursor.zipper,
                                 cursor.nodeCharOffset,
                                 nextInnerCharOffset,
                                 cursor.offset + 1,
                                 cursor.leafCharLength);
    }

    if (Rope.hasNext(cursor.zipper)) {
      Rope.Zipper<Text.TextMetrics, String> nextLeaf = Rope.nextLeaf(cursor.zipper);
      return new ImmutableCursor(
        nextLeaf,
        cursor.nodeCharOffset + cursor.leafCharLength,
        0,
        cursor.offset + 1,
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
        cursor.leafCharLength
      );
    }
    else if (Rope.hasPrev(cursor.zipper)) {
      Rope.Zipper<Text.TextMetrics, String> prevLeaf = Rope.prevLeaf(cursor.zipper);
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
        prevLeafCharLength
      );
    }
    else {
      return null;
    }
  }

  public static class TransientCursor extends AbstractCursor {

    public TransientCursor(Rope.Zipper<Text.TextMetrics, String> zipper,
                           long nodeCharOffset,
                           long innerCharOffset,
                           long offset,
                           long leafCharLength) {
      super(zipper, nodeCharOffset, innerCharOffset, offset, leafCharLength);
    }

    public static TransientCursor create(Rope.Zipper<Text.TextMetrics, String> zipper) {
      if (Rope.isBranch(zipper)) {
        throw new IllegalArgumentException();
      }

      long leafCharOffset = Text.nodeCharOffset(zipper);
      long innerCharOffset = Text.charOffset(zipper) - leafCharOffset;
      return new TransientCursor(Rope.toTransient(zipper),
                                 leafCharOffset,
                                 innerCharOffset,
                                 Text.offset(zipper),
                                 Rope.data(zipper).length());
    }

    public static TransientCursor createAtOffset(Rope.Tree<Text.TextMetrics, String> text, long offset) {
      return create(Text.scanToOffset(Text.zipper(text), offset));
    }

    public TransientCursor next() {
      long nextInnerCharOffset = Character.isSupplementaryCodePoint(getChar())
                                 ? innerCharOffset + 2
                                 : innerCharOffset + 1;
      if (nextInnerCharOffset < leafCharLength) {
        this.offset += 1;
        this.innerCharOffset = nextInnerCharOffset;
        return this;
      }
      else if (Rope.hasNext(zipper)) {
        Rope.Zipper<Text.TextMetrics, String> nextLeaf = Rope.nextLeaf(zipper);

        this.zipper = nextLeaf;
        this.nodeCharOffset += leafCharLength;
        this.innerCharOffset = 0;
        this.offset += 1;
        this.leafCharLength = leafText(nextLeaf).length();

        return this;
      }
      else {
        return null;
      }
    }

    public TransientCursor prev() {
      if (0 < innerCharOffset) {
        innerCharOffset = Character.isHighSurrogate(leafText(zipper).charAt((int)(innerCharOffset - 1)))
                          ? innerCharOffset - 2
                          : innerCharOffset - 1;
        offset -= 1;
        return this;
      }
      else if (Rope.hasPrev(zipper)) {
        Rope.Zipper<Text.TextMetrics, String> prevLeaf = Rope.prevLeaf(zipper);
        String prevLeafText = leafText(prevLeaf);
        long prevLeafCharLength = prevLeafText.length();
        long prevInnerCharOffset = Character.isHighSurrogate(prevLeafText.charAt((int)(prevLeafCharLength - 1)))
                                   ? prevLeafCharLength - 2
                                   : prevLeafCharLength - 1;
        zipper = prevLeaf;
        nodeCharOffset -= prevLeafCharLength;
        innerCharOffset = prevInnerCharOffset;
        offset -= 1;
        leafCharLength = prevLeafCharLength;
        return this;
      }
      else {
        return null;
      }
    }
  }

  public static TransientCursor toTransient(ImmutableCursor cursor) {
    return new TransientCursor(
      Rope.toTransient(cursor.zipper),
      cursor.nodeCharOffset,
      cursor.innerCharOffset,
      cursor.offset,
      cursor.leafCharLength
    );
  }

  public static ImmutableCursor toPersistent(TransientCursor cursor) {
    return new ImmutableCursor(
      Rope.toPersistent(cursor.zipper),
      cursor.nodeCharOffset,
      cursor.innerCharOffset,
      cursor.offset,
      cursor.leafCharLength
    );
  }
}
