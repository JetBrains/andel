package andel;

/*
 * Iterate through unicode code points in any direction
 * `next` and `prev` will return null after reaching the end of a text tree
 * */
@SuppressWarnings({"WeakerAccess", "unused"})
public class Cursor {

  public static int codepoint(AbstractCursor cursor) {
    return Rope.data(cursor.zipper).codePointAt(cursor.innerCharOffset);
  }

  public static long offset(AbstractCursor cursor) {
    return cursor.offset;
  }

  public static long charOffset(AbstractCursor cursor) {
    return cursor.nodeCharOffset + cursor.innerCharOffset;
  }

  static abstract class AbstractCursor {
    Rope.Zipper<Text.TextMetrics, String> zipper;
    long nodeCharOffset;
    int innerCharOffset;
    long offset;
    long leafCharLength;

    AbstractCursor(Rope.Zipper<Text.TextMetrics, String> zipper,
                   long nodeCharOffset,
                   int innerCharOffset,
                   long offset,
                   long leafCharLength) {
      this.zipper = zipper;
      this.nodeCharOffset = nodeCharOffset;
      this.innerCharOffset = innerCharOffset;
      this.offset = offset;
      this.leafCharLength = leafCharLength;
    }
  }

  public static class PersistentCursor extends AbstractCursor {

    PersistentCursor(Rope.Zipper<Text.TextMetrics, String> zipper,
                     long nodeCharOffset,
                     int innerCharOffset,
                     long offset,
                     long leafCharLength) {
      super(zipper, nodeCharOffset, innerCharOffset, offset, leafCharLength);
    }

    public static PersistentCursor create(Rope.Zipper<Text.TextMetrics, String> zipper) {
      if (Rope.isBranch(zipper)) {
        throw new IllegalArgumentException("Will accept only leaf zippers");
      }

      long leafCharOffset = Text.nodeCharOffset(zipper);
      long innerCharOffset = Text.charOffset(zipper) - leafCharOffset;
      return new PersistentCursor(zipper,
                                  leafCharOffset,
                                  (int)innerCharOffset,
                                  Text.offset(zipper),
                                  Rope.data(zipper).length());
    }

    public static PersistentCursor createAtOffset(Rope.Tree<Text.TextMetrics, String> text, long offset) {
      if (offset < 0 && offset >= Text.length(text)) {
        throw new IllegalArgumentException("offset: " + offset + ", text length: " + Text.length(text));
      }
      Rope.Zipper<Text.TextMetrics, String> leaf = Rope.scan(Text.zipper(text), Text.byOffsetExclusive(offset));
      assert leaf != null && Rope.isLeaf(leaf);

      long leafCharOffset = Text.nodeCharOffset(leaf);
      String s = Rope.data(leaf);
      int innerOffset = (int)(offset - Text.nodeOffset(leaf));
      return new PersistentCursor(leaf, leafCharOffset, s.offsetByCodePoints(0, innerOffset), offset, s.length());
    }
  }

  public static PersistentCursor next(PersistentCursor cursor) {
    long nextInnerCharOffset = Character.isSupplementaryCodePoint(codepoint(cursor))
                               ? cursor.innerCharOffset + 2
                               : cursor.innerCharOffset + 1;

    if (nextInnerCharOffset < cursor.leafCharLength) {
      return new PersistentCursor(cursor.zipper,
                                  cursor.nodeCharOffset,
                                  (int)nextInnerCharOffset,
                                  cursor.offset + 1,
                                  cursor.leafCharLength);
    }

    if (Rope.hasNext(cursor.zipper)) {
      Rope.Zipper<Text.TextMetrics, String> nextLeaf = Rope.nextLeaf(cursor.zipper);
      return new PersistentCursor(
        nextLeaf,
        cursor.nodeCharOffset + cursor.leafCharLength,
        0,
        cursor.offset + 1,
        Rope.data(nextLeaf).length()
      );
    }

    return null;
  }

  public static PersistentCursor prev(PersistentCursor cursor) {
    if (0 < cursor.innerCharOffset) {
      long prevInnerCharOffset = Character.isHighSurrogate(Rope.data(cursor.zipper).charAt(cursor.innerCharOffset - 1))
                                 ? cursor.innerCharOffset - 2
                                 : cursor.innerCharOffset - 1;
      return new PersistentCursor(cursor.zipper,
                                  cursor.nodeCharOffset,
                                  (int)prevInnerCharOffset,
                                  cursor.offset - 1,
                                  cursor.leafCharLength
      );
    }
    else if (Rope.hasPrev(cursor.zipper)) {
      Rope.Zipper<Text.TextMetrics, String> prevLeaf = Rope.prevLeaf(cursor.zipper);
      String prevLeafText = Rope.data(prevLeaf);
      int prevLeafCharLength = prevLeafText.length();
      int prevInnerCharOffset = Character.isHighSurrogate(prevLeafText.charAt(prevLeafCharLength - 1))
                                ? prevLeafCharLength - 2
                                : prevLeafCharLength - 1;

      return new PersistentCursor(
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

    TransientCursor(Rope.Zipper<Text.TextMetrics, String> zipper,
                    long nodeCharOffset,
                    int innerCharOffset,
                    long offset,
                    long leafCharLength) {
      super(zipper, nodeCharOffset, innerCharOffset, offset, leafCharLength);
    }

    public static TransientCursor create(Rope.Zipper<Text.TextMetrics, String> zipper) {
      if (Rope.isBranch(zipper)) {
        throw new IllegalArgumentException("Will accept only leaf zippers");
      }

      long leafCharOffset = Text.nodeCharOffset(zipper);
      long innerCharOffset = Text.charOffset(zipper) - leafCharOffset;
      return new TransientCursor(Rope.toTransient(zipper),
                                 leafCharOffset,
                                 (int)innerCharOffset,
                                 Text.offset(zipper),
                                 Rope.data(zipper).length());
    }

    public static TransientCursor createAtOffset(Rope.Tree<Text.TextMetrics, String> text, long offset) {
      return create(Text.scanToOffset(Text.zipper(text), offset));
    }

    public TransientCursor next() {
      int nextInnerCharOffset = Character.isSupplementaryCodePoint(codepoint(this))
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
        this.leafCharLength = Rope.data(nextLeaf).length();

        return this;
      }
      else {
        return null;
      }
    }

    public TransientCursor prev() {
      if (0 < innerCharOffset) {
        innerCharOffset = Character.isHighSurrogate(Rope.data(zipper).charAt(innerCharOffset - 1))
                          ? innerCharOffset - 2
                          : innerCharOffset - 1;
        offset -= 1;
        return this;
      }
      else if (Rope.hasPrev(zipper)) {
        Rope.Zipper<Text.TextMetrics, String> prevLeaf = Rope.prevLeaf(zipper);
        String prevLeafText = Rope.data(prevLeaf);
        int prevLeafCharLength = prevLeafText.length();
        int prevInnerCharOffset = Character.isHighSurrogate(prevLeafText.charAt(prevLeafCharLength - 1))
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

  public static TransientCursor toTransient(PersistentCursor cursor) {
    return new TransientCursor(Rope.toTransient(cursor.zipper),
                               cursor.nodeCharOffset,
                               cursor.innerCharOffset,
                               cursor.offset,
                               cursor.leafCharLength
    );
  }

  public static PersistentCursor toPersistent(TransientCursor cursor) {
    return new PersistentCursor(Rope.toPersistent(cursor.zipper),
                                cursor.nodeCharOffset,
                                cursor.innerCharOffset,
                                cursor.offset,
                                cursor.leafCharLength
    );
  }
}
