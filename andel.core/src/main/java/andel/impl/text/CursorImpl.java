package andel.impl.text;

import andel.text.Cursor;

/*
 * Iterate through unicode code points in any direction
 * `next` and `prev` will return null after reaching the end of a text tree
 * */
@SuppressWarnings({"WeakerAccess", "unused"})
public class CursorImpl implements Cursor {
  String chunk;
  Rope.Zipper<TextMetrics, String> zipper;
  long nodeCharOffset;
  int innerCharOffset;
  long offset;
  final boolean isTransient;

  public CursorImpl(Rope.Zipper<TextMetrics, String> zipper,
                    String chunk,
                    long nodeCharOffset,
                    int innerCharOffset,
                    long offset,
                    boolean isTransient) {
    this.zipper = zipper;
    this.nodeCharOffset = nodeCharOffset;
    this.innerCharOffset = innerCharOffset;
    this.offset = offset;
    this.chunk = chunk;
    this.isTransient = isTransient;
  }

  public static CursorImpl create(Rope.Zipper<TextMetrics, String> zipper, boolean isTransient) {
    if (Rope.isBranch(zipper)) {
      throw new IllegalArgumentException("Will accept only leaf zippers");
    }

    long leafCharOffset = TextImpl.nodeCharOffset(zipper);
    long innerCharOffset = TextImpl.charOffset(zipper) - leafCharOffset;

    if (innerCharOffset == Rope.data(zipper).length()) {
      zipper = Rope.nextLeaf(zipper);
      leafCharOffset = TextImpl.nodeCharOffset(zipper);
      innerCharOffset = TextImpl.charOffset(zipper) - leafCharOffset;
    }

    return new CursorImpl(zipper,
                          Rope.data(zipper),
                          leafCharOffset,
                          (int)innerCharOffset,
                          TextImpl.offset(zipper),
                          isTransient);
  }

  public static CursorImpl createAtOffset(Rope.Tree<TextMetrics, String> text, long offset, boolean isTransient) {
    if (offset < 0 || offset >= TextImpl.length(text)) {
      throw new IllegalArgumentException("offset: " + offset + ", text length: " + TextImpl.length(text));
    }
    Rope.Zipper<TextMetrics, String> leaf = Rope.scan(TextImpl.zipper(text), TextImpl.byOffsetExclusive(offset));
    assert leaf != null && Rope.isLeaf(leaf);

    long leafCharOffset = TextImpl.nodeCharOffset(leaf);
    String s = Rope.data(leaf);
    int innerOffset = (int)(offset - TextImpl.nodeOffset(leaf));
    return new CursorImpl(leaf, s, leafCharOffset, s.offsetByCodePoints(0, innerOffset), offset, isTransient);
  }

  @Override
  public int codepoint() {
    return Rope.data(this.zipper).codePointAt(this.innerCharOffset);
  }

  @Override
  public long offset() {
    return this.offset;
  }

  @Override
  public long charOffset() {
    return this.nodeCharOffset + this.innerCharOffset;
  }

  @Override
  public Cursor asTransient() {
    return new CursorImpl(Rope.toTransient(this.zipper),
                          this.chunk,
                          this.nodeCharOffset,
                          this.innerCharOffset,
                          this.offset,
                          true);
  }

  @Override
  public boolean isTransient() {
    return this.isTransient;
  }

  @Override
  public Cursor asPersistent() {
    return new CursorImpl(Rope.toPersistent(this.zipper),
                          this.chunk,
                          this.nodeCharOffset,
                          this.innerCharOffset,
                          this.offset,
                          false);
  }

  @Override
  public Cursor next() {
    int nextInnerCharOffset = Character.isSupplementaryCodePoint(this.codepoint())
                              ? this.innerCharOffset + 2
                              : this.innerCharOffset + 1;
    if (nextInnerCharOffset < this.chunk.length()) {
      if (this.isTransient) {
        this.innerCharOffset = nextInnerCharOffset;
        this.offset++;
        return this;
      }
      else {
        return new CursorImpl(this.zipper,
                              this.chunk,
                              this.nodeCharOffset,
                              nextInnerCharOffset,
                              this.offset + 1,
                              false);
      }
    }
    else {
      if (Rope.hasNext(this.zipper)) {
        Rope.Zipper<TextMetrics, String> nextLeaf = Rope.nextLeaf(this.zipper);
        long nodeCharOffsetPrime = this.nodeCharOffset + this.chunk.length();
        int innerCharOffsetPrime = 0;
        long offsetPrime = this.offset + 1;
        String chunkPrime = Rope.data(nextLeaf);
        if (this.isTransient) {
          this.zipper = nextLeaf;
          this.nodeCharOffset = nodeCharOffsetPrime;
          this.innerCharOffset = innerCharOffsetPrime;
          this.offset = offsetPrime;
          this.chunk = chunkPrime;
          return this;
        }
        else {
          return new CursorImpl(nextLeaf,
                                chunkPrime,
                                nodeCharOffsetPrime,
                                innerCharOffsetPrime,
                                offsetPrime,
                                false);
        }
      }
      else {
        return null;
      }
    }
  }

  @Override
  public Cursor prev() {
    CursorImpl cursor = this;
    if (0 < cursor.innerCharOffset) {
      int prevInnerCharOffset = Character.isHighSurrogate(Rope.data(cursor.zipper).charAt(cursor.innerCharOffset - 1))
                                ? cursor.innerCharOffset - 2
                                : cursor.innerCharOffset - 1;
      if (cursor.isTransient) {
        this.innerCharOffset = prevInnerCharOffset;
        this.offset--;
        return this;
      }
      else {
        return new CursorImpl(cursor.zipper,
                              cursor.chunk,
                              cursor.nodeCharOffset,
                              prevInnerCharOffset,
                              cursor.offset - 1,
                              false);
      }
    }
    else if (Rope.hasPrev(cursor.zipper)) {
      Rope.Zipper<TextMetrics, String> prevLeaf = Rope.prevLeaf(cursor.zipper);
      String prevLeafText = Rope.data(prevLeaf);
      int prevLeafCharLength = prevLeafText.length();
      int prevInnerCharOffset = Character.isHighSurrogate(prevLeafText.charAt(prevLeafCharLength - 1))
                                ? prevLeafCharLength - 2
                                : prevLeafCharLength - 1;
      long nodeCharOffsetPrime = cursor.nodeCharOffset - prevLeafCharLength;
      long offsetPrime = cursor.offset - 1;
      if (this.isTransient) {
        this.zipper = prevLeaf;
        this.chunk = prevLeafText;
        this.nodeCharOffset = nodeCharOffsetPrime;
        this.innerCharOffset = prevInnerCharOffset;
        this.offset = offsetPrime;
        return this;
      }
      else {
        return new CursorImpl(prevLeaf,
                              prevLeafText,
                              nodeCharOffsetPrime,
                              prevInnerCharOffset,
                              offsetPrime,
                              false);
      }
    }
    else {
      return null;
    }
  }
}
