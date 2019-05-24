package andel;

import andel.Rope.ZipperOps;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;


public class Text {

  public final static class TextMetrics {

    public final long length;
    public final long geometricLength;
    public final long linesCount;
    public final long charsCount;
    public final long newlinePrefixGeomLength;
    public final long newlineSuffixGeomLength;
    public final long maxLineLength;

    public TextMetrics(long length,
                       long geometricLength,
                       long linesCount,
                       long charsCount,
                       long newlinePrefixGeomLength,
                       long newlineSuffixGeomLength,
                       long maxLineLength) {
      this.length = length;
      this.geometricLength = geometricLength;
      this.linesCount = linesCount;
      this.charsCount = charsCount;
      this.newlinePrefixGeomLength = newlinePrefixGeomLength;
      this.newlineSuffixGeomLength = newlineSuffixGeomLength;
      this.maxLineLength = maxLineLength;
    }

    public static final TextMetrics EMPTY = new TextMetrics();

    public TextMetrics() {
      this.length = 0;
      this.geometricLength = 0;
      this.linesCount = 0;
      this.charsCount = 0;
      this.newlinePrefixGeomLength = 0;
      this.newlineSuffixGeomLength = 0;
      this.maxLineLength = 0;
    }

    public TextMetrics add(TextMetrics other) {
      long length = this.length + other.length;
      long geomLength = this.geometricLength + other.geometricLength;
      long linesCount = this.linesCount + other.linesCount;
      long charsCount = this.charsCount + other.charsCount;
      long newlinePrefixGeomLength = this.linesCount == 0
                                     ? this.newlinePrefixGeomLength + other.newlinePrefixGeomLength
                                     : this.newlinePrefixGeomLength;
      long maxLineLength = Math.max(Math.max(this.maxLineLength, other.maxLineLength),
                                    this.newlineSuffixGeomLength + other.newlinePrefixGeomLength);
      long newlineSuffixGeomLength = other.linesCount == 0
                                     ? this.newlineSuffixGeomLength + other.newlineSuffixGeomLength
                                     : other.newlineSuffixGeomLength;

      return new TextMetrics(length,
                             geomLength,
                             linesCount,
                             charsCount,
                             newlinePrefixGeomLength,
                             maxLineLength,
                             newlineSuffixGeomLength);
    }
  }

  public enum OffsetKind {
    CodePoints,
    Characters,
    Geom
  }

  public static TextMetrics metricsTo(String str, OffsetKind kind, long offset) {
    long prevLineGeomOffset = 0;
    long maxLineLength = 0;
    long newlinePrefixGeomLength = 0;
    long codePointsCount = 0;
    int charsCount = 0;
    long geometricLength = 0;
    long linesCount = 0;

    while (kind == OffsetKind.CodePoints && codePointsCount < offset ||
           kind == OffsetKind.Characters && charsCount < offset ||
           kind == OffsetKind.Geom && geometricLength < offset) {
      int codepoint = str.codePointAt(charsCount);

      if (codepoint == '\n') {
        maxLineLength = Math.max(Math.max(maxLineLength, newlinePrefixGeomLength), codePointsCount - prevLineGeomOffset);
        newlinePrefixGeomLength = linesCount == 0 ? geometricLength : newlinePrefixGeomLength;
        prevLineGeomOffset = geometricLength;
        linesCount += 1;
      }

      codePointsCount += 1;
      geometricLength += codepoint == '\t' ? 4 : 1;

      charsCount += Character.charCount(codepoint);
    }

    long newlineSuffixGeomLength = geometricLength - prevLineGeomOffset - (linesCount == 0 ? 1 : 0);

    newlinePrefixGeomLength = linesCount == 0 ? codePointsCount : newlinePrefixGeomLength;
    maxLineLength = Math.max(maxLineLength, newlineSuffixGeomLength);

    return new TextMetrics(codePointsCount,
                           geometricLength,
                           linesCount,
                           charsCount,
                           newlinePrefixGeomLength,
                           newlineSuffixGeomLength,
                           maxLineLength);
  }

  public static final TextOps TEXT_OPS = new TextOps(32, 64);

  public static class TextOps implements ZipperOps<TextMetrics, String> {

    final int splitThresh;
    final int leafSplitThresh;
    final int leafMergeThresh;

    public TextOps(int branching, int leafWidth) {
      splitThresh = branching;
      leafMergeThresh = leafWidth / 2;
      leafSplitThresh = leafWidth;
    }

    @Override
    public TextMetrics calculateMetrics(String data) {
      return metricsTo(data, OffsetKind.Characters, data.length());
    }

    @Override
    public TextMetrics emptyMetrics() {
      return TextMetrics.EMPTY;
    }

    @Override
    public TextMetrics rf(TextMetrics o1, TextMetrics o2) {
      return o1.add(o2);
    }

    //@Override
    //public Object scanRf(Object o1, Object o2) {
    //  TextMetrics m1 = (TextMetrics) o1;
    //  TextMetrics m2 = (TextMetrics) o2;
    //
    //  long length = m1.length + m2.length;
    //  long geomLength = m1.geometricLength + m2.geometricLength;
    //  long linesCount = m1.linesCount + m2.linesCount;
    //  long charsCount = m1.charsCount + m2.charsCount;
    //
    //  return new TextMetrics(length,
    //                         geomLength,
    //                         linesCount,
    //                         charsCount,
    //                         0,
    //                         0,
    //                         0);
    //}

    @Override
    public boolean isLeafOverflown(String leafData) {
      // TODO we may benefit from calculated metrics here
      return leafSplitThresh < leafData.codePointCount(0, leafData.length());
    }

    @Override
    public boolean isLeafUnderflown(String leafData) {
      return leafData.codePointCount(0, leafData.length()) < leafMergeThresh;
    }

    @Override
    public String mergeLeaves(String leafData1, String leafData2) {
      return leafData1.concat(leafData2);
    }

    private static int splitString(String s, int charFrom, ArrayList<String> result, int from, int to, int thresh) {
      int length = to - from;
      if (length <= thresh) {
        int charTo = s.offsetByCodePoints(charFrom, length);
        result.add(s.substring(charFrom, charTo));
        return charTo;
      }
      else {
        int halfLength = length / 2;
        int halfCharOffset = splitString(s, charFrom, result, from, from + halfLength, thresh);
        return splitString(s, halfCharOffset, result, from + halfLength, to, thresh);
      }
    }

    @Override
    public List<String> splitLeaf(String leafData) {
      int codePointsLength = leafData.codePointCount(0, leafData.length());
      // TODO fastpath if codepointsLength == string.length
      assert leafSplitThresh < codePointsLength;
      ArrayList<String> result = new ArrayList<>();
      splitString(leafData, 0, result, 0, codePointsLength, leafSplitThresh);
      return result;
    }

    @Override
    public int splitThreshold() {
      return splitThresh;
    }
  }

  //TODO it's off by one, isn't it?
  public static BiFunction<TextMetrics, TextMetrics, Boolean> offsetPredicate(long offset) {
    return (acc, next) -> offset <= acc.length + next.length;
  }

  public static BiFunction<TextMetrics, TextMetrics, Boolean> charOffsetPredicate(long offset) {
    return (acc, next) -> offset <= acc.charsCount + next.charsCount;
  }

  public static BiFunction<TextMetrics, TextMetrics, Boolean> byCharOffsetExclusive(int offset) {
    return (o, o2) -> o.charsCount <= offset && offset < o.charsCount + o2.charsCount;
  }

  public static long offset(Rope.Zipper<TextMetrics, String> loc) {
    if (loc.oacc != null) {
      return loc.oacc.length;
    }
    else if (loc.acc != null) {
      return loc.acc.length;
    }
    else {
      return 0;
    }
  }

  public static long nodeOffset(Rope.Zipper<TextMetrics, String> loc) {
    TextMetrics acc = loc.acc;
    return acc == null ? 0 : acc.length;
  }

  public static long charOffset(Rope.Zipper<TextMetrics, ?> loc) {
    return Rope.currentAcc(loc).charsCount;
  }

  public static long nodeCharOffset(Rope.Zipper<TextMetrics, ?> loc) {
    TextMetrics acc = loc.acc;
    return acc == null ? 0 : acc.charsCount;
  }

  @SuppressWarnings("unused")
  public static Rope.Node<TextMetrics> makeText(String s) {
    Rope.ZipperOps<TextMetrics, String> ops = TEXT_OPS;
    Rope.Leaf<TextMetrics, String> leaf = Rope.makeLeaf(s, ops);
    ArrayList<Object> children = new ArrayList<>(1);
    children.add(leaf);
    return Rope.growTree(children, ops);
  }

  public static Rope.Zipper<TextMetrics, String> zipper(Rope.Node<TextMetrics> root) {
    return Rope.Zipper.zipper(root, TEXT_OPS);
  }

  @SuppressWarnings("unused")
  public static Rope.Node<TextMetrics> root(Rope.Zipper<TextMetrics, ?> loc) {
    return Rope.root(loc);
  }

  public static Rope.Zipper<TextMetrics, String> scanToOffset(Rope.Zipper<TextMetrics, String> zipper, long offset) {
    if (offset < Rope.currentAcc(zipper).length)
      throw new IllegalArgumentException("Backwards scan");

    Rope.Zipper<TextMetrics, String> offsetLoc = Rope.scan(zipper, offsetPredicate(offset));
    if (offsetLoc == null){
      throw new IndexOutOfBoundsException();
    }

    if (Rope.isRoot(offsetLoc)) {
      return offsetLoc;
    }
    long o = nodeOffset(offsetLoc);
    String s = Rope.leaf(offsetLoc).data;
    offsetLoc.oacc = offsetLoc.acc.add(metricsTo(s, OffsetKind.CodePoints, offset - o));
    return offsetLoc;
  }

  public static Rope.Zipper<TextMetrics, String> scanToCharOffset(Rope.Zipper<TextMetrics, String> zipper, long offset) {
    if (offset < Rope.currentAcc(zipper).charsCount)
      throw new IllegalArgumentException("Backwards scan");

    Rope.Zipper<TextMetrics, String> offsetLoc = Rope.scan(zipper, charOffsetPredicate(offset));
    if (offsetLoc == null){
      throw new IndexOutOfBoundsException();
    }

    if (Rope.isRoot(offsetLoc)) {
      return offsetLoc;
    }
    long o = nodeCharOffset(offsetLoc);
    String s = Rope.leaf(offsetLoc).data;
    offsetLoc.oacc = offsetLoc.acc.add(metricsTo(s, OffsetKind.Characters, offset - o));
    return offsetLoc;
  }

  public static Rope.Zipper<TextMetrics, String> retain(Rope.Zipper<TextMetrics, String> loc, long l) {
    return scanToOffset(loc, offset(loc) + l);
  }

  @SuppressWarnings("unused")
  public static Rope.Zipper<TextMetrics, String> insert(Rope.Zipper<TextMetrics, String> loc, String s) {
    if (s.isEmpty()) {
      return loc;
    }

    assert loc != null;

    Rope.Zipper<TextMetrics, String> leaf;
    Rope.Zipper<TextMetrics, String> branch = null;

    // TODO i know that the only case when there is no leaf to insert is an empty tree

    Rope.Zipper<TextMetrics, String> i = loc;
    while (i != null && Rope.isBranch(i)) {
      branch = i;
      i = Rope.downLeft(i);
    }
    leaf = i;

    if (leaf == null) {
      ArrayList<Object> children = new ArrayList<>();
      children.add(Rope.makeLeaf(s, branch.ops));
      Rope.Zipper<TextMetrics, String> newRoot = Rope.replace(branch, Rope.makeNode(children, branch.ops));
      return retain(newRoot, s.codePointCount(0, s.length()));
    }
    else {
      int relCharOffset = (int)(charOffset(leaf) - nodeCharOffset(leaf));
      ZipperOps<TextMetrics, String> ops = leaf.ops;
      String data = Rope.leaf(leaf).data;
      return retain(Rope.replace(leaf, Rope.makeLeaf(data.substring(0, relCharOffset)
                                                       .concat(s)
                                                       .concat(data.substring(relCharOffset)),
                                                     ops)),
                    s.codePointCount(0, s.length()));
    }
  }

  @SuppressWarnings("unused")
  public static Rope.Zipper<TextMetrics, String> delete(Rope.Zipper<TextMetrics, String> loc, int l) {

    while (l > 0) {
      //TODO optimize here (we can delete entire subtree if it's inside of deletion range
      while (Rope.isBranch(loc)) {
        loc = Rope.downLeft(loc);
        assert loc != null;
      }

      long i = offset(loc);
      String s = Rope.leaf(loc).data;
      int relOffset = (int)(i - nodeOffset(loc));
      int chunkLength = s.codePointCount(0, s.length());
      int end = Math.min(chunkLength, relOffset + l);
      ZipperOps<TextMetrics, String> ops = loc.ops;
      int deleted = end - relOffset;
      l -= deleted;
      if (relOffset == 0 && end == chunkLength) {
        loc = Rope.remove(loc);
      }
      else {
        String news = s.substring(0, s.offsetByCodePoints(0, relOffset))
          .concat(s.substring(s.offsetByCodePoints(0, end)));
        Rope.Zipper<TextMetrics, String> newLeaf = Rope.replace(loc, Rope.makeLeaf(news, ops));

        if (end == chunkLength) {
          if (l == 0) {
            return newLeaf;
          }
          else {
            loc = Rope.next(newLeaf);
          }
        }
        else {
          loc = newLeaf;
        }
      }
    }
    return loc;
  }

  public static String text(Rope.Zipper<TextMetrics, String> loc, int length) {
    if (loc == null) {
      throw new IllegalArgumentException();
    }
    if (length == 0) {
      return "";
    }

    StringBuilder sb = new StringBuilder();
    while (true) {
      assert loc != null;
      if (Rope.isBranch(loc)) {
        loc = Rope.downLeft(loc);
      }
      else {
        long i = offset(loc);
        Rope.Leaf<TextMetrics, String> leaf = Rope.leaf(loc);
        String chunk = leaf.data;
        long chunkOffset = nodeOffset(loc);
        int start = (int)(i - chunkOffset);
        int end = (int)Math.min(leaf.metrics.length, start + length);
        int charsStart = chunk.offsetByCodePoints(0, start);
        int charsEnd = chunk.offsetByCodePoints(0, end);

        sb.append(chunk, charsStart, charsEnd);
        length -= (end - start);
        if (length > 0) {
          loc = Rope.next(loc);
        }
        else {
          return sb.toString();
        }
      }
    }
  }

  public static class Sequence implements CharSequence {

    Rope.Node<TextMetrics> root;
    Rope.Zipper<TextMetrics, String> zipper;
    final int from, to; // in chars

    Sequence(Rope.Node<TextMetrics> root, int from, int to) {
      if (from < 0 || to > root.metrics.charsCount)
        throw new IllegalArgumentException();

      this.root = root;
      this.from = from;
      this.to = to;
      Rope.Zipper<TextMetrics, String> z = Rope.toTransient(zipper(root));
      this.zipper = Rope.scan(z, byCharOffsetExclusive(from));
    }

    public Sequence(Rope.Node<TextMetrics> root) {
      this(root, 0, (int)(root.metrics).charsCount);
    }

    @Override
    public int length() {
      return to - from;
    }

    @Override
    public char charAt(int index) {
      if (index > to - from || index < 0)
        throw new IndexOutOfBoundsException("index:" + index + ", from:" + from + ", to:" + to);

      int absoluteCharOffset = from + index;
      int nodeCharOffset = (int)nodeCharOffset(zipper);
      String currentChunk = Rope.leaf(zipper).data;
      
      if (absoluteCharOffset < nodeCharOffset) {
        Rope.Zipper<TextMetrics, String> rootLoc = Rope.toTransient(zipper(root));
        Rope.Zipper<TextMetrics, String> offsetZipper = Rope.scan(rootLoc, byCharOffsetExclusive(absoluteCharOffset));
        assert offsetZipper != null;
        this.zipper = offsetZipper;
        String newChunk = Rope.leaf(offsetZipper).data;
        return newChunk.charAt(absoluteCharOffset - (int) nodeCharOffset(offsetZipper));
      }
      else if (absoluteCharOffset < nodeCharOffset + currentChunk.length()) {
        return currentChunk.charAt(absoluteCharOffset - nodeCharOffset);
      }
      else {
        Rope.Zipper<TextMetrics, String> offsetLoc = Rope.scan(zipper, byCharOffsetExclusive(absoluteCharOffset));
        assert offsetLoc != null;
        this.zipper = offsetLoc;
        String newChunk = Rope.leaf(offsetLoc).data;
        return newChunk.charAt(absoluteCharOffset - (int)nodeCharOffset(offsetLoc));
      }
    }

    @Override
    public CharSequence subSequence(int start, int end) {
      int length = to - from;
      if (start > length || end > length)
        throw new IndexOutOfBoundsException();
      if (start > end)
        throw new IllegalArgumentException();

      return new Sequence(root, from + start, from + end);
    }

    @Override
    public String toString() {
      //TODO  Index out of bounds : to = length, no such offset
      Rope.Zipper<TextMetrics, String> fromLoc = scanToCharOffset(zipper(root), from);
      Rope.Zipper<TextMetrics, String> toLoc = scanToCharOffset(fromLoc, to);
      return text(fromLoc, (int)(offset(toLoc) - offset(fromLoc)));
    }
  }

  public static long length(Rope.Node text) {
    return ((TextMetrics)text.metrics).length;
  }
}
