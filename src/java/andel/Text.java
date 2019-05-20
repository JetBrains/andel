package andel;

import andel.Rope.ZipperOps;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;


public class Text {

  public static class TextMetrics {

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

    public TextMetrics merge(TextMetrics other) {
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

  public static class TextOps implements ZipperOps {

    final int splitThresh;
    final int leafSplitThresh;
    final int leafMergeThresh;

    public TextOps(int branching, int leafWidth) {
      splitThresh = branching;
      leafMergeThresh = leafWidth / 2;
      leafSplitThresh = leafWidth;
    }

    @Override
    public Object calculateMetrics(Object data) {
      String s = (String)data;
      return metricsTo(s, OffsetKind.Characters, s.length());
    }

    @Override
    public Object emptyMetrics() {
      return TextMetrics.EMPTY;
    }

    @Override
    public Object rf(Object o1, Object o2) {
      return ((TextMetrics)o1).merge((TextMetrics)o2);
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
    public boolean isLeafOverflown(Object leafData) {
      String s = (String)leafData;
      return leafSplitThresh <= s.codePointCount(0, s.length());
    }

    @Override
    public boolean isLeafUnderflown(Object leafData) {
      String s = (String)leafData;
      return s.codePointCount(0, s.length()) < leafMergeThresh;
    }

    @Override
    public Object mergeLeaves(Object leafData1, Object leafData2) {
      return ((String)leafData1).concat((String)leafData2);
    }

    private static void splitString(String s, ArrayList<Object> result, int from, int to, int thresh) {
      int length = to - from;
      if (thresh <= length) {
        int halfLength = length / 2;
        splitString(s, result, from, from + halfLength, thresh);
        splitString(s, result, from + halfLength, to, thresh);
      }
      else {
        result.add(s.substring(s.offsetByCodePoints(0, from), s.offsetByCodePoints(0, to)));
      }
    }

    @Override
    public List<Object> splitLeaf(Object leafData) {
      String s = (String)leafData;
      assert leafSplitThresh <= s.codePointCount(0, s.length());
      ArrayList<Object> result = new ArrayList<>();
      splitString(s, result, 0, s.length(), leafSplitThresh);
      return result;
    }

    @Override
    public int splitThreshold() {
      return splitThresh;
    }
  }

  public static BiFunction<Object, Object, Boolean> offsetPredicate(long offset) {
    return (acc, next) -> {
      TextMetrics m1 = (TextMetrics)acc;
      TextMetrics m2 = (TextMetrics)next;
      return offset <= m1.merge(m2).length;
    };
  }

  public static long offset(Rope.Zipper loc) {
    if (loc.isEnd) {
      return ((TextMetrics)Rope.getMetrics(Rope.currentNode(loc))).length;
    }
    else if (loc.oacc != null) {
      return ((TextMetrics)loc.oacc).length;
    }
    else if (loc.acc != null) {
      return ((TextMetrics)loc.acc).length;
    }
    else {
      return 0;
    }
  }

  public static long nodeOffset(Rope.Zipper loc) {
    TextMetrics acc = (TextMetrics)loc.acc;
    return acc == null ? 0 : acc.length;
  }

  public static long charOffset(Rope.Zipper loc) {
    TextMetrics acc = (TextMetrics)Rope.currentAcc(loc);
    return acc == null ? 0 : acc.charsCount;
  }

  public static long nodeCharOffset(Rope.Zipper loc) {
    TextMetrics acc = (TextMetrics)loc.acc;
    return acc == null ? 0 : acc.charsCount;
  }

  public static Rope.Node makeText(String s) {
    Rope.ZipperOps ops = TEXT_OPS;
    return Rope.growTree(Rope.wrapNode(Rope.makeLeaf(s, ops), ops), ops);
  }

  public static Rope.Zipper zipper(Rope.Node root) {
    return Rope.Zipper.zipper(root, TEXT_OPS);
  }

  public static Rope.Node root(Rope.Zipper loc) {
    Rope.Node r = (Rope.Node)Rope.root(loc);
    if (r.children.isEmpty()) {
      return makeText("");
    }
    else {
      return r;
    }
  }

  public static Rope.Zipper scanToOffset(Rope.Zipper loc, long offset) {
    Rope.Zipper offsetLoc = Rope.scan(loc, offsetPredicate(offset));
    if (offsetLoc.isEnd) {
      return offsetLoc;
    }
    else {
      long o = nodeOffset(offsetLoc);
      String s = (String)((Rope.Leaf)Rope.currentNode(offsetLoc)).data;
      TextMetrics acc = (TextMetrics)offsetLoc.acc;
      offsetLoc.oacc = acc.merge(metricsTo(s, OffsetKind.CodePoints, offset - o));
      return offsetLoc;
    }
  }

  public static Rope.Zipper retain(Rope.Zipper loc, long l) {
    return scanToOffset(loc, offset(loc) + l);
  }

  public static Rope.Zipper insert(Rope.Zipper loc, String s) {
    if (s.isEmpty()) {
      return loc;
    }

    assert loc != null;
    assert !loc.isEnd;

    Rope.Zipper leaf;
    Rope.Zipper branch = null;

    Rope.Zipper i = loc;
    while (i != null && Rope.isBranch(i)) {
      branch = i;
      i = Rope.downForward(i);
    }
    leaf = i;

    if (leaf == null) {
      ArrayList<Object> children = new ArrayList<>();
      children.add(Rope.makeLeaf(s, branch.ops));
      Rope.Zipper newRoot = Rope.replace(branch, Rope.makeNode(children, branch.ops));
      newRoot.isEnd = false;
      return retain(newRoot,
                    s.codePointCount(0, s.length()));
    } else {
      int relCharOffset = (int)(charOffset(leaf) - nodeCharOffset(leaf));
      ZipperOps ops = leaf.ops;
      String data = (String)((Rope.Leaf)(Rope.currentNode(leaf))).data;
      return retain(Rope.replace(leaf, Rope.makeLeaf(data.substring(0, relCharOffset)
                                         .concat(s)
                                         .concat(data.substring(relCharOffset)),
                                       ops)),
                    s.codePointCount(0, s.length()));
    }
  }

  public static Rope.Zipper delete(Rope.Zipper loc, int l) {
    while (l > 0) {
      if (loc.isEnd){
        throw new IndexOutOfBoundsException();
      }
      //TODO optimize here (we can delete entire subtree if it's inside of deletion range
      while (Rope.isBranch(loc)) {
        loc = Rope.downForward(loc);
      }

      assert loc != null;

      long i = offset(loc);
      String s = (String)((Rope.Leaf)Rope.currentNode(loc)).data;
      int relOffset = (int)(i - nodeOffset(loc));
      int chunkLength = s.codePointCount(0, s.length());
      int end = Math.min(chunkLength, relOffset + l);
      ZipperOps ops = loc.ops;
      int deleted = end - relOffset;
      l -= deleted;
      if (relOffset == 0 && end == chunkLength) {
        loc = Rope.remove(loc);
      }
      else {
        Rope.Zipper newLeaf = Rope.edit(loc, (o) -> {
          String data = (String)((Rope.Leaf)o).data;
          return Rope.makeLeaf(data.substring(0, s.offsetByCodePoints(0, relOffset))
                                 .concat(data.substring(s.offsetByCodePoints(0, end))),
                               ops);
        });
        if (end == chunkLength) {
          Rope.Zipper next = Rope.next(newLeaf);
          if (next.isEnd) {
            if (l > 0) {
              throw new IndexOutOfBoundsException();
            } else {
              return newLeaf;
            }
          } else {
            loc = next;
          }
        }
        else {
          loc = newLeaf;
        }
      }
    }
    return loc;
  }

  public static String text(Rope.Zipper loc, int length) {
    StringBuilder sb = new StringBuilder();
    while (length > 0) {
      assert loc != null;
      if (loc.isEnd) {
        throw new IllegalArgumentException("Length is out of bounds, length: " + length);
      }
      if (Rope.isBranch(loc)) {
        loc = Rope.downForward(loc);
      }
      else {
        long i = offset(loc);
        Rope.Leaf leaf = (Rope.Leaf)Rope.currentNode(loc);
        String chunk = (String)leaf.data;
        long chunkOffset = nodeOffset(loc);
        int start = (int)(i - chunkOffset);
        int codepointsCount = (int)((TextMetrics)leaf.metrics).length;
        int end = Math.min(codepointsCount, start + length);
        int charsStart = chunk.offsetByCodePoints(0, start);
        int charsEnd = chunk.offsetByCodePoints(0, end);

        sb.append(chunk, charsStart, charsEnd);
        length -= (end - start);
        loc = Rope.next(loc);
      }
    }
    return sb.toString();
  }
}