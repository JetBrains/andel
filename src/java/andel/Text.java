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

    while (kind == OffsetKind.CodePoints && codePointsCount <= offset ||
           kind == OffsetKind.Characters && charsCount <= offset ||
           kind == OffsetKind.Geom && geometricLength <= offset) {
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

  public static final TextOps TEXT_OPS = new TextOps();

  public static class TextOps implements ZipperOps {

    final static int splitThresh = 64;
    final static int mergeThresh = 32;

    @Override
    public Object calculateMetrics(Object data) {
      String s = (String)data;
      return metricsTo(s, OffsetKind.Characters, s.length());
    }

    @Override
    public Object emptyMetrics() {
      return new TextMetrics();
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
      return splitThresh <= s.codePointCount(0, s.length());
    }

    @Override
    public boolean isLeafUnderflown(Object leafData) {
      String s = (String)leafData;
      return s.codePointCount(0, s.length()) < mergeThresh;
    }

    @Override
    public Object mergeLeaves(Object leafData1, Object leafData2) {
      return ((String)leafData1).concat((String)leafData2);
    }

    private static void splitString(String s, ArrayList<Object> result, int from, int to, int thresh) {
      int length = to - from;
      if (length < thresh) {
        result.add(s.substring(s.offsetByCodePoints(0, from), s.offsetByCodePoints(0, to)));
      }
      else {
        int halfLength = length / 2;
        splitString(s, result, from, from + halfLength, thresh);
        splitString(s, result, from + halfLength, to, thresh);
      }
    }

    @Override
    public List<Object> splitLeaf(Object leafData) {
      String s = (String)leafData;
      assert splitThresh < s.codePointCount(0, s.length());
      ArrayList<Object> result = new ArrayList<>();
      splitString(s, result, 0, s.length(), splitThresh);
      return result;
    }

    @Override
    public int splitThreshold() {
      return splitThresh;
    }
  }

  public BiFunction<Object, Object, Boolean> offsetPredicate(long offset) {
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
    return ((TextMetrics)loc.acc).length;
  }

  public static long charOffset(Rope.Zipper loc) {
    return ((TextMetrics)Rope.currentAcc(loc)).charsCount;
  }

  public static long nodeCharOffset(Rope.Zipper loc) {
    return ((TextMetrics)loc.acc).charsCount;
  }

  public boolean atRightBorder(Rope.Zipper loc) {
    // TODO подумоть
    String s = (String)((Rope.Leaf)Rope.currentNode(loc)).data;
    long o = charOffset(loc);
    long n = nodeCharOffset(loc);
    return s.length() == (o - n);
  }

  public Rope.Node makeText(String s) {
    Rope.ZipperOps ops = TEXT_OPS;
    return Rope.growTree(Rope.wrapNode(Rope.makeLeaf(s, ops), ops), ops);
  }

  public Rope.Node  root(Rope.Zipper loc) {
    Rope.Node r = (Rope.Node)Rope.root(loc);
    if (r.children.isEmpty()) {
      return makeText("");
    }
    else {
      return r;
    }
  }

  public Rope.Zipper scanToOffset(Rope.Zipper loc, long offset) {
    Rope.Zipper offsetLoc = Rope.scan(loc, offsetPredicate(offset));
    if (offsetLoc.isEnd) {
      return offsetLoc;
    }
    else {
      long o = nodeOffset(offsetLoc);
      String s = (String)((Rope.Leaf)Rope.currentNode(offsetLoc)).data;
      TextMetrics acc = (TextMetrics)offsetLoc.acc;
      offsetLoc.oacc = acc.merge(metricsTo(s, OffsetKind.CodePoints, offset - o));
      Rope.Zipper nextLoc = Rope.nextLeaf(offsetLoc);
      if (atRightBorder(offsetLoc) && !(nextLoc.isEnd)) {
        return nextLoc;
      }
      else {
        return offsetLoc;
      }
    }
  }

  public Rope.Zipper retain(Rope.Zipper loc, long l) {
    return scanToOffset(loc, offset(loc) + l);
  }

  public Rope.Zipper insert(Rope.Zipper loc, String s) {
    while (Rope.isBranch(loc)) {
      loc = Rope.downForward(loc);
    }

    assert loc != null;
    int relCharOffset = (int)(charOffset(loc) - nodeCharOffset(loc));
    Rope.Zipper finalLoc = loc;
    return retain(Rope.edit(loc,
                            o -> {
                              String data = (String)((Rope.Leaf)o).data;
                              return Rope.makeLeaf(data.substring(0, relCharOffset)
                                                     .concat(s)
                                                     .concat(data.substring(relCharOffset)),
                                                   finalLoc.ops);
                            }),
                  s.codePointCount(0, s.length()));
  }

  public Rope.Zipper delete(Rope.Zipper loc, int l) {
    while (Rope.isBranch(loc)) {
      loc = Rope.downForward(loc);
    }

    assert loc != null;
    while (l > 0) {
      long i = offset(loc);
      String s = (String)((Rope.Leaf)Rope.currentNode(loc)).data;
      int relOffset = (int)(i - nodeOffset(loc));
      int chunkLength = s.codePointCount(0, s.length());
      int end = Math.min(chunkLength, relOffset + l);
      ZipperOps ops = loc.ops;
      loc = relOffset == 0 && end == chunkLength
            ? Rope.remove(loc)
            : scanToOffset(Rope.edit(loc, (o) -> {
              String data = (String)((Rope.Leaf)o).data;
              return Rope.makeLeaf(data.substring(0, s.codePointCount(0, relOffset))
                                     .concat(data.substring(s.codePointCount(0, end))),
                                   ops);
            }), i);
      int deleted = end - relOffset;
      l -= deleted;
    }
    return loc;
  }
}