package andel.impl.text;

import andel.impl.text.Rope.ZipperOps;
import andel.text.TextConsumer;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

@SuppressWarnings({"WeakerAccess", "unused"})
public class TextImpl {

  public enum OffsetKind {
    CodePoints,
    Characters,
    Geom,
    Lines
  }

  public static TextMetrics metricsTo(String str, OffsetKind kind, long offset) {
    long maxLineLength = 0;
    long newlinePrefixGeomLength = 0;
    long newlineSuffixGeomLength = 0;
    long codePointsCount = 0;
    int charsCount = 0;
    long geometricLength = 0;
    long linesCount = 0;

    while (kind == OffsetKind.CodePoints && codePointsCount < offset ||
           kind == OffsetKind.Characters && charsCount < offset ||
           kind == OffsetKind.Geom && geometricLength < offset ||
           kind == OffsetKind.Lines && linesCount < offset) {
      int codepoint = str.codePointAt(charsCount);
      if (codepoint == '\n') {
        maxLineLength = Math.max(Math.max(maxLineLength, newlinePrefixGeomLength), newlineSuffixGeomLength);
        newlineSuffixGeomLength = 0;
        linesCount += 1;
      } else {
        int gl = codepoint == '\t' ? 4 : 1;
        if (linesCount == 0){
          newlinePrefixGeomLength += gl;
        }
        newlineSuffixGeomLength += gl;
        geometricLength += gl;
      }
      codePointsCount += 1;
      charsCount += Character.charCount(codepoint);
    }

    maxLineLength = Math.max(Math.max(maxLineLength, newlineSuffixGeomLength), newlinePrefixGeomLength);

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

    public final int splitThresh;
    public final int leafSplitThresh;
    final int leafMergeThresh;

    public TextOps(int branching, int leafWidth) {
      this.splitThresh = branching;
      this.leafMergeThresh = leafWidth / 2;
      this.leafSplitThresh = leafWidth;
    }

    @Override
    public TextMetrics calculateMetrics(String data) {
      return metricsTo(data, OffsetKind.Characters, data.length());
    }

    private static final TextMetrics EMPTY_TEXT_METRICS = new TextMetrics();

    @Override
    public TextMetrics emptyMetrics() {
      return EMPTY_TEXT_METRICS;
    }

    @Override
    public TextMetrics rf(TextMetrics o1, TextMetrics o2) {
      return o1.add(o2);
    }

    @Override
    public TextMetrics rf(List<TextMetrics> metrics) {
      TextMetrics acc = new TextMetrics();
      for (TextMetrics metric : metrics) {
        acc.merge(metric);
      }
      return acc;
    }

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

    private static int splitString(String s, int charFrom, ArrayList<String> result, int from, int to, int thresh, boolean fastPath) {
      int length = to - from;
      if (length <= thresh) {
        int charTo = fastPath ? to : s.offsetByCodePoints(charFrom, length);
        result.add(s.substring(charFrom, charTo));
        return charTo;
      }
      else {
        int halfLength = length / 2;
        int halfCharOffset = splitString(s, charFrom, result, from, from + halfLength, thresh, fastPath);
        return splitString(s, halfCharOffset, result, from + halfLength, to, thresh, fastPath);
      }
    }

    @Override
    public List<String> splitLeaf(String leafData) {
      int codePointsLength = leafData.codePointCount(0, leafData.length());
      assert leafSplitThresh < codePointsLength;
      ArrayList<String> result = new ArrayList<>();
      splitString(leafData, 0, result, 0, codePointsLength, leafSplitThresh, codePointsLength == leafData.length());
      return result;
    }

    @Override
    public int splitThreshold() {
      return splitThresh;
    }
  }

  /*
  * this predicate may leave you in a leaf that does not contain exactly this offset
  * it is used to scan to the end of the document
  *
  * <|> (0, 1, 2) (3, 4, 5) -> scanToOffset(3) -> (0, 1, 2 <|>) (3, 4 ,5)
  *
  * */
  public static BiFunction<TextMetrics, TextMetrics, Boolean> offsetPredicate(long offset) {
    return (acc, next) -> offset <= acc.length + next.length;
  }

  public static BiFunction<TextMetrics, TextMetrics, Boolean> geomOffsetPredicate(long offset) {
    return (acc, next) -> offset <= acc.geometricLength + next.geometricLength;
  }

  public static BiFunction<TextMetrics, TextMetrics, Boolean> charOffsetPredicate(long offset) {
    return (acc, next) -> offset <= acc.charsCount + next.charsCount;
  }

  public static BiFunction<TextMetrics, TextMetrics, Boolean> byOffsetExclusive(long offset) {
    return (o, o2) -> o.length <= offset && offset < o.length + o2.length;
  }

  public static BiFunction<TextMetrics, TextMetrics, Boolean> byCharOffsetExclusive(long offset) {
    return (o, o2) -> o.charsCount <= offset && offset < o.charsCount + o2.charsCount;
  }

  public static BiFunction<TextMetrics, TextMetrics, Boolean> linePredicate(long offset) {
    return (acc, next) -> offset <= acc.newlinesCounts + next.newlinesCounts;
  }

  public static long offset(Rope.Zipper<TextMetrics, String> zipper) {
    return Rope.currentAcc(zipper).length;
  }

  public static long geomOffset(Rope.Zipper<TextMetrics, String> zipper) {
    return Rope.currentAcc(zipper).geometricLength;
  }

  public static long nodeOffset(Rope.Zipper<TextMetrics, String> zipper) {
    TextMetrics acc = zipper.acc;
    return acc == null ? 0 : acc.length;
  }

  public static long nodeGeomOffset(Rope.Zipper<TextMetrics, String> zipper) {
    TextMetrics acc = zipper.acc;
    return acc == null ? 0 : acc.geometricLength;
  }


  public static long charOffset(Rope.Zipper<TextMetrics, ?> zipper) {
    return Rope.currentAcc(zipper).charsCount;
  }

  public static long nodeCharOffset(Rope.Zipper<TextMetrics, ?> zipper) {
    TextMetrics acc = zipper.acc;
    return acc == null ? 0 : acc.charsCount;
  }

  public static long line(Rope.Zipper<TextMetrics, ?> zipper) {
    return Rope.currentAcc(zipper).newlinesCounts;
  }

  public static Rope.Tree<TextMetrics, String> makeText(String s, Rope.ZipperOps<TextMetrics, String> ops) {
    // TODO Optimize: instead of growing tree from the root, make bunch of leaves and build the tree bottom up
    Rope.Node<TextMetrics> root = Rope.growTree(new Rope.Node<>(Rope.singletonList(s), Rope.singletonList(ops.calculateMetrics(s))), ops);
    return new Rope.Tree<>(root, ops.rf(root.metrics), ops);
  }

  public static Rope.Tree<TextMetrics, String> makeText(String s){
    return makeText(s, TEXT_OPS);
  }

  public static Rope.Zipper<TextMetrics, String> zipper(Rope.Tree<TextMetrics, String> tree) {
    return Rope.Zipper.zipper(tree, tree.ops);
  }

  public static Rope.Tree<TextMetrics, String> root(Rope.Zipper<TextMetrics, String> loc) {
    return Rope.root(loc);
  }

  private static BiFunction<TextMetrics, TextMetrics, Boolean> predicate(OffsetKind kind, long offset){
    switch (kind) {
      case CodePoints:
        return offsetPredicate(offset);
      case Characters:
        return charOffsetPredicate(offset);
      case Geom:
        return geomOffsetPredicate(offset);
      case Lines:
        return linePredicate(offset);
    }
    throw new IllegalArgumentException(kind.toString());
  }

  private static long getOffset(TextMetrics metrics, OffsetKind kind){
    switch (kind) {
      case CodePoints:
        return metrics.length;
      case Characters:
        return metrics.charsCount;
      case Geom:
        return metrics.geometricLength;
      case Lines:
        return metrics.newlinesCounts;
    }
    throw new IllegalArgumentException(kind.toString());

  }

  private static Rope.Zipper<TextMetrics, String> scan(Rope.Zipper<TextMetrics, String> zipper, long offset, OffsetKind kind){
    long currentOffset = getOffset(Rope.currentAcc(zipper), kind);
    if (offset < currentOffset)
      throw new IllegalArgumentException("Backwards scan: current is " + currentOffset + ", scanning to " + offset);

    /* leaf chunk may contain intervals where specific metric is not incremented
     * if we start chunk reduction from the beginning we may end up at the start of this interval
     * resulting in a position which is to the left from original
     */
    if (offset == currentOffset)
      return zipper;

    boolean isTransient = zipper.isTransient;
    zipper = Rope.toTransient(zipper);
    Rope.Zipper<TextMetrics, String> offsetLoc = Rope.scan(zipper, predicate(kind, offset));
    if (offsetLoc == null){
      throw new IndexOutOfBoundsException("Kind: " + kind + ", looking for offset " + offset);
    }

    if (Rope.isRoot(offsetLoc)) {
      return offsetLoc;
    }
    long o = getOffset(offsetLoc.acc, kind);
    String s = Rope.data(offsetLoc);
    offsetLoc.oacc = offsetLoc.acc.add(metricsTo(s, kind, offset - o));
    return isTransient ? offsetLoc : Rope.toPersistent(offsetLoc);
  }

  public static Rope.Zipper<TextMetrics, String> scanToOffset(Rope.Zipper<TextMetrics, String> zipper, long offset) {
    return scan(zipper, offset, OffsetKind.CodePoints);
  }

  public static Rope.Zipper<TextMetrics, String> scanToGeomOffset(Rope.Zipper<TextMetrics, String> zipper, long offset) {
    return scan(zipper, offset, OffsetKind.Geom);
  }

  public static Rope.Zipper<TextMetrics, String> scanToCharOffset(Rope.Zipper<TextMetrics, String> zipper, long offset) {
    return scan(zipper, offset, OffsetKind.Characters);
  }

  public static Rope.Zipper<TextMetrics, String> scanToLineStart(Rope.Zipper<TextMetrics, String> zipper, long offset) {
    return scan(zipper, offset, OffsetKind.Lines);
  }

  public static Rope.Zipper<TextMetrics, String> retain(Rope.Zipper<TextMetrics, String> loc, long l) {
    return scanToOffset(loc, offset(loc) + l);
  }

  public static Rope.Zipper<TextMetrics, String> insert(Rope.Zipper<TextMetrics, String> loc, String s) {
    if (s.isEmpty()) {
      return loc;
    }

    assert loc != null;

    Rope.Zipper<TextMetrics, String> leaf;
    Rope.Zipper<TextMetrics, String> branch = null;

    // TODO i know that the only case when there is no data to insert is an empty tree

    Rope.Zipper<TextMetrics, String> i = loc;
    while (i != null && Rope.isBranch(i)) {
      branch = i;
      i = Rope.downLeft(i);
    }
    leaf = i;

    if (leaf == null) {
      TextMetrics metrics = branch.ops.calculateMetrics(s);
      Rope.Zipper<TextMetrics, String> newRoot =
        Rope.replace(branch,
                     new Rope.Node<>(Rope.singletonList(s),
                                     Rope.singletonList(metrics)),
                     metrics);
      return retain(newRoot, s.codePointCount(0, s.length()));
    }
    else {
      int relCharOffset = (int)(charOffset(leaf) - nodeCharOffset(leaf));
      ZipperOps<TextMetrics, String> ops = leaf.ops;
      String data = Rope.data(leaf);
      String newData = data.substring(0, relCharOffset)
        .concat(s)
        .concat(data.substring(relCharOffset));
      return Rope.replace(leaf, newData, ops.calculateMetrics(newData));
    }
  }

  public static Rope.Zipper<TextMetrics, String> delete(Rope.Zipper<TextMetrics, String> loc, int l) {

    while (l > 0) {
      //TODO optimize here (we can delete entire subtree if it's inside of deletion range
      while (Rope.isBranch(loc)) {
        loc = Rope.downLeft(loc);
        assert loc != null;
      }

      long i = offset(loc);
      String s = Rope.data(loc);
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
        Rope.Zipper<TextMetrics, String> newLeaf = Rope.replace(loc, news, ops.calculateMetrics(news));

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

  public static Rope.Zipper<TextMetrics, String> consumeText(Rope.Zipper<TextMetrics, String> loc, int length, TextConsumer consumer) {
    if (loc == null) {
      throw new IllegalArgumentException();
    }
    if (length == 0) {
      return loc;
    }

    while (true) {
      assert loc != null;
      if (Rope.isBranch(loc)) {
        loc = Rope.downLeft(loc);
      }
      else {
        long i = offset(loc);
        String chunk = Rope.data(loc);
        long chunkOffset = nodeOffset(loc);
        int start = (int)(i - chunkOffset);
        int end = (int)Math.min(Rope.metrics(loc).length, start + length);
        int charsStart = chunk.offsetByCodePoints(0, start);
        int charsEnd = chunk.offsetByCodePoints(0, end);
        consumer.consume(chunk, charsStart, charsEnd);
        length -= (end - start);
        if (length > 0) {
          loc = Rope.next(loc);
        }
        else {
          loc = loc.copy();
          loc.oacc = loc.acc.add(metricsTo(chunk, OffsetKind.Characters, charsEnd));
          return loc;
        }
      }
    }
  }

  public static long length(Rope.Tree<TextMetrics, String> text) {
    return text.metrics.length;
  }

  public static long linesCount(Rope.Tree<TextMetrics, String> text) {
    return text.metrics.newlinesCounts + 1;
  }

  public static long charsCount(Rope.Tree<TextMetrics, String> text) {
    return text.metrics.charsCount;
  }

  public static long maxLineLength(Rope.Tree<TextMetrics, String> text) {
    return text.metrics.maxLineLength;
  }
}
