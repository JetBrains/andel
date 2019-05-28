package andel;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

public class Markup {

  //public static final MarkupOps MARKUP_OPS = new MarkupOps(32);
  //public static final Rope.Node<MarkupMetrics> EMPTY_TREE;
  //
  //static {
  //
  //  Object leftSentinel = Rope.makeLeaf(new RelativeMarker(-1, 0, 0, false, false, null), MARKUP_OPS);
  //  Object rightSentinel = Rope.makeLeaf(new RelativeMarker(-1, Integer.MAX_VALUE, 0, false, false, null), MARKUP_OPS);
  //
  //  ArrayList<Object> sentinels = new ArrayList<>();
  //  sentinels.add(leftSentinel);
  //  sentinels.add(rightSentinel);
  //
  //  EMPTY_TREE = Rope.makeNode(sentinels, MARKUP_OPS);
  //
  //}
  //
  //public static class AbsoluteMarker {
  //  public final long id;
  //  public final long from;
  //  public final long to;
  //  public final boolean isGreedyLeft;
  //  public final boolean isGreedyRight;
  //  public final Object attrs;
  //
  //  public AbsoluteMarker(long id,
  //                        long from,
  //                        long to,
  //                        boolean isGreedyLeft,
  //                        boolean isGreedyRight,
  //                        Object attrs) {
  //    this.id = id;
  //    this.from = from;
  //    this.to = to;
  //    this.isGreedyLeft = isGreedyLeft;
  //    this.isGreedyRight = isGreedyRight;
  //    this.attrs = attrs;
  //  }
  //}
  //
  //public static class RelativeMarker {
  //  public final long id;
  //  public final long offset;
  //  public final long length;
  //  public final boolean isGreedyLeft;
  //  public final boolean isGreedyRight;
  //  public final Object attrs;
  //
  //  public RelativeMarker(long id, long offset, long length, boolean isGreedyLeft, boolean isGreedyRight, Object attrs) {
  //    this.id = id;
  //    this.offset = offset;
  //    this.length = length;
  //    this.isGreedyLeft = isGreedyLeft;
  //    this.isGreedyRight = isGreedyRight;
  //    this.attrs = attrs;
  //  }
  //
  //  public RelativeMarker copyWithOffset(long offset){
  //    return new RelativeMarker(this.id, offset, this.length, this.isGreedyLeft, this.isGreedyRight, this.attrs);
  //  }
  //}
  //
  //public final static class MarkupMetrics {
  //  public final long offset;
  //  public final long length;
  //  public final long rightest;
  //
  //  public MarkupMetrics(long offset,
  //                       long length,
  //                       long rightest) {
  //    this.offset = offset;
  //    this.length = length;
  //    this.rightest = rightest;
  //  }
  //
  //  public static final MarkupMetrics EMPTY = new MarkupMetrics(0, 0, 0);
  //
  //  public MarkupMetrics add(MarkupMetrics other) {
  //    return new MarkupMetrics(
  //      this.offset,
  //      Math.max(this.length, this.rightest + other.offset + other.length),
  //      this.rightest + other.offset + other.rightest
  //    );
  //  }
  //}
  //
  //public static class MarkupOps implements Rope.ZipperOps<MarkupMetrics, RelativeMarker> {
  //
  //  final int splitThresh;
  //
  //  public MarkupOps(int branching) {
  //    splitThresh = branching;
  //  }
  //
  //  @Override
  //  public MarkupMetrics calculateMetrics(RelativeMarker marker) {
  //    return new MarkupMetrics(marker.offset, marker.length, 0);
  //  }
  //
  //  @Override
  //  public MarkupMetrics emptyMetrics() {
  //    return MarkupMetrics.EMPTY;
  //  }
  //
  //  @Override
  //  public MarkupMetrics rf(MarkupMetrics m1, MarkupMetrics m2) {
  //    return m1.add(m2);
  //  }
  //
  //  @Override
  //  public int splitThreshold() {
  //    return splitThresh;
  //  }
  //}
  //
  //public static Rope.Zipper<MarkupMetrics, RelativeMarker> zipper(Rope.Node<MarkupMetrics> root) {
  //  return Rope.Zipper.zipper(root, MARKUP_OPS);
  //}
  //
  //public static Rope.Node<MarkupMetrics> root(Rope.Zipper<MarkupMetrics, RelativeMarker> loc) {
  //  return Rope.root(loc);
  //}
  //
  //private static long offsetToTreeBasis(long offset) {
  //  return offset + 1;
  //}
  //
  //private static long treeBasisToOffset(long offset) {
  //  return offset - 1;
  //}
  //
  //private static long absoluteFrom(Rope.Zipper<MarkupMetrics, ?> zipper) {
  //  return Rope.currentAcc(zipper).rightest + Rope.metrics(zipper).offset;
  //}
  //
  //private static long absoluteTo(Rope.Zipper<MarkupMetrics, ?> zipper) {
  //  return absoluteFrom(zipper) + Rope.metrics(zipper).length;
  //}
  //
  //public AbsoluteMarker currentMarker(Rope.Zipper<MarkupMetrics, RelativeMarker> zipper) {
  //  Rope.Leaf<MarkupMetrics, RelativeMarker> l = Rope.data(zipper);
  //  RelativeMarker m = l.data;
  //  return new AbsoluteMarker(m.id,
  //                            treeBasisToOffset(absoluteFrom(zipper)),
  //                            treeBasisToOffset(absoluteTo(zipper)),
  //                            m.isGreedyLeft,
  //                            m.isGreedyRight,
  //                            m.attrs);
  //}
  //
  //public static BiFunction<MarkupMetrics, MarkupMetrics, Boolean> byOffset(long offset) {
  //  return (o, o2) -> {
  //    // TODO: remove allocation
  //    MarkupMetrics sum = o.add(o2);
  //    return offset < sum.offset + sum.rightest;
  //  };
  //}
  //
  //private static boolean intersectsInclusively(long from1, long to1, long from2, long to2) {
  //  return from1 < from2
  //         ? from2 <= to1
  //         : from1 <= to2;
  //}
  //
  //public static BiFunction<MarkupMetrics, MarkupMetrics, Boolean> byIntersect(long from, long to) {
  //  return (acc, nodeMetrics) -> {
  //    long nodeFrom = nodeMetrics.offset + acc.rightest;
  //    long nodeTo = nodeFrom + nodeMetrics.length;
  //    return intersectsInclusively(nodeFrom, nodeTo, from, to);
  //  };
  //}
  //
  //private static Rope.Zipper<MarkupMetrics, RelativeMarker> insert(Rope.Zipper<MarkupMetrics, RelativeMarker> zipper, AbsoluteMarker marker) {
  //  assert zipper.isTransient;
  //  long from = marker.from;
  //  Rope.Zipper<MarkupMetrics,RelativeMarker> insertLocation = Rope.scan(zipper, byOffset(from));
  //  MarkupMetrics rMetrics = Rope.metrics(insertLocation);
  //  long rAbsoluteFrom = absoluteFrom(insertLocation);
  //  long lAbsoluteFrom = rAbsoluteFrom - rMetrics.offset;
  //  long offset = marker.from - lAbsoluteFrom;
  //  long newROffset = rAbsoluteFrom - marker.from;
  //
  //  RelativeMarker relative = new RelativeMarker(
  //    marker.id,
  //    offset,
  //    marker.to - marker.from,
  //    marker.isGreedyLeft,
  //    marker.isGreedyRight,
  //    marker.attrs
  //  );
  //
  //  Rope.Zipper<MarkupMetrics, RelativeMarker> zipper2 = Rope.insertLeft(insertLocation, relative);
  //  return Rope.replace(zipper2, Rope.makeLeaf(Rope.data(zipper2).data.copyWithOffset(newROffset), zipper2.ops));
  //}



}
