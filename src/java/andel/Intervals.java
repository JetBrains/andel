package andel;

import java.util.*;

import io.lacuna.bifurcan.IntMap;


public class Intervals<T> {

  private final static long OPEN_ROOT_ID = -1;
  private final static long CLOSED_ROOT_ID = -2;
  private final static long FIRST_ID = -3;

  /*
   * we employ two separate trees to store markers with greedy and not greedy start
   * this is necessary because we store intervals sorted by start offset
   * this order is hard to preserve in situations of open/closed intervals starting on the same offset
   * e.g. (0, 5) [1 3) -> collapse(0, 2) -> (0, 3) [0, 1] -> expand(0, 1) -> (1, 4) [0, 2] !!!
   * */

  public final Node openRoot;
  public final Node closedRoot;
  public final int maxChildren;
  public final IntMap<Long> parentsMap;
  private final long nextInnerId;

  public Intervals(int maxChildren, Node openRoot, Node closedRoot, IntMap<Long> parentsMap, long nextId) {
    this.maxChildren = maxChildren;
    this.openRoot = openRoot;
    this.closedRoot = closedRoot;
    this.nextInnerId = nextId;
    this.parentsMap = parentsMap;
  }

  public Intervals(int maxChildren) {
    this(maxChildren, Node.empty(maxChildren / 2), Node.empty(maxChildren / 2), new IntMap<>(), FIRST_ID);
  }

  public static class Node {
    public LongArrayList ids;
    public LongArrayList starts;
    public LongArrayList ends;
    public ArrayList<Object> children;

    public Node(LongArrayList ids, LongArrayList starts, LongArrayList ends, ArrayList<Object> children) {
      this.ids = ids;
      this.starts = starts;
      this.ends = ends;
      this.children = children;
    }

    public static Node empty(int capacity) {
      return new Node(new LongArrayList(capacity),
                      new LongArrayList(capacity),
                      new LongArrayList(capacity),
                      new ArrayList<>(capacity));
    }

    public void add(long id, long start, long end, Object child) {
      this.ids.add(id);
      this.starts.add(start);
      this.ends.add(end);
      this.children.add(child);
    }

    public Node copy() {
      //noinspection unchecked
      return new Node(this.ids.copy(),
                      this.starts.copy(),
                      this.ends.copy(),
                      (ArrayList<Object>)this.children.clone());
    }
  }

  public static class Interval<T> {
    public long id;
    public long from;
    public long to;
    public T data;
    public boolean closedLeft;
    public boolean closedRight;

    public Interval(long id, long from, long to, boolean closedLeft, boolean closedRight, T data) {
      this.id = id;
      this.from = from;
      this.to = to;
      this.data = data;
      this.closedLeft = closedLeft;
      this.closedRight = closedRight;
    }

    @Override
    public String toString() {
      return "id=" + id + (closedLeft ? "[" : "(") + from + ", " + to + (closedRight ? "]" : ")");
    }
  }

  static class Zipper {
    public boolean startsAreOpen;
    public boolean changed = false;
    public boolean hasRightCousin;
    public long delta = 0;
    public Zipper parent = null;
    public long rightCousinStart;
    public LongArrayList starts;
    public LongArrayList ends;
    public LongArrayList ids;
    public ArrayList<Object> children;
    public Context editingContext;
    public int idx = 0;

    private static final LongArrayList ROOT_ENDS = new LongArrayList(new long[]{Long.MAX_VALUE});
    private static final LongArrayList ROOT_STARTS = new LongArrayList(new long[]{0});
    private static final LongArrayList OPEN_ROOT_IDS = new LongArrayList(new long[]{Long.valueOf(OPEN_ROOT_ID)});
    private static final LongArrayList CLOSED_ROOT_IDS = new LongArrayList(new long[]{Long.valueOf(CLOSED_ROOT_ID)});

    public static Zipper create(Node root, Context editingContext, boolean rootIsOpen) {
      Zipper zipper = new Zipper();
      zipper.startsAreOpen = rootIsOpen;
      zipper.rightCousinStart = Long.MAX_VALUE;
      zipper.hasRightCousin = false;
      zipper.starts = ROOT_STARTS;
      zipper.ends = ROOT_ENDS;
      zipper.ids = rootIsOpen ? OPEN_ROOT_IDS : CLOSED_ROOT_IDS;
      zipper.editingContext = editingContext;
      ArrayList<Object> children = new ArrayList<>();
      children.add(root);
      zipper.children = children;
      zipper.idx = 0;
      return zipper;
    }

    public static boolean isRoot(Zipper z) {
      return z.parent == null;
    }

    public static boolean isBranch(Zipper z) {
      return z.children.get(z.idx) instanceof Node;
    }

    public static Node node(Zipper z) {
      return (Node)z.children.get(z.idx);
    }

    public static Zipper down(Zipper z) {
      if (isBranch(z)) {
        Node child = (Node)z.children.get(z.idx);
        if (child.children.size() == 0) {
          return null;
        }
        Zipper r = new Zipper();
        r.parent = z;
        r.starts = child.starts;
        r.ends = child.ends;
        r.ids = child.ids;
        r.delta = z.delta + z.starts.get(z.idx);
        r.hasRightCousin = z.hasRightCousin || z.idx < z.children.size() - 1;
        r.rightCousinStart = r.hasRightCousin
                             ? (z.idx + 1 < z.starts.size()
                                ? z.starts.get(z.idx + 1)
                                : z.rightCousinStart) - z.starts.get(z.idx)
                             : Long.MAX_VALUE;
        assert r.rightCousinStart >= 0 : "rightCousinStart:" + r.rightCousinStart;
        r.children = child.children;
        r.editingContext = z.editingContext;
        r.idx = 0;
        return r;
      }
      else {
        throw new IllegalArgumentException();
      }
    }

    static Zipper insert(Zipper z, int idx, long id, long start, long end, Object data) {
      assert z.editingContext.parentsMap.get(id, null) == null;
      z.starts.add(idx, start - z.delta);
      z.ends.add(idx, end - z.delta);
      z.ids.add(idx, id);
      z.children.add(idx, data);
      z.editingContext.parentsMap = z.editingContext.parentsMap.put(id, ((Long)z.parent.ids.get(z.parent.idx)));
      z.changed = true;
      return z;
    }

    public static Zipper replace(Zipper p, Node n, long delta) {
      p.children.set(p.idx, n);
      p.starts.set(p.idx, p.starts.get(p.idx) + delta);
      p.ends.set(p.idx, p.starts.get(p.idx) + delta + max(n.ends));
      p.changed = true;
      return p;
    }

    public static Zipper skip(Zipper z) {
      if (z.idx + 1 < z.children.size()) {
        z.idx += 1;
        return z;
      }
      else {
        return z.hasRightCousin ? skip(up(z)) : null;
      }
    }

    public static Zipper up(Zipper z) {
      if (z.changed) {
        Zipper p = z.parent;
        if (!p.changed) {
          p.ids = p.ids.copy();
          p.starts = p.starts.copy();
          p.ends = p.ends.copy();
          p.children = new ArrayList<>(p.children);
        }
        Node n = new Node(z.ids, z.starts, z.ends, z.children);
        long delta = p.parent == null ? 0 : normalize(n);
        n = balanceChildren(z.editingContext, n);
        replace(p, n, delta);
        return p;
      }
      else {
        return z.parent;
      }
    }
  }

  public static Node subNode(Node source, int from, int to) {
    ArrayList<Object> children = new ArrayList<>(to - from);
    children.addAll(source.children.subList(from, to));
    return new Node(source.ids.subList(from, to),
                    source.starts.subList(from, to),
                    source.ends.subList(from, to),
                    children);
  }

  private static void splitNode(ArrayList<Node> result, Node source, int from, int to, int thresh) {
    int length = to - from;
    if (length <= thresh) {
      result.add(subNode(source, from, to));
    }
    else {
      int half = length / 2;
      splitNode(result, source, from, from + half, thresh);
      splitNode(result, source, from + half, to, thresh);
    }
  }

  static ArrayList<Node> splitNode(Node node, int splitThreshold) {
    ArrayList<Node> result = new ArrayList<>();
    splitNode(result, node, 0, node.children.size(), splitThreshold);
    return result;
  }

  static boolean childrenNeedSplitting(Node node, int splitThreshold) {
    for (Object child : node.children) {
      if (child instanceof Node) {
        if (((Node)child).children.size() > splitThreshold) {
          return true;
        }
      }
      else {
        return false;
      }
    }
    return false;
  }

  private static IntMap<Long> adopt(IntMap<Long> parentsMap, long parentId, LongArrayList childrenIds) {
    Long id = parentId;
    for (int k = 0; k < childrenIds.size(); ++k) {
      parentsMap = parentsMap.put(childrenIds.get(k), id);
    }
    return parentsMap;
  }

  static Node splitChildren(Context ctx, Node node) {
    int splitThreshold = ctx.maxChildren;
    if (childrenNeedSplitting(node, splitThreshold)) {
      Node result = Node.empty(splitThreshold / 2);
      IntMap<Long> m = ctx.parentsMap;
      long nextId = ctx.nextId;
      for (int i = 0; i < node.children.size(); i++) {
        Node child = (Node)node.children.get(i);
        long childDelta = node.starts.get(i);
        long childId = node.ids.get(i);
        if (child.children.size() > splitThreshold) {
          ArrayList<Node> partition = splitNode(child, splitThreshold);
          for (int j = 0; j < partition.size(); j++) {
            Node p = partition.get(j);
            long delta = normalize(p);
            result.children.add(p);
            result.starts.add(delta + childDelta);
            result.ends.add(max(p.ends) + childDelta + delta);
            if (j == 0) {
              result.ids.add(childId);
            }
            else {
              long newId = nextId--;
              m = adopt(m, newId, p.ids);
              m = m.put(newId, m.get(childId, null));
              result.ids.add(newId);
            }
          }
        }
        else {
          result.add(childId, node.starts.get(i), node.ends.get(i), child);
        }
      }
      ctx.nextId = nextId;
      ctx.parentsMap = m;
      return result;
    }
    else {
      return node;
    }
  }

  private static boolean childrenNeedMerging(Node node, int threshold) {
    for (Object child : node.children) {
      if (child instanceof Node) {
        if (((Node)child).children.size() < threshold) {
          return true;
        }
      }
      else {
        return false;
      }
    }
    return false;
  }

  static Node mergeNodes(long leftDelta, Node left, long rightDelta, Node right) {
    int capacity = left.children.size() + right.children.size();
    long delta = rightDelta - leftDelta;
    Node n = Node.empty(capacity);
    int leftc = left.children.size();
    int rightc = right.children.size();
    for (int i = 0; i < leftc; i++) {
      n.children.add(left.children.get(i));
      n.starts.add(left.starts.get(i));
      n.ends.add(left.ends.get(i));
      n.ids.add(left.ids.get(i));
    }
    for (int i = 0; i < rightc; i++) {
      n.children.add(right.children.get(i));
      n.starts.add(right.starts.get(i) + delta);
      n.ends.add(right.ends.get(i) + delta);
      n.ids.add(right.ids.get(i));
    }
    return n;
  }

  static Node mergeChildren(Context ctx, Node node) {
    int splitThreshold = ctx.maxChildren;
    int mergeThreshold = splitThreshold / 2;
    if (childrenNeedMerging(node, mergeThreshold)) {
      Node result = Node.empty(mergeThreshold);
      Node left = (Node)node.children.get(0);
      long leftDelta = node.starts.get(0);
      long leftId = node.ids.get(0);
      long leftEnd = node.ends.get(0);
      for (int i = 1; i < node.children.size(); i++) {
        Node right = (Node)node.children.get(i);
        long rightDelta = node.starts.get(i);
        long rightId = node.ids.get(i);
        long rightEnd = node.ends.get(i);
        if (left.children.size() < mergeThreshold || right.children.size() < mergeThreshold) {
          Node merged = mergeChildren(ctx, mergeNodes(leftDelta, left, rightDelta, right));
          if (merged.children.size() > splitThreshold) {
            ArrayList<Node> split = splitNode(merged, splitThreshold);
            assert split.size() == 2;

            ctx.parentsMap = adopt(ctx.parentsMap, leftId, split.get(0).ids);
            result.add(leftId, leftDelta, leftDelta + max(split.get(0).ends), split.get(0));

            ctx.parentsMap = adopt(ctx.parentsMap, rightId, split.get(1).ids);
            left = split.get(1);
            leftDelta += normalize(split.get(1));
            leftEnd = leftDelta + max(split.get(1).ends);
            leftId = rightId;
          }
          else {
            left = merged;
            leftEnd = leftDelta + max(merged.ends);
            ctx.parentsMap = adopt(ctx.parentsMap, leftId, right.ids);
          }
        }
        else {
          result.add(leftId, leftDelta, leftEnd, left);
          left = right;
          leftDelta = rightDelta;
          leftId = rightId;
          leftEnd = rightEnd;
        }
      }
      result.add(leftId, leftDelta, leftEnd, left);
      return result;
    }
    else {
      return node;
    }
  }

  private static class Context {
    public long nextId;
    public int maxChildren;
    public IntMap<Long> parentsMap;

    Context(long nextId, int maxChildren, IntMap<Long> parentsMap) {
      this.nextId = nextId;
      this.maxChildren = maxChildren;
      this.parentsMap = parentsMap;
    }
  }

  static Node balanceChildren(Context ctx, Node node) {
    // TODO we can use the fact that insertions are separated from deletions
    // you don't need to assume both can happen on the same zipper
    return mergeChildren(ctx, splitChildren(ctx, node));
  }

  private static boolean intersects(long s1, long e1, long s2, long e2) {
    return s1 <= s2 ? s2 <= e1 : s1 <= e2;
  }

  static Node growTree(Context ctx, Long rootId, Node node) {
    Node balanced = balanceChildren(ctx, node);
    if (balanced.children.size() > ctx.maxChildren) {
      ArrayList<Object> newChildren = new ArrayList<>();
      newChildren.add(balanced);
      long newLevelId = ctx.nextId--;
      ctx.parentsMap = adopt(ctx.parentsMap, newLevelId, balanced.ids);
      ctx.parentsMap = ctx.parentsMap.put(newLevelId, rootId);
      Node newRoot = new Node(new LongArrayList(new long[]{newLevelId}),
                              new LongArrayList(new long[]{0}),
                              new LongArrayList(new long[]{Long.MAX_VALUE}),
                              newChildren);
      return growTree(ctx, rootId, newRoot);
    }
    else {
      return balanced;
    }
  }

  static Node shrinkTree(Context ctx, Long rootId, Node root) {
    if (root.children.size() == 1 && root.children.get(0) instanceof Node) {
      long delta = root.starts.get(0);
      Node child = (Node)root.children.get(0);
      for (int i = 0; i < child.starts.size(); i++) {
        child.starts.set(i, child.starts.get(i) + delta);
        child.ends.set(i, child.ends.get(i) + delta);
      }
      ctx.parentsMap = adopt(ctx.parentsMap, rootId, child.ids);
      return shrinkTree(ctx, rootId, child);
    }
    else {
      return root;
    }
  }

  static Node root(Zipper z) {
    while (!Zipper.isRoot(z)) {
      z = Zipper.up(z);
    }
    Long rootId = z.startsAreOpen ? Long.valueOf(OPEN_ROOT_ID) : Long.valueOf(CLOSED_ROOT_ID);
    return shrinkTree(z.editingContext, rootId, growTree(z.editingContext, rootId, Zipper.node(z)));
  }

  private static int findInsertionPoint(LongArrayList ss, long o) {
    // find nearest interval with start greater than insertion offset to preserve insertion order of markers with same start
    int i = 0;
    while (i < ss.size() && ss.get(i) <= o) {
      ++i;
    }
    return i;
  }

  static <T> Zipper insert(Zipper z, long id, long from, long to, T data) {
    int retries = 0;
    while (true) {
      //noinspection AssertWithSideEffects
      assert ++retries < 1000;
      if (from - z.delta <= z.rightCousinStart) {
        int insertIdx = findInsertionPoint(z.starts, from - z.delta);
        if (Zipper.isBranch(z)) {
          z.idx = Math.max(0, insertIdx - 1);
          Zipper down = Zipper.down(z);
          if (down == null) {
            assert Zipper.isRoot(z);
            Node newRoot = Node.empty(z.editingContext.maxChildren);
            newRoot.starts.add(from);
            newRoot.ends.add(to);
            newRoot.ids.add(id);
            newRoot.children.add(data);
            z.editingContext.parentsMap =
              z.editingContext.parentsMap.put(id, z.startsAreOpen ? Long.valueOf(OPEN_ROOT_ID) : Long.valueOf(CLOSED_ROOT_ID));
            return Zipper.replace(z, newRoot, 0);
          }
          z = down;
        }
        else {
          return Zipper.insert(z, insertIdx, id, from, to, data);
        }
      }
      else {
        z = Zipper.up(z);
      }
    }
  }

  private static long normalize(Node node) {
    if (node.starts.size() == 0){
      return 0;
    } else {
      long delta = node.starts.get(0);
      if (delta != 0) {
        for (int i = 0; i < node.starts.size(); i++) {
          node.starts.set(i, node.starts.get(i) - delta);
          node.ends.set(i, node.ends.get(i) - delta);
        }
      }
      return delta;
    }
  }

  public static class Batch<T> {
    long lastSeenFrom = Long.MIN_VALUE;
    Zipper closedZipper;
    Zipper openZipper;
    Context editingContext;

    Batch(Zipper openZipper, Zipper closedZipper, Context editingContext) {
      this.openZipper = openZipper;
      this.closedZipper = closedZipper;
      this.editingContext = editingContext;
    }

    public Batch<T> insert(long id, long from, long to, boolean closedLeft, boolean closedRight, T data) {
      if (from < lastSeenFrom) {
        throw new IllegalArgumentException("batch is not sorted");
      }
      if (from == to && !closedLeft && !closedRight) {
        throw new IllegalArgumentException();
      }

      lastSeenFrom = from;

      if (from * 2 < from || to * 2 < to) {
        throw new ArithmeticException();
      }

      from = from * 2 - (closedLeft ? 1 : 0);
      to = to * 2 + (closedRight ? 1 : 0);
      if (closedLeft) {
        this.closedZipper = Intervals.insert(this.closedZipper, id, from, to, data);
      }
      else {
        this.openZipper = Intervals.insert(this.openZipper, id, from, to, data);
      }
      return this;
    }

    public Intervals<T> finish() {
      return new Intervals<>(this.editingContext.maxChildren,
                             root(this.openZipper),
                             root(this.closedZipper),
                             editingContext.parentsMap.forked(),
                             editingContext.nextId);
    }
  }

  public static <T> Batch<T> batch(Intervals<T> tree) {
    Context ctx = new Context(tree.nextInnerId, tree.maxChildren, tree.parentsMap.linear());
    return new Batch<>(Zipper.create(tree.openRoot, ctx, true),
                       Zipper.create(tree.closedRoot, ctx, false),
                       ctx);
  }

  public static <T> Intervals<T> insert(Intervals<T> tree, List<Interval<T>> intervals) {
    Batch<T> batch = batch(tree);
    for (Interval<T> interval : intervals) {
      batch.insert(interval.id,
                   interval.from, interval.to,
                   interval.closedLeft, interval.closedRight,
                   interval.data);
    }
    return batch.finish();
  }

  public interface IntervalsIterator<T> {
    boolean closedLeft();

    boolean closedRight();

    long from();

    long to();

    long id();

    T data();

    boolean next();
  }

  static class Iterator<T> implements IntervalsIterator<T> {
    private Zipper z;
    private final long queryFrom;
    private final long queryTo;

    Iterator(Zipper z, long queryFrom, long queryTo) {
      this.z = z;
      this.queryFrom = queryFrom;
      this.queryTo = queryTo;
    }

    @Override
    public boolean closedLeft() {
      return (z.delta + z.starts.get(z.idx)) % 2 != 0;
    }

    @Override
    public boolean closedRight() {
      return (z.delta + z.ends.get(z.idx)) % 2 != 0;
    }

    @Override
    public long from() {
      long from = z.delta + z.starts.get(z.idx);
      return from / 2 + Math.max(0, from % 2);
    }

    @Override
    public long to() {
      return (z.delta + z.ends.get(z.idx)) / 2;
    }

    @Override
    public long id() {
      return z.ids.get(z.idx);
    }

    @Override
    public T data() {
      //noinspection unchecked
      return (T)z.children.get(z.idx);
    }

    @Override
    public boolean next() {
      Zipper next = Zipper.isRoot(z) ? z : Zipper.skip(z);
      z = next == null ? null : findNextIntersection(next, queryFrom, queryTo);
      return z != null;
    }
  }

  public static class MergingIterator<T> implements IntervalsIterator<T> {

    private IntervalsIterator<T> first;
    private IntervalsIterator<T> second;
    private boolean myFirstTime = true;

    public MergingIterator(IntervalsIterator<T> left, IntervalsIterator<T> right) {
      this.first = left;
      this.second = right;
    }

    @Override
    public boolean closedLeft() {
      return first.closedLeft();
    }

    @Override
    public boolean closedRight() {
      return first.closedRight();
    }

    @Override
    public long from() {
      return first.from();
    }

    @Override
    public long to() {
      return first.to();
    }

    @Override
    public long id() {
      return first.id();
    }

    @Override
    public T data() {
      return first.data();
    }

    @Override
    public boolean next() {
      if (myFirstTime) {
        myFirstTime = false;
        boolean firstNext = first.next();
        boolean secondNext = second.next();
        if (firstNext && secondNext) {
          if (first.from() > second.from()) {
            IntervalsIterator<T> tmp = second;
            second = first;
            first = tmp;
          }
          return true;
        }
        else if (firstNext) {
          second = null;
          return true;
        }
        else if (secondNext) {
          first = second;
          second = null;
          return true;
        }
        else {
          return false;
        }
      }
      else {
        if (first.next()) {
          if (second != null) {
            if (first.from() > second.from()) {
              IntervalsIterator<T> tmp = second;
              second = first;
              first = tmp;
            }
          }
          return true;
        }
        else {
          first = second;
          second = null;
          return first != null;
        }
      }
    }
  }

  static Zipper findNextIntersection(Zipper zipper, long from, long to) {
    for (int i = zipper.idx; i < zipper.starts.size(); ++i) {
      if (intersects(from, to, zipper.starts.get(i) + zipper.delta, zipper.ends.get(i) + zipper.delta)) {
        zipper.idx = i;
        if (Zipper.isBranch(zipper)) {
          Zipper down = Zipper.down(zipper);
          return down == null ? null : findNextIntersection(down, from, to);
        }
        else {
          return zipper;
        }
      }
    }
    Zipper up = Zipper.up(zipper);
    if (up == null) {
      return null;
    }

    Zipper skip = Zipper.skip(up);
    return skip == null ? null : findNextIntersection(skip, from, to);
  }

  public static IntervalsIterator query(Intervals tree, long from, long to) {
    return new MergingIterator<>(
      new Iterator<>(Zipper.create(tree.openRoot, null, true), from * 2 - 1, to * 2),
      new Iterator<>(Zipper.create(tree.closedRoot, null, false), from * 2 - 1, to * 2));
  }

  public static List<Interval> queryAll(Intervals tree, long from, long to) {
    ArrayList<Interval> result = new ArrayList<>();
    IntervalsIterator it = query(tree, from, to);
    while (it.next()) {
      result.add(new Interval<>(it.id(), it.from(), it.to(), it.closedLeft(), it.closedRight(), it.data()));
    }
    return result;
  }

  static Node expand(Node node, long offset, long len) {
    Node result = Node.empty(node.children.size());
    for (int i = 0; i < node.children.size(); i++) {
      long id = node.ids.get(i);
      long start = node.starts.get(i);
      long end = node.ends.get(i);
      Object child = node.children.get(i);
      if (end - start == 1 && (end == offset || start == offset)) {
        // special case to handle markers with zero length and (closedLeft or closedRight)
        // examples:
        // (1, 1] stored as (2, 3) and (offset*2)=2
        // [1, 1) stored as (1, 2) and (offset*2)=2
        Object c = child instanceof Node
                   ? expand((Node)child, offset - start, len)
                   : child;
        result.add(id, start, end + len, c);
      }
      else if (end <= offset) {
        //....(interval)....
        //...............o....
        result.add(id, start, end, child);
      }
      else if (start < offset) {
        //....(interval)....
        //........o.........
        Object c = child instanceof Node
                   ? expand((Node)child, offset - start, len)
                   : child;
        result.add(id, start, end + len, c);
      }
      else {
        //......(interval)....
        //....o...............
        result.add(id, start + len, end + len, child);
      }
    }
    return result;
  }

  public static <T> Intervals<T> expand(Intervals<T> tree, long start, long len) {
    if (len == 0) {
      return tree;
    }
    return new Intervals<>(tree.maxChildren,
                           expand(tree.openRoot, start * 2, len * 2),
                           expand(tree.closedRoot, start * 2, len * 2),
                           tree.parentsMap,
                           tree.nextInnerId);
  }

  private static IntMap<Long> extinct(IntMap<Long> parentsMap, long id, Object child) {
    parentsMap = parentsMap.remove(id);
    if (child instanceof Node) {
      Node node = (Node)child;
      for (int i = 0; i < node.ids.size(); ++i) {
        parentsMap = extinct(parentsMap, node.ids.get(i), node.children.get(i));
      }
    }
    return parentsMap;
  }

  static Node collapse(Context ctx, long d, Node node, long offset, long len) {
    Node result = Node.empty(node.children.size());
    for (int i = 0; i < node.children.size(); i++) {
      long start = node.starts.get(i);
      long end = node.ends.get(i);
      Object child = node.children.get(i);
      long id = node.ids.get(i);
      if (end - start == 1 && (start + 1 == offset + len || end - 1 == offset)) {
        // special case to distinguish markers of zero length closed on one side
        // and save them from removing
        // (1, 1] stored as (2, 3) delete (1, 2) which means (2, 4)
        // [2, 2) stored as (3, 4) delete (1, 2) which means (2, 4)
        // ==> will be removed without this case
        if (offset < start){
          result.add(id, start - len, end - len, child);
        } else {
          result.add(id, start, end, child);
        }
      }
      else if (end <= offset) {
        // (interval)..............
        // .........[deletion]
        // interval is not affected by deletion range
        result.add(id, start, end, child);
      }
      else if (offset + len <= start) {
        //.............(interval)....
        //....[deletion].............
        // interval will move left
        result.add(id, start - len, end - len, child);
      }
      else if (offset <= start && end <= offset + len) {
        //........(interval)........
        //....[....deletion....]....
        // entire interval will be deleted
        // just drop it on the floor
        ctx.parentsMap = extinct(ctx.parentsMap, id, child);
      }
      else {
        //....(....interval....)....
        //......[deletion]..........
        // or
        //........(interval)....
        //....[deletion]........
        // or
        //....(....interval....).....
        //................[deletion].

        if (child instanceof Node) {
          Node c = (Node)child;
          c = collapse(ctx, d + start, c, offset - start, len);
          long delta = normalize(c);
          long newStart = start + delta;
          result.add(id, newStart, newStart + max(c.ends), c);
        }
        else {
          long newStart = offset < start
                          ? Math.max(offset - (d + start) % 2, start - len)
                          : start;
          long newEnd = Math.max(offset + (d + end) % 2, end - len);
          result.add(id, newStart, newEnd, child);
        }
      }
    }
    return balanceChildren(ctx, result);
  }

  public static <T> Intervals<T> collapse(Intervals<T> tree, long start, long len) {
    if (len == 0) {
      return tree;
    }
    Context ctx = new Context(tree.nextInnerId, tree.maxChildren, tree.parentsMap.linear());
    Node openRoot = shrinkTree(ctx, Long.valueOf(OPEN_ROOT_ID),
                               growTree(ctx, Long.valueOf(OPEN_ROOT_ID),
                                        collapse(ctx, 0, tree.openRoot, start * 2, len * 2)));
    Node closedRoot = shrinkTree(ctx, Long.valueOf(CLOSED_ROOT_ID),
                                 growTree(ctx, Long.valueOf(CLOSED_ROOT_ID),
                                          collapse(ctx, 0, tree.closedRoot, start * 2, len * 2)));
    return new Intervals<>(tree.maxChildren, openRoot, closedRoot, ctx.parentsMap.forked(), ctx.nextId);
  }

  private static Node remove(Context ctx, Node node, long nodeId, HashMap<Long, LongArrayList> subtree) {
    LongArrayList victims = subtree.get(nodeId);
    if (victims == null) {
      return node;
    }
    Node copy = node.copy();
    for (int i = 0; i < victims.size(); i++) {
      long vid = victims.get(i);
      int vidx = copy.ids.indexOf(vid);
      // victims list might contain duplicates
      if (vidx >= 0) {
        Object victim = copy.children.get(vidx);
        if (victim instanceof Node) {
          Node newNode = remove(ctx, ((Node)victim), vid, subtree);
          long delta = normalize(newNode);
          copy.children.set(vidx, newNode);
          long start = copy.starts.get(vidx) + delta;
          copy.starts.set(vidx, start);
          copy.ends.set(vidx, max(newNode.ends) + start);
        }
        else {
          copy.children.remove(vidx);
          copy.starts.remove(vidx);
          copy.ends.remove(vidx);
          copy.ids.remove(vidx);
          ctx.parentsMap = ctx.parentsMap.remove(vid);
        }
      }
    }
    return balanceChildren(ctx, copy);
  }

  public static <T> Intervals<T> remove(Intervals<T> tree, Iterable<Long> ids) {
    HashMap<Long, LongArrayList> deletionSubtree = deletionSubtree(tree.parentsMap, ids);
    Context ctx = new Context(tree.nextInnerId, tree.maxChildren, tree.parentsMap.linear());

    Node openRoot = shrinkTree(ctx, OPEN_ROOT_ID, remove(ctx, tree.openRoot, OPEN_ROOT_ID, deletionSubtree));
    Node closedRoot = shrinkTree(ctx, CLOSED_ROOT_ID, remove(ctx, tree.closedRoot, CLOSED_ROOT_ID, deletionSubtree));

    return new Intervals<>(tree.maxChildren,
                           openRoot,
                           closedRoot,
                           ctx.parentsMap.forked(),
                           ctx.nextId);
  }

  static LongArrayList resolvePath(IntMap<Long> parents, long id) {
    LongArrayList path = new LongArrayList(4);
    Long p = id;
    while (true) {
      p = parents.get((long)p, null);
      if (p == null) {
        throw new NoSuchElementException();
      }
      path.add(p);
      if (p == OPEN_ROOT_ID || p == CLOSED_ROOT_ID) {
        return path;
      }
    }
  }

  static Node chooseRoot(Intervals tree, long id) {
    if (id == OPEN_ROOT_ID) {
      return tree.openRoot;
    }
    else if (id == CLOSED_ROOT_ID) {
      return tree.closedRoot;
    }
    else {
      throw new IllegalArgumentException("given id:" + id);
    }
  }

  public static <T> Interval<T> getById(Intervals<T> tree, long id) {
    LongArrayList path = resolvePath(tree.parentsMap, id);
    Node n = chooseRoot(tree, path.get(path.size() - 1));
    int delta = 0;
    for (int i = path.size() - 2; i >= 0; --i) {
      int idx = n.ids.indexOf(path.get(i));
      if (idx == -1) {
        throw new IllegalStateException();
      }
      else {
        delta += n.starts.get(idx);
        n = (Node)n.children.get(idx);
      }
    }
    int idx = n.ids.indexOf(id);
    if (idx == -1) {
      throw new IllegalStateException();
    }
    else {
      long from = n.starts.get(idx) + delta;
      long to = n.ends.get(idx) + delta;
      //noinspection unchecked
      return new Interval<>(n.ids.get(idx),
                            from / 2 + Math.max(0, from % 2),
                            to / 2,
                            from % 2 != 0,
                            to % 2 != 0,
                            (T)n.children.get(idx));
    }
  }

  private static HashMap<Long, LongArrayList> deletionSubtree(IntMap<Long> parents, Iterable<Long> toBeDeleted) {
    HashMap<Long, LongArrayList> subtree = new HashMap<>();

    for (Long id : toBeDeleted) {
      if (id < 0) {
        throw new IllegalArgumentException("id:" + id);
      }
      long cid = id;
      if (parents.get(cid, null) != null) {
        while (cid != OPEN_ROOT_ID && cid != CLOSED_ROOT_ID) {
          Long pid = parents.get(cid, null);
          if (pid == null) {
            throw new NoSuchElementException("id:" + cid);
          }

          LongArrayList sibs = subtree.getOrDefault(pid, new LongArrayList());
          sibs.add(cid);
          subtree.put(pid, sibs);

          cid = pid;
        }
      }
    }

    return subtree;
  }

  private static long max(LongArrayList arr) {
    long m = Long.MIN_VALUE;
    for (int i = 0; i < arr.size(); ++i) {
      long l = arr.get(i);
      if (l > m) {
        m = l;
      }
    }
    return m;
  }

  public static class LongArrayList {
    private long[] buffer;
    private int size;

    public LongArrayList() {
      this(8);
    }

    public LongArrayList(int capacity) {
      buffer = new long[capacity];
      size = 0;
    }

    public LongArrayList(long[] array) {
      buffer = array;
      size = array.length;
    }

    public int size() {
      return size;
    }

    public long get(int idx) {
      return buffer[idx];
    }

    public void set(int idx, long v) {
      assert idx < size;
      buffer[idx] = v;
    }

    public void add(long v) {
      if (buffer.length == size) {
        this.buffer = Arrays.copyOf(buffer, buffer.length * 2);
      }
      this.buffer[this.size] = v;
      this.size += 1;
    }

    public void add(int idx, long v) {
      assert idx <= size;
      if (buffer.length == size) {
        long[] newBuffer = new long[size * 2];
        System.arraycopy(buffer, 0, newBuffer, 0, idx);
        System.arraycopy(buffer, idx, newBuffer, idx + 1, size - idx);
        buffer = newBuffer;
      }
      else {
        System.arraycopy(buffer, idx, buffer, idx + 1, size - idx);
      }
      this.buffer[idx] = v;
      this.size += 1;
    }

    public void removeRange(int from, int to) {
      if (to > this.size) {
        throw new IndexOutOfBoundsException();
      }
      if (to < this.size) {
        System.arraycopy(buffer, to, buffer, from, size - to);
      }
      size -= to - from;
    }

    public void remove(int idx) {
      removeRange(idx, idx + 1);
    }

    @Override
    public String toString() {
      return Arrays.toString(Arrays.copyOfRange(buffer, 0, size));
    }

    public LongArrayList copy(int capacity) {
      LongArrayList r = new LongArrayList();
      r.buffer = Arrays.copyOf(this.buffer, capacity);
      r.size = this.size;
      return r;
    }

    public LongArrayList copy() {
      return this.copy(this.size);
    }

    public int binarySearch(long key) {
      return Arrays.binarySearch(buffer, 0, size, key);
    }

    public int binarySearch(int idx, long key) {
      return Arrays.binarySearch(buffer, idx, size, key);
    }

    public LongArrayList subList(int from, int to) {
      LongArrayList r = new LongArrayList(to - from);
      System.arraycopy(this.buffer, from, r.buffer, 0, to - from);
      r.size = to - from;
      return r;
    }

    public int indexOf(long key) {
      for (int i = 0; i < this.size; i++) {
        if (this.buffer[i] == key) {
          return i;
        }
      }
      return -1;
    }
  }
}
