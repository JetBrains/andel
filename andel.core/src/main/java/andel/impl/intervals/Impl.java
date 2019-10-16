package andel.impl.intervals;

import java.util.*;

import andel.intervals.Interval;
import andel.intervals.Intervals;
import andel.intervals.IntervalsIterator;
import andel.impl.util.LongArrayList;
import io.lacuna.bifurcan.IntMap;


@SuppressWarnings({"WeakerAccess", "unused"})
public class Impl {

  private final static long OPEN_ROOT_ID = -1;
  private final static long CLOSED_ROOT_ID = -2;
  private final static long FIRST_ID = -3;
  public static final long MAX_VALUE = Long.MAX_VALUE / 2 - 1;

  /*
   * we employ two separate trees to store markers with greedy and not greedy start
   * this is necessary because we store intervals sorted by start offset
   * this order is hard to preserve in situations of open/closed intervals starting on the same offset
   * e.g. (0, 5) [1 3] -> collapse(0, 2) -> (0, 3) [0, 1] -> expand(0, 1) -> (1, 4) [0, 2] !!!
   * */

  public static class IntervalsImpl<T> implements Intervals<T> {
    public final Node openRoot;
    public final Node closedRoot;
    public final int maxChildren;
    public final IntMap<Long> parentsMap;
    public final long nextInnerId;

    public IntervalsImpl(int maxChildren, Node openRoot, Node closedRoot, IntMap<Long> parentsMap, long nextId) {
      this.maxChildren = maxChildren;
      this.openRoot = openRoot;
      this.closedRoot = closedRoot;
      this.nextInnerId = nextId;
      this.parentsMap = parentsMap;
    }

    @Override
    public Batch<T> batch() {
      return Impl.batch(this);
    }

    @Override
    public Interval<T> findById(long id) {
      return Impl.getById(this, id);
    }

    @Override
    public Intervals<T> removeByIds(Iterable<Long> ids) {
      return Impl.remove(this, ids);
    }

    @Override
    public IntervalsIterator<T> query(long start, long end) {
      return Impl.query(this, start, end);
    }

    @Override
    public IntervalsIterator<T> queryReverse(long start, long end) {
      return Impl.queryReverse(this, start, end);
    }

    @Override
    public Intervals<T> expand(long offset, long length) {
      return Impl.expand(this, offset, length);
    }

    @Override
    public Intervals<T> collapse(long offset, long length) {
      return Impl.collapse(this, offset, length);
    }
  }

  public static <T> IntervalsImpl<T> empty(int maxChildren) {
    Node openRoot = Node.empty(maxChildren / 2);
    Node closedRoot = Node.empty(maxChildren / 2);
    return new IntervalsImpl<>(maxChildren, openRoot, closedRoot, new IntMap<>(), FIRST_ID);
  }

  public static class Node {
    public final LongArrayList ids;
    public final LongArrayList starts;
    public final LongArrayList ends;
    public final ArrayList<Object> children;

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

  public static class Zipper {
    public long rootId;
    public boolean changed = false;
    public boolean hasRightCousin;
    public boolean hasLeftCousin;
    public long delta = 0;
    public Zipper parent = null;
    public long rightCousinStart;
    public LongArrayList starts;
    public LongArrayList ends;
    public LongArrayList ids;
    public ArrayList<Object> children;
    public EditingContext editingContext;
    public int idx = 0;

    private static final LongArrayList ROOT_ENDS = new LongArrayList(new long[]{Long.MAX_VALUE});
    private static final LongArrayList ROOT_STARTS = new LongArrayList(new long[]{0});
    private static final LongArrayList OPEN_ROOT_IDS = new LongArrayList(new long[]{OPEN_ROOT_ID});
    private static final LongArrayList CLOSED_ROOT_IDS = new LongArrayList(new long[]{CLOSED_ROOT_ID});

    public static Zipper create(Node root, EditingContext editingContext, boolean rootIsOpen) {
      Zipper zipper = new Zipper();
      zipper.rootId = rootIsOpen ? OPEN_ROOT_ID : CLOSED_ROOT_ID;
      zipper.rightCousinStart = Long.MAX_VALUE;
      zipper.hasRightCousin = false;
      zipper.hasLeftCousin = false;
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

    public static long id(Zipper z) {
      return z.ids.get(z.idx);
    }

    public static long from(Zipper z) {
      long from = z.delta + z.starts.get(z.idx);
      return from / 2 + Math.max(0, from % 2);
    }

    public static long to(Zipper z) {
      return (z.delta + z.ends.get(z.idx)) / 2;
    }

    public static <T> T data(Zipper z) {
      //noinspection unchecked
      return (T)z.children.get(z.idx);
    }

    public static boolean isRoot(Zipper z) {
      return z.parent == null;
    }

    public static boolean isBranch(Zipper z) {
      return z.children.size() > 0 && z.children.get(0) instanceof Node;
    }

    public static Node node(Zipper z) {
      return (Node)z.children.get(z.idx);
    }

    private static Zipper down(Zipper z, int idx) {
      Node child = (Node)z.children.get(z.idx);
      assert 0 <= idx && idx < child.children.size();
      Zipper r = new Zipper();
      r.parent = z;
      r.starts = child.starts;
      r.ends = child.ends;
      r.ids = child.ids;
      r.delta = z.delta + z.starts.get(z.idx);
      r.hasLeftCousin = z.hasLeftCousin || z.idx > 0;
      r.hasRightCousin = z.hasRightCousin || z.idx < z.children.size() - 1;
      r.rightCousinStart = r.hasRightCousin
                           ? (z.idx + 1 < z.starts.size()
                              ? z.starts.get(z.idx + 1)
                              : z.rightCousinStart) - z.starts.get(z.idx)
                           : Long.MAX_VALUE;
      assert r.rightCousinStart >= 0 : "rightCousinStart:" + r.rightCousinStart;
      r.children = child.children;
      r.editingContext = z.editingContext;
      r.idx = idx;
      return r;
    }

    public static Zipper downLeft(Zipper z) {
      if (isBranch(z)) {
        Node child = (Node)z.children.get(z.idx);
        if (child.children.size() == 0) {
          return null;
        }
        return down(z, 0);
      }
      else {
        throw new IllegalArgumentException();
      }
    }

    public static Zipper downRight(Zipper z) {
      if (isBranch(z)) {
        Node child = (Node)z.children.get(z.idx);
        if (child.children.size() == 0) {
          return null;
        }
        return down(z, child.children.size() - 1);
      }
      else {
        throw new IllegalArgumentException();
      }
    }

    public static Zipper replace(Zipper p, Node n, long delta) {
      if (!p.changed) {
        p.ids = p.ids.copy();
        p.starts = p.starts.copy();
        p.ends = p.ends.copy();
        p.children = new ArrayList<>(p.children);
      }
      p.children.set(p.idx, n);
      long newStart = p.starts.get(p.idx) + delta;
      p.starts.set(p.idx, newStart);
      p.ends.set(p.idx, newStart + max(n.ends));
      p.changed = true;
      return p;
    }

    public static Zipper left(Zipper z) {
      if (z.idx - 1 >= 0) {
        z.idx -= 1;
        return z;
      }
      else {
        return null;
      }
    }

    public static Zipper right(Zipper z) {
      if (z.idx + 1 < z.children.size()) {
        z.idx += 1;
        return z;
      }
      else {
        return null;
      }
    }

    public static Zipper skipRight(Zipper z) {
      Zipper right = right(z);
      if (right != null) {
        return right;
      }
      else {
        return z.hasRightCousin ? skipRight(up(z)) : null;
      }
    }

    public static Zipper skipLeft(Zipper z) {
      Zipper left = left(z);
      if (left != null) {
        return left;
      }
      else {
        return z.hasLeftCousin ? skipLeft(up(z)) : null;
      }
    }

    public static boolean hasNext(Zipper z) {
      return z.idx + 1 < z.children.size()
             || z.hasRightCousin
             || isBranch(z) && ((Node)z.children.get(z.idx)).children.size() > 0;
    }

    public static Zipper next(Zipper z) {
      assert hasNext(z);
      do {
        z = isBranch(z) ? downLeft(z) : skipRight(z);
        assert z != null;
      }
      while (isBranch(z));
      return z;
    }

    public static Zipper remove(Zipper z) {
      if (!z.changed) {
        z.ids = z.ids.copy();
        z.starts = z.starts.copy();
        z.ends = z.ends.copy();
        z.children = new ArrayList<>(z.children);
      }
      z.changed = true;
      z.ids.remove(z.idx);
      z.starts.remove(z.idx);
      z.ends.remove(z.idx);
      z.children.remove(z.idx);
      z.idx = z.idx - 1;
      return z;
    }

    public static Zipper up(Zipper z) {
      if (z.changed) {
        Zipper p = z.parent;
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

    public static Node root(Zipper z) {
      while (!Zipper.isRoot(z)) {
        z = Zipper.up(z);
      }
      return shrinkTree(z.editingContext, z.rootId, growTree(z.editingContext, z.rootId, Zipper.node(z)));
    }

    private static int findInsertionPoint(LongArrayList ss, long o) {
      // find nearest interval with start greater than insertion offset to preserve insertion order of markers with same start
      int i = 0;
      while (i < ss.size() && ss.get(i) <= o) {
        ++i;
      }
      return i;
    }

    public static <T> Zipper insert(Zipper z, long from, long to, boolean closedLeft, boolean closedRight, T data) {
      return insert(z, --z.editingContext.nextId, from, to, closedLeft, closedRight, data);
    }

    public static <T> Zipper insert(Zipper z, long id, long from, long to, boolean closedLeft, boolean closedRight, T data) {
      from = from * 2 - (closedLeft ? 1 : 0);
      to = to * 2 + (closedRight ? 1 : 0);
      int retries = 0;
      while (true) {
        //noinspection AssertWithSideEffects
        assert ++retries < 1000;
        if (from - z.delta <= z.rightCousinStart) {
          int insertIdx = findInsertionPoint(z.starts, from - z.delta);
          if (Zipper.isBranch(z)) {
            z.idx = Math.max(0, insertIdx - 1);
            Zipper down = Zipper.downLeft(z);
            if (down == null) {
              assert Zipper.isRoot(z);
              Node newRoot = Node.empty(z.editingContext.maxChildren);
              newRoot.add(id, from, to, data);
              z.editingContext.parentsMap =
                z.editingContext.parentsMap.put(id, (Long)z.rootId);
              return Zipper.replace(z, newRoot, 0);
            }
            z = down;
          }
          else {
            if (!z.changed) {
              z.ids = z.ids.copy();
              z.starts = z.starts.copy();
              z.ends = z.ends.copy();
              z.children = new ArrayList<>(z.children);
            }
            if (z.editingContext.parentsMap.get(id, null) != null) {
              throw new IllegalArgumentException("id is not unique:" + id);
            }
            z.starts.add(insertIdx, from - z.delta);
            z.ends.add(insertIdx, to - z.delta);
            z.ids.add(insertIdx, id);
            z.children.add(insertIdx, data);
            Long currentId = z.parent.ids.get(z.parent.idx);
            z.editingContext.parentsMap = z.editingContext.parentsMap.put(id, currentId);
            z.changed = true;
            z.idx = insertIdx <= z.idx ? z.idx + 1 : z.idx;
            return z;
          }
        }
        else {
          z = Zipper.up(z);
        }
      }
    }
  }

  private static void splitNode(ArrayList<Node> result, Node source, int from, int to, int thresh) {
    int length = to - from;
    if (length <= thresh) {
      ArrayList<Object> children = new ArrayList<>(to - from);
      children.addAll(source.children.subList(from, to));
      result.add(new Node(source.ids.subList(from, to),
                          source.starts.subList(from, to),
                          source.ends.subList(from, to),
                          children));
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

  static Node splitChildren(EditingContext ctx, Node node) {
    int splitThreshold = ctx.maxChildren;
    if (childrenNeedSplitting(node, splitThreshold)) {
      Node result = Node.empty(splitThreshold / 2);
      IntMap<Long> m = ctx.parentsMap;
      long nextId = ctx.nextId;
      for (int i = 0; i < node.children.size(); i++) {
        Node child = (Node)node.children.get(i);
        long childDelta = node.starts.get(i);
        long childId = node.ids.get(i);
        Long parentId = m.get(childId, null);
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
              m = m.put(newId, parentId);
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

  static Node mergeChildren(EditingContext ctx, Node node) {
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

  public static class EditingContext {
    public long nextId;
    public final int maxChildren;
    public IntMap<Long> parentsMap;

    public EditingContext(long nextId, int maxChildren, IntMap<Long> parentsMap) {
      this.nextId = nextId;
      this.maxChildren = maxChildren;
      this.parentsMap = parentsMap;
    }
  }

  static Node balanceChildren(EditingContext ctx, Node node) {
    return mergeChildren(ctx, splitChildren(ctx, node));
  }

  private static boolean intersects(long s1, long e1, long s2, long e2) {
    return s1 <= s2 ? s2 <= e1 : s1 <= e2;
  }

  static Node growTree(EditingContext ctx, long rootId, Node node) {
    Node balanced = balanceChildren(ctx, node);
    if (balanced.children.size() > ctx.maxChildren) {
      ArrayList<Object> newChildren = new ArrayList<>();
      newChildren.add(balanced);
      long newLevelId = ctx.nextId--;
      ctx.parentsMap = adopt(ctx.parentsMap, newLevelId, balanced.ids);
      ctx.parentsMap = ctx.parentsMap.put(newLevelId, (Long)rootId);
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

  static Node shrinkTree(EditingContext ctx, long rootId, Node root) {
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

  private static long normalize(Node node) {
    if (node.starts.size() == 0) {
      return 0;
    }
    else {
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

  public static class Batch<T> implements Intervals.Batch<T> {
    long lastSeenFrom = Long.MIN_VALUE;
    Zipper closedZipper;
    Zipper openZipper;
    EditingContext editingContext;

    Batch(Zipper openZipper, Zipper closedZipper, EditingContext editingContext) {
      this.openZipper = openZipper;
      this.closedZipper = closedZipper;
      this.editingContext = editingContext;
    }

    @Override
    public void add(long id, long from, long to, boolean closedLeft, boolean closedRight, T data) {
      if (from < lastSeenFrom) {
        throw new IllegalArgumentException("batch is not sorted");
      }
      if (from == to && !((closedLeft && closedRight) || (!closedLeft && !closedRight))) {
        throw new IllegalArgumentException("Interval is empty");
      }

      lastSeenFrom = from;

      if (from * 2 < from || to * 2 < to) {
        throw new ArithmeticException();
      }

      if (closedLeft) {
        this.closedZipper = Zipper.insert(this.closedZipper, id, from, to, closedLeft, closedRight, data);
      }
      else {
        this.openZipper = Zipper.insert(this.openZipper, id, from, to, closedLeft, closedRight, data);
      }
    }

    @Override
    public IntervalsImpl<T> commit() {
      return new IntervalsImpl<>(this.editingContext.maxChildren,
                                 Zipper.root(this.openZipper),
                                 Zipper.root(this.closedZipper),
                                 editingContext.parentsMap.forked(),
                                 editingContext.nextId);
    }
  }

  public static <T> Batch<T> batch(IntervalsImpl<T> tree) {
    EditingContext ctx = new EditingContext(tree.nextInnerId, tree.maxChildren, tree.parentsMap.linear());
    return new Batch<>(Zipper.create(tree.openRoot, ctx, true),
                       Zipper.create(tree.closedRoot, ctx, false),
                       ctx);
  }

  abstract static class AbstractIterator<T> implements IntervalsIterator<T> {
    Zipper z;

    AbstractIterator(Zipper z) {
      this.z = z;
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
      return Zipper.from(z);
    }

    @Override
    public long to() {
      return Zipper.to(z);
    }

    @Override
    public long id() {
      return Zipper.id(z);
    }

    @Override
    public T data() {
      return Zipper.data(z);
    }
  }

  static class ForwardIterator<T> extends AbstractIterator<T> {
    private final long queryFrom;
    private final long queryTo;

    ForwardIterator(Zipper z, long queryFrom, long queryTo) {
      super(z);
      this.queryFrom = queryFrom;
      this.queryTo = queryTo;
    }

    @Override
    public boolean next() {
      Zipper next = Zipper.isRoot(z) ? z : Zipper.skipRight(z);
      z = next == null ? null : nextIntersection(next, queryFrom, queryTo);
      return z != null;
    }
  }

  static class BackwardIterator<T> extends AbstractIterator<T> {
    private final long queryFrom;
    private final long queryTo;

    BackwardIterator(Zipper z, long queryFrom, long queryTo) {
      super(z);
      this.queryFrom = queryFrom;
      this.queryTo = queryTo;
    }

    @Override
    public boolean next() {
      Zipper next = Zipper.isRoot(z) ? z : Zipper.skipLeft(z);
      z = next == null ? null : prevIntersection(next, queryFrom, queryTo);
      return z != null;
    }
  }

  public static Zipper skipToOffset(Zipper z, long offset) {
    for (int i = z.idx; i < z.starts.size(); ++i) {
      if (z.starts.get(i) + z.delta >= offset) {
        if (Zipper.isBranch(z)) {
          Zipper down = Zipper.downLeft(z);
          return down == null ? z : skipToOffset(down, offset);
        }
        else {
          z.idx = i - 1;
          return z;
        }
      }
    }
    if (z.hasRightCousin) {
      return skipToOffset(Zipper.up(z), offset);
    }
    else {
      z.idx = z.starts.size() - 1;
      if (Zipper.isBranch(z)) {
        Zipper down = Zipper.downLeft(z);
        return down == null ? z : skipToOffset(down, offset);
      }
      else {
        return z;
      }
    }
  }

  public static Zipper nextIntersection(Zipper zipper, long from, long to) {
    for (int i = zipper.idx; i < zipper.starts.size(); ++i) {
      if (intersects(from, to, zipper.starts.get(i) + zipper.delta, zipper.ends.get(i) + zipper.delta)) {
        zipper.idx = i;
        if (Zipper.isBranch(zipper)) {
          Zipper down = Zipper.downLeft(zipper);
          return down == null ? null : nextIntersection(down, from, to);
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

    Zipper skip = Zipper.skipRight(up);
    return skip == null ? null : nextIntersection(skip, from, to);
  }

  static Zipper prevIntersection(Zipper zipper, long from, long to) {
    for (int i = zipper.idx; i >= 0; --i) {
      if (intersects(from, to, zipper.starts.get(i) + zipper.delta, zipper.ends.get(i) + zipper.delta)) {
        zipper.idx = i;
        if (Zipper.isBranch(zipper)) {
          Zipper down = Zipper.downRight(zipper);
          return down == null ? null : prevIntersection(down, from, to);
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

    Zipper skip = Zipper.skipLeft(up);
    return skip == null ? null : prevIntersection(skip, from, to);
  }

  public static <T> IntervalsIterator<T> query(IntervalsImpl<T> tree, long from, long to) {
    assert from <= to;
    from = Math.min(MAX_VALUE, from);
    to = Math.min(MAX_VALUE, to);
    return new MergingIterator<>(
      new ForwardIterator<>(Zipper.create(tree.openRoot, null, true), from * 2 - 1, to * 2),
      new ForwardIterator<>(Zipper.create(tree.closedRoot, null, false), from * 2 - 1, to * 2),
      IntervalsIterator.FORWARD_COMPARATOR);
  }

  public static <T> IntervalsIterator<T> queryReverse(IntervalsImpl<T> tree, long from, long to) {
    assert from <= to;
    from = Math.min(MAX_VALUE, from);
    to = Math.min(MAX_VALUE, to);
    return new MergingIterator<>(
      new BackwardIterator<>(Zipper.create(tree.openRoot, null, true), from * 2 - 1, to * 2),
      new BackwardIterator<>(Zipper.create(tree.closedRoot, null, false), from * 2 - 1, to * 2),
      IntervalsIterator.BACKWARD_COMPARATOR);
  }

  static Node expand(Node node, long offset, long len) {
    Node result = Node.empty(node.children.size());
    for (int i = 0; i < node.children.size(); i++) {
      long id = node.ids.get(i);
      long start = node.starts.get(i);
      long end = node.ends.get(i);
      Object child = node.children.get(i);
      if (start < offset && offset < end) {
        //....(interval)....
        //........o.........
        Object c = child instanceof Node
                   ? expand((Node)child, offset - start, len)
                   : child;
        result.add(id, start, end + len, c);
      }
      else if (offset <= start) {
        //......(interval)....
        //....o...............
        result.add(id, start + len, end + len, child);
      }
      else {
        //....(interval)....
        //...............o....
        result.add(id, start, end, child);
      }
    }
    return result;
  }

  public static <T> IntervalsImpl<T> expand(IntervalsImpl<T> tree, long start, long len) {
    if (len == 0) {
      return tree;
    }
    return new IntervalsImpl<>(tree.maxChildren,
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

  static Node collapse(EditingContext ctx, long d, Node node, long offset, long len) {
    Node result = Node.empty(node.children.size());
    for (int i = 0; i < node.children.size(); i++) {
      long start = node.starts.get(i);
      long end = node.ends.get(i);
      Object child = node.children.get(i);
      long id = node.ids.get(i);
      if (end <= offset) {
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
          if (newEnd - newStart < 2) {
            ctx.parentsMap = extinct(ctx.parentsMap, id, child);
          }
          else {
            result.add(id, newStart, newEnd, child);
          }
        }
      }
    }
    return balanceChildren(ctx, result);
  }

  public static <T> IntervalsImpl<T> collapse(IntervalsImpl<T> tree, long start, long len) {
    if (len == 0) {
      return tree;
    }
    EditingContext ctx = new EditingContext(tree.nextInnerId, tree.maxChildren, tree.parentsMap.linear());
    Node openRoot = shrinkTree(ctx, OPEN_ROOT_ID,
                               growTree(ctx, OPEN_ROOT_ID,
                                        collapse(ctx, 0, tree.openRoot, start * 2, len * 2)));
    Node closedRoot = shrinkTree(ctx, CLOSED_ROOT_ID,
                                 growTree(ctx, CLOSED_ROOT_ID,
                                          collapse(ctx, 0, tree.closedRoot, start * 2, len * 2)));
    return new IntervalsImpl<>(tree.maxChildren, openRoot, closedRoot, ctx.parentsMap.forked(), ctx.nextId);
  }

  private static Node remove(EditingContext ctx, Node node, long nodeId, HashMap<Long, HashSet<Long>> subtree) {
    HashSet<Long> victims = subtree.get(nodeId);
    if (victims == null) {
      return node;
    }
    Node copy = node.copy();
    for (Long vid : victims) {
      int vidx = copy.ids.indexOf(vid);
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
    return balanceChildren(ctx, copy);
  }

  public static <T> IntervalsImpl<T> remove(IntervalsImpl<T> tree, Iterable<Long> ids) {
    HashMap<Long, HashSet<Long>> deletionSubtree = deletionSubtree(tree.parentsMap, ids);
    EditingContext ctx = new EditingContext(tree.nextInnerId, tree.maxChildren, tree.parentsMap.linear());

    Node openRoot = shrinkTree(ctx, OPEN_ROOT_ID, remove(ctx, tree.openRoot, OPEN_ROOT_ID, deletionSubtree));
    Node closedRoot = shrinkTree(ctx, CLOSED_ROOT_ID, remove(ctx, tree.closedRoot, CLOSED_ROOT_ID, deletionSubtree));

    return new IntervalsImpl<>(tree.maxChildren,
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
        throw new NoSuchElementException("id: " + id + " not found in tree");
      }
      path.add(p);
      if (p == OPEN_ROOT_ID || p == CLOSED_ROOT_ID) {
        return path;
      }
    }
  }

  static Node chooseRoot(IntervalsImpl tree, long id) {
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

  public static <T> Interval<T> getById(IntervalsImpl<T> tree, long id) {
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

  private static HashMap<Long, HashSet<Long>> deletionSubtree(IntMap<Long> parents, Iterable<Long> toBeDeleted) {
    HashMap<Long, HashSet<Long>> subtree = new HashMap<>();

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

          HashSet<Long> sibs = subtree.getOrDefault(pid, new HashSet<>());
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
}
