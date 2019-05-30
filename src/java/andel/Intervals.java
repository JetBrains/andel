package andel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

public class Intervals<T> {

  // public PersistentIntMap parents;

  /*
   * we employ two separate trees to store markers with greedy and not greedy start
   * this is necessary because we store intervals sorted by start offset
   * this order is hard to preserve in situations of open/closed intervals starting on the same offset
   * e.g. (0, 5) [1 3) -> collapse(0, 2) -> (0, 3) [0, 1] -> expand(0, 1) -> (1, 4) [0, 2] !!!
   * */
  public Node openRoot;
  public Node closedRoot;
  public int maxChildren;

  public Intervals(int maxChildren, Node openRoot, Node closedRoot) {
    this.maxChildren = maxChildren;
    this.openRoot = openRoot;
    this.closedRoot = closedRoot;
  }

  public Intervals(int maxChildren) {
    this(maxChildren, Node.empty(maxChildren / 2), Node.empty(maxChildren / 2));
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

    public void add(long id, long start, long end, Object child){
      this.ids.add(id);
      this.starts.add(start);
      this.ends.add(end);
      this.children.add(child);
    }
  }

  public static class Interval<T> {
    public long id;
    public long from;
    public long to;
    public T data;

    public Interval(long id, long from, long to, T data) {
      this.id = id;
      this.from = from;
      this.to = to;
      this.data = data;
    }

    @Override
    public String toString() {
      return "[id=" + id + ", from=" + from + ", to=" + to + "]";
    }
  }

  public static class Zipper {
    public boolean startsAreOpen;
    public int MAX_CHILDREN;
    public boolean changed = false;
    public boolean hasRightCousin;
    public long delta = 0;
    public Zipper parent = null;
    public long rightCousinStart;
    public LongArrayList starts;
    public LongArrayList ends;
    public LongArrayList ids;
    public ArrayList<Object> children;
    public int idx = 0;

    public static Zipper create(Node root, int maxChildren, boolean greedyLeft) {
      Zipper zipper = new Zipper();
      zipper.MAX_CHILDREN = maxChildren;
      zipper.startsAreOpen = greedyLeft;
      zipper.rightCousinStart = Long.MAX_VALUE;
      zipper.hasRightCousin = false;
      zipper.starts = new LongArrayList(new long[]{0});
      zipper.ends = new LongArrayList(new long[]{Long.MAX_VALUE});
      zipper.ids = new LongArrayList(new long[]{0});
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
        r.MAX_CHILDREN = z.MAX_CHILDREN;
        r.starts = child.starts;
        r.ends = child.ends;
        r.ids = child.ids;
        r.delta = z.delta + z.starts.get(z.idx);
        r.hasRightCousin = z.hasRightCousin || z.idx < z.children.size() - 1;
        r.rightCousinStart = z.idx + 1 < z.starts.size()
                             ? z.starts.get(z.idx + 1)
                             : z.rightCousinStart;
        r.children = child.children;
        r.idx = 0;
        return r;
      }
      else {
        throw new IllegalArgumentException();
      }
    }

    public static Zipper insert(Zipper z, int idx, long id, long start, long end, Object data) {
      z.starts.add(idx, start - z.delta);
      z.ends.add(idx, end - z.delta);
      z.ids.add(idx, id);
      z.children.add(idx, data);
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
        long delta = normalize(n);
        n = balanceChildren(n, z.MAX_CHILDREN);

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

  static Node splitChildren(Node node, int splitThreshold) {
    if (childrenNeedSplitting(node, splitThreshold)) {
      Node result = Node.empty(splitThreshold / 2);
      for (int i = 0; i < node.children.size(); i++) {
        Node child = (Node)node.children.get(i);
        long childDelta = node.starts.get(i);
        if (child.children.size() > splitThreshold) {
          ArrayList<Node> partition = splitNode(child, splitThreshold);
          for (Node p : partition) {
            long delta = normalize(p);
            result.children.add(p);
            result.starts.add(delta + childDelta);
            result.ends.add(max(p.ends) + childDelta + delta);
            result.ids.add(0); // TODO assign meaningful id and adopt all children
          }
        }
        else {
          result.children.add(child);
          result.starts.add(node.starts.get(i));
          result.ends.add(node.ends.get(i));
          result.ids.add(node.ids.get(i));
        }
      }
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

  static Node mergeChildren(Node node, int splitThreshold) {
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
          Node merged = mergeChildren(mergeNodes(leftDelta, left, rightDelta, right), splitThreshold);
          if (merged.children.size() > splitThreshold) {
            ArrayList<Node> split = splitNode(merged,
                                              splitThreshold); // will split in two equals parts, but probably we should split mergeThresh, total - mergeThresh;
            assert split.size() == 2;

            result.children.add(split.get(0));
            result.starts.add(leftDelta);
            result.ends.add(leftDelta + max(split.get(0).ends));
            result.ids.add(leftId);
            // TODO some children may come from current right, should adopt them

            left = split.get(1);
            leftDelta += normalize(split.get(1));
            leftEnd = leftDelta + max(split.get(1).ends);
            leftId = 0; // TODO assign meaningful id and adopt children
          }
          else {
            left = merged;
            leftEnd = leftDelta + max(merged.ends);
            // TODO adopt children from right
          }
        }
        else {
          result.children.add(left);
          result.starts.add(leftDelta);
          result.ends.add(leftEnd);
          result.ids.add(leftId);
          left = right;
          leftDelta = rightDelta;
          leftId = rightId;
          leftEnd = rightEnd;
        }
      }
      return result;
    }
    else {
      return node;
    }
  }

  static Node balanceChildren(Node node, int maxChildren) {
    // TODO we can use the fact that insertions are separated from deletions
    // you don't need to assume both can happen on the same zipper
    return mergeChildren(splitChildren(node, maxChildren), maxChildren);
  }

  private static boolean intersects(long s1, long e1, long s2, long e2) {
    return s1 <= s2 ? s2 < e1 : s1 < e2;
  }

  static Node growTree(Node node, int maxChildren) {
    Node balanced = balanceChildren(node, maxChildren);
    if (balanced.children.size() > maxChildren) {
      ArrayList<Object> newChildren = new ArrayList<>();
      newChildren.add(balanced);
      // todo generate proper id, adopt
      return growTree(new Node(new LongArrayList(new long[]{-1}),
                               new LongArrayList(new long[]{0}),
                               new LongArrayList(new long[]{Long.MAX_VALUE}),
                               newChildren),
                      maxChildren);
    }
    else {
      return balanced;
    }
  }

  static Node shrinkTree(Node node) {
    if (node.children.size() == 1 && node.children.get(0) instanceof Node) {
      return shrinkTree((Node)node.children.get(0));
    }
    else {
      return node;
    }
  }

  static Node root(Zipper z) {
    while (!Zipper.isRoot(z)) {
      z = Zipper.up(z);
    }
    return shrinkTree(growTree(Zipper.node(z), z.MAX_CHILDREN));
  }

  static <T> Zipper insert(Zipper z, long id, long from, long to, T data) {
    while (true) {
      if (from <= z.rightCousinStart) { // TODO should i check left neighbour?
        int insertIdx = z.starts.binarySearch(from - z.delta);
        insertIdx = insertIdx < 0 ? ~insertIdx : insertIdx;
        if (Zipper.isBranch(z)) {
          z.idx = Math.max(0, insertIdx - 1);
          Zipper down = Zipper.down(z);
          if (down == null) {
            assert Zipper.isRoot(z);
            Node newRoot = Node.empty(z.MAX_CHILDREN);
            newRoot.starts.add(from);
            newRoot.ends.add(to);
            newRoot.ids.add(id);
            newRoot.children.add(data);
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
    long delta = node.starts.get(0);
    if (delta != 0) {
      for (int i = 0; i < node.starts.size(); i++) {
        node.starts.set(i, node.starts.get(i) - delta);
        node.ends.set(i, node.ends.get(i) - delta);
      }
    }
    return delta;
  }

  public static class Bulk<T> {
    long lastSeenFrom = Long.MIN_VALUE;

    Bulk(Zipper closedZipper, Zipper openZipper) {
    }

    public Bulk<T> consume(long id, long from, long to, int greediness, T data) {
      throw new UnsupportedOperationException();
    }

    public Intervals<T> commit() {
      throw new UnsupportedOperationException();
    }
  }

  public static <T> Intervals<T> insert(Intervals<T> tree, List<Interval<T>> intervals) {
    Zipper z = Zipper.create(tree.openRoot, tree.maxChildren, false);
    for (Interval<T> interval : intervals) {
      z = insert(z, interval.id, interval.from, interval.to, interval.data);
    }
    Node newRoot = root(z);
    return new Intervals<>(tree.maxChildren, newRoot, tree.closedRoot);
  }

  public static class IntervalsIterator<T> {
    private Zipper z;
    private final long queryFrom;
    private final long queryTo;

    public IntervalsIterator(Zipper z, long queryFrom, long queryTo) {
      this.z = z;
      this.queryFrom = queryFrom;
      this.queryTo = queryTo;
    }

    public long from() {
      return z.delta + z.starts.get(z.idx);
    }

    public long to() {
      return z.delta + z.ends.get(z.idx);
    }

    public long id() {
      return z.ids.get(z.idx);
    }

    public T data() {
      //noinspection unchecked
      return (T)z.children.get(z.idx);
    }

    public boolean next() {
      Zipper next = Zipper.isRoot(z) ? z : Zipper.skip(z);
      z = next == null ? null : findNextIntersection(next, queryFrom, queryTo);
      return z != null;
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
    Zipper skip = Zipper.skip(Zipper.up(zipper));
    return skip == null ? null : findNextIntersection(skip, from, to);
  }

  public static IntervalsIterator query(Intervals tree, long start, long end) {
    return new IntervalsIterator(Zipper.create(tree.openRoot, tree.maxChildren, false), start, end);
  }

  static Node expand(Node node, long offset, long len){
    Node result = Node.empty(node.children.size());
    for (int i = 0; i < node.children.size(); i++) {
      if (node.ends.get(i) < offset) {
        result.add(node.ids.get(i),
                   node.starts.get(i),
                   node.ends.get(i),
                   node.children.get(i));
      }
      else if (node.starts.get(i) < offset) {
        Object child = node.children.get(i);
        result.add(node.ids.get(i),
                   node.starts.get(i),
                   node.ends.get(i) + len,
                   child instanceof Node
                   ? expand((Node)child, offset - node.starts.get(i), len)
                   : child);

      } else {
        result.add(node.ids.get(i),
                   node.starts.get(i) + len,
                   node.ends.get(i) + len,
                   node.children.get(i));
      }
    }
    return result;
  }

  public static <T> Intervals<T> expand(Intervals<T> tree, long start, long len) {
    return new Intervals<T>(tree.maxChildren, expand(tree.openRoot, start, len), tree.closedRoot);
  }

  public static Intervals collapse(Intervals tree, long start, long len) {
    throw new UnsupportedOperationException();
  }

  public static Node remove(Node n, Set<Integer> ids) {
    throw new UnsupportedOperationException();
  }

  public static <T> Interval<T> getById(Intervals<T> tree, long id) {
    throw new UnsupportedOperationException();
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
      System.arraycopy(buffer, to, buffer, from, to - from);
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

    public LongArrayList subList(int from, int to) {
      LongArrayList r = new LongArrayList(to - from);
      System.arraycopy(this.buffer, from, r.buffer, 0, to - from);
      r.size = to - from;
      return r;
    }
  }
}
