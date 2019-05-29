package andel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

public class Intervals<T> {

  // public PersistentIntMap parents;

  public Node root;
  public Node greedyRoot;
  public int maxChildren;

  Intervals(int maxChildren, Node openRoot, Node closedRoot) {
    this.maxChildren = maxChildren;
    this.root = openRoot;
    this.greedyRoot = closedRoot;
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
  }

  public static class Interval<T> {
    public long id;
    public long from;
    public long to;
    public T data;
  }

  public static class Zipper {
    public boolean isGreedyLeft;
    public int MAX_CHILDREN;
    public boolean isChanged = false;
    public long delta = 0;
    public Zipper parent = null;
    public long from;
    public long to;
    public long rightNeighbour;
    public LongArrayList starts;
    public LongArrayList ends;
    public LongArrayList ids;
    public ArrayList<Object> children;
    public int idx = 0;

    public static Zipper create(Node root, int maxChildren, boolean greedyLeft) {
      Zipper zipper = new Zipper();
      zipper.MAX_CHILDREN = maxChildren;
      zipper.isGreedyLeft = greedyLeft;
      zipper.starts = root.starts;
      zipper.ends = root.ends;
      zipper.ids = root.ids;
      zipper.rightNeighbour = Long.MAX_VALUE;
      zipper.children = root.children;
      return zipper;
    }

    public static boolean isBranch(Zipper z) {
      return z.children.get(z.idx) instanceof Node;
    }

    public static Zipper down(Zipper z) {
      if (isBranch(z)) {
        Node child = (Node)z.children.get(z.idx);
        Zipper r = new Zipper();
        r.parent = z;
        r.starts = child.starts;
        r.ends = child.ends;
        r.ids = child.ids;
        r.delta = z.delta + z.starts.get(z.idx);
        r.rightNeighbour = z.idx + 1 < z.starts.size()
                           ? z.starts.get(z.idx + 1)
                           : z.rightNeighbour;
        r.children = child.children;
        return r;
      }
      else {
        throw new IllegalArgumentException();
      }
    }

    public static Zipper insert(Zipper z, long id, long start, long end, Object data) {
      throw new UnsupportedOperationException();
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
        if (((Node)child).children.size() > splitThreshold) {
          return true;
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
        if (((Node)child).children.size() < threshold) {
          return true;
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
          if (left.children.size() < mergeThreshold || right.children.size() < mergeThreshold){

            Node merged = mergeChildren(mergeNodes(leftDelta, left, rightDelta, right), splitThreshold);
            if (merged.children.size() > splitThreshold){
              ArrayList<Node> split = splitNode(merged, splitThreshold);

            } else {

            }
            throw new UnsupportedOperationException();
          } else {
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

    public static Zipper up(Zipper z) {
      if (z.isChanged) {
        Zipper p = z.parent;
        if (!p.isChanged) {
          p.ids = p.ids.copy();
          p.starts = p.starts.copy();
          p.ends = p.ends.copy();
          p.children = new ArrayList<>(p.children);
        }
        Node n = new Node(z.ids, z.starts, z.ends, z.children);
        long delta = normalize(n);
        n = balanceChildren(n, z.MAX_CHILDREN);

        p.children.set(p.idx, n);
        p.starts.set(p.idx, p.starts.get(p.idx) + delta);
        p.ends.set(p.idx, p.starts.get(p.idx) + delta + max(n.ends));
        p.isChanged = true;
        return p;
      }
      else {
        return z.parent;
      }
    }
  }

  public static class IntervalsIterator {

  }

  private static boolean intersects(long s1, long e1, long s2, long e2) {
    return s1 <= s2 ? s2 < e1 : s1 < e2;
  }

  static Node root(Zipper z) {
    while (z.parent != null) {
      z = Zipper.up(z);
    }
    //TODO grow/shrink
    //return new Intervals<>();
    throw new UnsupportedOperationException();
  }

  static <T> Zipper insert(Zipper z, long id, long from, long to, T data) {
    while (true) {
      if (from <= z.rightNeighbour) { // TODO should i check left neighbour?
        int insertIdx = z.starts.binarySearch(from - z.delta);
        insertIdx = insertIdx < 0 ? Math.max(0, ~insertIdx - 1) : insertIdx;
        z.idx = insertIdx;   // TODO pass idx to down and insert
        if (Zipper.isBranch(z)) {
          z = Zipper.down(z);
        }
        else {
          return Zipper.insert(z, id, from, to, data);
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
    Zipper z = Zipper.create(tree.root, tree.maxChildren, false);
    for (Interval<T> interval : intervals) {
      z = insert(z, interval.id, interval.from, interval.to, interval.data);
    }
    Node newRoot = root(z);
    throw new UnsupportedOperationException();
  }

  public static IntervalsIterator query(Intervals tree, long start, long end) {
    //Zipper z = Zipper.create(tree.root);
    //for(int i = 0, delta = 0; i < n.children.length; delta += n.deltas[i++]){
    //  if (intersects(start, end, n.starts[i] + delta, n.ends[i] + delta)){
    //
    //  }
    //}
    throw new UnsupportedOperationException();
  }

  public static Intervals expand(Intervals tree, long start, long len) {
    throw new UnsupportedOperationException();
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
      assert idx < size;
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
      return Arrays.copyOfRange(buffer, 0, size).toString();
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
      assert from > 0;
      assert to < size;
      LongArrayList r = new LongArrayList(from - to);
      System.arraycopy(this.buffer, from, r.buffer, 0, from - to);
      r.size = from - to;
      return r;
    }
  }
}
