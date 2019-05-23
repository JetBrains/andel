package andel;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.BiFunction;

public class Rope {

  public static class Node<Metrics> {
    public ArrayList<Object> children;
    // store precalculated accumulator values in childrenMetrics to use binary search
    public Metrics metrics;
    public ArrayList<Metrics> childrenMetrics;

    public Node(Metrics metrics, ArrayList<Object> children, ArrayList<Metrics> childrenMetrics) {
      this.children = children;
      this.metrics = metrics;
      this.childrenMetrics = childrenMetrics;
    }
  }

  public static class Leaf<Metrics, Data> {
    public Metrics metrics;
    public Data data;
  }

  public interface ZipperOps<Metrics, Data> {

    Metrics calculateMetrics(Data data);

    Metrics emptyMetrics();

    Metrics rf(Metrics m1, Metrics m2);

    default Metrics rf(List<Metrics> metrics) {
      Metrics r = emptyMetrics();
      for (Metrics m : metrics) {
        r = rf(r, m);
      }
      return r;
    }

    boolean isLeafOverflown(Data leafData);

    boolean isLeafUnderflown(Data leafData);

    Data mergeLeaves(Data leafData1, Data leafData22);

    List<Data> splitLeaf(Data leaf);

    int splitThreshold();
  }

  public static class Zipper<Metrics, Data> implements Cloneable {
    public ZipperOps<Metrics, Data> ops;

    public Zipper<Metrics, Data> parent;
    public ArrayList<Object> siblings;
    public ArrayList<Metrics> metrics;
    public int idx = 0;

    public Metrics acc;
    public Metrics oacc;

    public boolean isChanged = false;
    public boolean isTransient = false;
    //public boolean isRoot = false; // suspiciously useless flag

    public Zipper() { }

    @Override
    public Zipper clone() {
      try {
        return (Zipper)super.clone();
      }
      catch (CloneNotSupportedException e) {
        throw new RuntimeException(e);
      }
    }

    public static <Metrics, Data> Zipper<Metrics, Data> zipper(Node<Metrics> node, ZipperOps<Metrics, Data> ops) {
      Zipper<Metrics, Data> z = new Zipper<>();
      z.parent = null;
      z.ops = ops;
      z.idx = 0;
      z.siblings = singletonList(node);
      z.metrics = singletonList(node.metrics);
      z.acc = ops.emptyMetrics();
      z.oacc = null;
      return z;
    }
  }

  private static <T> ArrayList<T> singletonList(T object) {
    ArrayList<T> l = new ArrayList<>(1);
    l.add(object);
    return l;
  }

  public static <Metrics, Data> Node<Metrics> node(Zipper<Metrics, Data> loc) {
    return (Node<Metrics>)loc.siblings.get(loc.idx);
  }

  public static <Metrics, Data> Leaf<Metrics, Data> leaf(Zipper<Metrics, Data> loc) {
    return (Leaf<Metrics, Data>)loc.siblings.get(loc.idx);
  }

  public static <Metrics, Data> Metrics currentAcc(Zipper<Metrics, Data> loc) {
    if (loc.oacc != null) {
      return loc.oacc;
    }
    if (loc.acc != null) {
      return loc.acc;
    }
    return loc.ops.emptyMetrics();
  }

  public static boolean isBranch(Zipper loc) {
    return loc.siblings.get(loc.idx) instanceof Node;
  }

  public static boolean isRoot(Zipper loc){
    return loc.parent == null;
  }

  public static ArrayList<Object> getChildren(Object node) {
    return ((Node)node).children;
  }

  public static Object metrics(Object node) {
    if (node instanceof Node) {
      return ((Node)node).metrics;
    }
    else {
      return ((Leaf)node).metrics;
    }
  }

  public static boolean isRightmost(Zipper location) {
    return location.idx == location.siblings.size() - 1 && (location.parent == null || isRightmost(location.parent));
  }

  public static boolean isLeftmost(Zipper location) {
    return location.idx == 0 && (location.parent == null || isLeftmost(location.parent));
  }

  public static <Metrics, Data> Node<Metrics> makeNode(ArrayList<Object> children, ZipperOps<Metrics, Data> ops) {
    ArrayList<Metrics> childrenMetrics = new ArrayList<>(children.size());
    for (Object child : children) {
      childrenMetrics.add((Metrics)metrics(child));
    }
    return new Node<>(ops.rf(childrenMetrics), children, childrenMetrics);
  }

  public static <Metrics, Data> Leaf<Metrics, Data> makeLeaf(Data data, ZipperOps<Metrics, Data> ops) {
    Leaf<Metrics, Data> l = new Leaf<>();
    l.data = data;
    l.metrics = ops.calculateMetrics(data);
    return l;
  }

  static <Metrics, Data> boolean splitNeeded(ArrayList<Object> children, ZipperOps<Metrics, Data> ops) {
    if (children.get(0) instanceof Node) {
      for (Object child : children) {
        if (getChildren(child).size() > ops.splitThreshold()) {
          return true;
        }
      }
    }
    else {
      for (Object child : children) {
        if (ops.isLeafOverflown(((Leaf<Metrics, Data>)child).data)) {
          return true;
        }
      }
    }
    return false;
  }

  private static void partitionList(ArrayList<ArrayList<Object>> result, ArrayList<Object> source, int from, int to, int thresh) {
    int length = to - from;
    if (length <= thresh) {
      ArrayList<Object> part = new ArrayList<>(thresh);
      part.addAll(source.subList(from, to));
      result.add(part);
    }
    else {
      int half = length / 2;
      partitionList(result, source, from, from + half, thresh);
      partitionList(result, source, from + half, to, thresh);
    }
  }

  static ArrayList<ArrayList<Object>> partitionChildren(ArrayList<Object> children, int threshold) {
    ArrayList<ArrayList<Object>> result = new ArrayList<>();
    partitionList(result, children, 0, children.size(), threshold);
    return result;
  }

  static <Metrics, Data> ArrayList<Object> splitChildren(ArrayList<Object> children, ZipperOps<Metrics, Data> ops) {
    if (splitNeeded(children, ops)) {
      ArrayList<Object> result = new ArrayList<>(children.size() / 2);
      for (Object child : children) {
        if (child instanceof Node && getChildren(child).size() > ops.splitThreshold()) {
          ArrayList<ArrayList<Object>> partition = partitionChildren(getChildren(child), ops.splitThreshold());
          for (ArrayList<Object> part : partition) {
            result.add(makeNode(part, ops));
          }
        }
        else if (child instanceof Leaf && ops.isLeafOverflown(((Leaf<Metrics, Data>)child).data)) {
          for (Data o : ops.splitLeaf(((Leaf<Metrics, Data>)child).data)) {
            result.add(makeLeaf(o, ops));
          }
        }
        else {
          result.add(child);
        }
      }
      return result;
    }
    return children;
  }

  static <Metrics, Data> boolean isMergeNeeded(ArrayList<Object> children, ZipperOps<Metrics, Data> ops) {
    int mergeThreshold = ops.splitThreshold() / 2;
    if (children.get(0) instanceof Node) {
      for (Object child : children) {
        if (getChildren(child).size() < mergeThreshold) {
          return true;
        }
      }
    }
    else {
      for (Object child : children) {
        if (ops.isLeafUnderflown(((Leaf<Metrics, Data>)child).data)) {
          return true;
        }
      }
    }
    return false;
  }

  static <Metrics, Data> ArrayList<Object> mergeChildren(ArrayList<Object> children, ZipperOps<Metrics, Data> ops) {
    final int mergeThreshold = ops.splitThreshold() / 2;
    if (isMergeNeeded(children, ops)) {
      ArrayList<Object> result = new ArrayList<>(mergeThreshold);
      if (children.get(0) instanceof Node) {
        Object left = children.get(0);
        for (int i = 1; i < children.size(); i++) {
          Object right = children.get(i);
          ArrayList<Object> leftChildren = getChildren(left);
          ArrayList<Object> rightChildren = getChildren(right);

          if (leftChildren.size() < mergeThreshold || rightChildren.size() < mergeThreshold) {
            if (leftChildren.size() + rightChildren.size() > ops.splitThreshold()) {
              int n = (leftChildren.size() + rightChildren.size()) / 2;
              // todo no actual need in tmp array here
              ArrayList<Object> tmp = joinChildren(leftChildren, rightChildren);

              ArrayList<Object> newLeft = new ArrayList<>(n);
              newLeft.addAll(tmp.subList(0, n));
              ArrayList<Object> newRight = new ArrayList<>(tmp.size() - n);
              newRight.addAll(tmp.subList(n, tmp.size()));
              result.add(makeNode(mergeChildren(newLeft, ops), ops));
              left = makeNode(newRight, ops);
            }
            else {
              left = makeNode(mergeChildren(joinChildren(leftChildren, rightChildren), ops), ops);
            }
          }
          else {
            result.add(left);
            left = right;
          }
        }
        result.add(left);
      }
      else {
        Data leftData = ((Leaf<Metrics, Data>)children.get(0)).data;

        for (int i = 1; i < children.size(); i++) {
          Data rightData = ((Leaf<Metrics, Data>)children.get(i)).data;
          if (ops.isLeafUnderflown(leftData) || ops.isLeafUnderflown(rightData)) {
            Data mergedData = ops.mergeLeaves(leftData, rightData);
            if (ops.isLeafOverflown(mergedData)) {
              List<Data> split = ops.splitLeaf(mergedData);
              Data dataLeft = split.get(0);
              Data dataRight = split.get(1);
              result.add(makeLeaf(dataLeft, ops));
              leftData = dataRight;
            }
            else {
              leftData = mergedData;
            }
          }
          else {
            result.add(makeLeaf(leftData, ops));
            leftData = rightData;
          }
        }
        result.add(makeLeaf(leftData, ops));
      }
      return result;
    }
    return children;
  }

  static ArrayList<Object> joinChildren(ArrayList<Object> leftChildren, ArrayList<Object> rightChildren) {
    ArrayList<Object> tmp = new ArrayList<>(leftChildren.size() + rightChildren.size());
    tmp.addAll(leftChildren);
    tmp.addAll(rightChildren);
    return tmp;
  }

  static <Metrics, Data> ArrayList<Object> balanceChildren(ArrayList<Object> children, ZipperOps<Metrics, Data> ops) {
    return mergeChildren(splitChildren(children, ops), ops);
  }

  public static <Metrics, Data> Node<Metrics> growTree(ArrayList<Object> children, ZipperOps<Metrics, Data> ops) {
    ArrayList<Object> balanced = balanceChildren(children, ops);
    if (balanced.size() > ops.splitThreshold()) {
      return growTree(singletonList(makeNode(balanced, ops)), ops);
    }
    else {
      return makeNode(balanced, ops);
    }
  }

  static <Metrics> Node<Metrics> shrinkTree(Node<Metrics> node) {
    ArrayList<Object> children = getChildren(node);
    if (children.size() == 1 && children.get(0) instanceof Node) {
      return shrinkTree((Node<Metrics>)children.get(0));
    }
    return node;
  }

  static boolean isMutable(Zipper loc) {
    return loc.isTransient && loc.isChanged;
  }

  public static <Metrics, Data> Zipper<Metrics, Data> replace(Zipper<Metrics, Data> loc, Object node) {
    Zipper<Metrics, Data> copy = loc.clone();
    copy.isChanged = true;
    if (isMutable(copy)) {
      copy.siblings.set(copy.idx, node);
    }
    else {
      ArrayList<Object> siblingsCopy = (ArrayList<Object>)copy.siblings.clone();
      ArrayList<Metrics> metricsCopy = (ArrayList<Metrics>)copy.metrics.clone();
      siblingsCopy.set(copy.idx, node);
      metricsCopy.set(copy.idx, (Metrics)metrics(node));
      copy.siblings = siblingsCopy;
      copy.metrics = metricsCopy;
    }
    return copy;
  }

  public static <Metrics, Data> Zipper<Metrics, Data> up(Zipper<Metrics, Data> loc) {
    if (loc.isChanged) {
      if (loc.parent == null) {
        Zipper<Metrics, Data> zipper = new Zipper<>();
        zipper.ops = loc.ops;
        zipper.isTransient = loc.isTransient;
        zipper.idx = 0;
        Node<Metrics> node = shrinkTree(growTree(singletonList(node(loc)), loc.ops));
        zipper.siblings = singletonList(node);
        zipper.metrics = singletonList(node.metrics);
//        zipper.isRoot = true;
        return zipper;
      }
      else {
        return replace(loc.parent, makeNode(balanceChildren(loc.siblings, loc.ops), loc.ops));
      }
    }
    else {
      return loc.parent;
    }
  }

  /*
   * moves to next direct sibling and adds metrics of current node to accumulator
   * returns null if it is last child
   * */
  public static <Metrics, Data> Zipper<Metrics, Data> right(Zipper<Metrics, Data> loc) {
    if (loc.idx < loc.siblings.size() - 1) {
      Zipper<Metrics, Data> zipper = new Zipper<>();
      zipper.ops = loc.ops;
      zipper.idx = loc.idx + 1;
      zipper.siblings = loc.siblings;
      zipper.metrics = loc.metrics;
      zipper.acc = loc.acc == null
                   ? loc.metrics.get(loc.idx)
                   : loc.ops.rf(loc.acc, loc.metrics.get(loc.idx));
      zipper.oacc = null;
      zipper.parent = loc.parent;
      zipper.isChanged = loc.isChanged;
      return zipper;
    }
    return null;
  }

  /*
   * moves to first child of current node
   * returns null for leaves and empty nodes
   * */
  public static <Metrics, Data> Zipper<Metrics, Data> downLeft(Zipper<Metrics, Data> loc) {
    if (isBranch(loc)) {
      Node<Metrics> n = node(loc);
      if (n.children.isEmpty()) {
        return null;
      }
      Zipper<Metrics, Data> zipper = new Zipper<>();
      zipper.ops = loc.ops;
      zipper.siblings = n.children;
      zipper.metrics = n.childrenMetrics;
      zipper.acc = loc.acc;
      zipper.oacc = null;
      zipper.idx = 0;
      zipper.parent = loc;
      return zipper;
    }
    return null;
  }

  /*
   * moves to last child of current node
   * returns null for leaves and empty nodes
   *
   * CAUTION: drops accumulated position
   * */
  public static <Metrics, Data> Zipper<Metrics, Data> downRight(Zipper<Metrics, Data> loc) {
    if (isBranch(loc)) {
      Node<Metrics> n = node(loc);
      if (n.children.isEmpty()) {
        return null;
      }
      Zipper<Metrics, Data> zipper = new Zipper<>();
      zipper.ops = loc.ops;
      zipper.siblings = n.children;
      zipper.metrics = n.childrenMetrics;
      zipper.idx = n.children.size() - 1;
      zipper.parent = loc;
      return zipper;
    }
    return null;
  }

  public static <Metrics, Data> Node<Metrics> root(Zipper<Metrics, Data> loc) {
    Zipper<Metrics, Data> parent = up(loc);
    return parent == null ? node(loc) : root(parent);
  }


  public static boolean hasNext(Zipper loc) {
    if (loc.parent == null && node(loc).children.size() > 0) {
      return true;
    }

    return isBranch(loc) || !isRightmost(loc);
  }

  public static boolean hasPrev(Zipper loc) {
    if (loc.parent == null && node(loc).children.size() > 0) {
      return true;
    }

    return isBranch(loc) || !isLeftmost(loc);
  }

  public static <Metrics, Data> Zipper<Metrics, Data> next(Zipper<Metrics, Data> loc) {
    if (isBranch(loc)) {
      Zipper<Metrics, Data> df = downLeft(loc);
      if (df != null) {
        return df;
      }
    }

    Zipper<Metrics, Data> right = right(loc);
    if (right != null) {
      return right;
    }

    if (isRightmost(loc)) {
      throw new NoSuchElementException();
    }

    Zipper<Metrics, Data> p = loc;
    while (true) {
      Zipper<Metrics, Data> up = up(p);
      if (up == null) {
        Zipper<Metrics, Data> zipper = new Zipper<>();
        zipper.ops = p.ops;
        zipper.idx = 0;
        zipper.isTransient = p.isTransient;
        Node<Metrics> node = node(p);
        zipper.siblings = singletonList(node);
        zipper.metrics = singletonList((Metrics)metrics(node));
        return zipper;
      }
      else {
        Zipper<Metrics, Data> r = right(up);
        if (r != null) {
          return r;
        }
        p = up;
      }
    }
  }

  public static <Metrics, Data> Zipper<Metrics, Data> skip(Zipper<Metrics, Data> loc) {
    Zipper<Metrics, Data> right = right(loc);
    if (right != null) {
      return right;
    }

    if (isRightmost(loc)) {
      throw new NoSuchElementException();
    }

    Zipper<Metrics, Data> p = loc;
    while (true) {
      Zipper<Metrics, Data> up = up(p);
      if (up == null) {
        Zipper<Metrics, Data> zipper = new Zipper<>();
        zipper.ops = p.ops;
        zipper.idx = 0;
        zipper.isTransient = p.isTransient;
        Node<Metrics> node = node(p);
        zipper.siblings = singletonList(node);
        zipper.metrics = singletonList((Metrics)metrics(node));
        return zipper;
      }
      else {
        Zipper<Metrics, Data> r = right(up);
        if (r != null) {
          return r;
        }
        p = up;
      }
    }
  }

  public static Zipper left(Zipper loc) {
    if (0 < loc.idx) {
      Zipper new_loc = new Zipper();
      new_loc.ops = loc.ops;
      new_loc.parent = loc.parent;
      new_loc.siblings = loc.siblings;
      new_loc.metrics = loc.metrics;
      new_loc.isChanged = loc.isChanged;
      new_loc.isTransient = loc.isTransient;
      new_loc.idx = loc.idx - 1;
      new_loc.acc = null; // acc is removed!
      new_loc.oacc = null; // acc is removed!
      return new_loc;
    }
    return null;
  }

  public static Zipper prev(Zipper loc) {
    if (isBranch(loc)) {
      Zipper dr = downRight(loc);
      if (dr != null) {
        return dr;
      }
    }

    Zipper left = left(loc);
    if (left != null) {
      return left;
    }

    if (isLeftmost(loc)) {
      throw new NoSuchElementException();
    }

    Zipper p = loc;
    while (true) {
      Zipper up = up(p);
      if (up != null) {
        Zipper l = left(up);
        if (l != null) {
          return l;
        }
        p = up;
      }
      else {
        Node e = node(loc);
        Zipper zipper = new Zipper();
        zipper.ops = p.ops;
        zipper.idx = 0;
        zipper.isTransient = loc.isTransient;
        zipper.siblings = wrapNode(e, loc.ops);
        zipper.metrics = wrapNode(metrics(e), loc.ops);
        return zipper;
      }
    }
  }

  static Zipper prevLeaf(Zipper loc) {
    do {
      loc = prev(loc);
    }
    while (loc.siblings.get(loc.idx) instanceof Leaf);
    return loc;
  }

  public static <Metrics, Data> Zipper<Metrics, Data> nextLeaf(Zipper<Metrics, Data> loc) {
    do {
      loc = next(loc);
    }
    while (isBranch(loc));
    return loc;
  }

  /*
   * stops in a leaf or in root node if the tree is empty, should never return null
   */
  public static <Metrics, Data> Zipper<Metrics, Data> scan(Zipper<Metrics, Data> loc, BiFunction<Metrics, Metrics, Boolean> pred) {
    while (loc != null) {
      Zipper<Metrics, Data> nextLoc = null;
      if (isRoot(loc)) {
        if (node(loc).children.isEmpty()) {
          return loc;
        }
        nextLoc = loc;
      }
      else {
        Metrics acc = loc.acc == null ? loc.ops.emptyMetrics() : loc.acc;
        int siblingsCount = loc.siblings.size();
        for (int i = loc.idx; i < siblingsCount; i++) {
          if (pred.apply(acc, loc.metrics.get(i))) {
            Zipper<Metrics, Data> zipper = new Zipper<>();
            zipper.ops = loc.ops;
            zipper.siblings = loc.siblings;
            zipper.metrics = loc.metrics;
            zipper.isTransient = loc.isTransient;
            zipper.isChanged = loc.isChanged;
            zipper.parent = loc.parent;
            zipper.oacc = loc.oacc;
            zipper.acc = acc;
            zipper.idx = i;
            nextLoc = zipper;
            break;
          }
          else {
            acc = loc.ops.rf(acc, loc.metrics.get(i));
          }
        }
      }

      if (nextLoc == null) {
        Zipper<Metrics, Data> u = up(loc);
        if (isRightmost(u)) {
          Zipper c = loc.clone();
          c.idx = c.siblings.size() - 1;
          return loc;
        }
        loc = skip(u);
      }
      else {
        if (isBranch(nextLoc)) {
          loc = downLeft(nextLoc);
        }
        else {
          return nextLoc;
        }
      }
    }
    throw new IllegalStateException();
  }

  private static <Metrics, Data> Metrics accumulateTillIdx(Zipper<Metrics, Data> zipper, int idx) {
    Metrics acc = zipper.parent.acc;
    for (int i = 0; i < idx; i++) {
      acc = zipper.ops.rf(acc, zipper.metrics.get(i));
    }
    return acc;
  }

  /*
   * removes current node preserving accumulated position
   *
   * returns zipper pointing to right sibling of the deleted node
   * if the deleted node was rightest sibling zipper skips to `next` node
   * will move to end position of nearest left leaf if there is no `next` node to go
   * if parent node has no more children it will be removed recursively
   * */
  public static <Metrics, Data> Zipper<Metrics, Data> remove(Zipper<Metrics, Data> loc) {
    while (loc.siblings.size() == 1) {
      if (loc.parent == null) {
        return replace(loc, new Node<>(loc.ops.emptyMetrics(), new ArrayList<>(), new ArrayList<>()));
      }
      else {
        loc = up(loc);
      }
    }

    ArrayList<Object> newSiblings;
    ArrayList<Metrics> newMetrics;
    if (isMutable(loc)) {
      newSiblings = loc.siblings;
      newMetrics = loc.metrics;
    }
    else {
      newSiblings = (ArrayList<Object>)loc.siblings.clone();
      newMetrics = (ArrayList<Metrics>)loc.metrics.clone();
    }
    newMetrics.remove(loc.idx);
    newSiblings.remove(loc.idx);


    Zipper<Metrics, Data> zipper = new Zipper<>();
    zipper.ops = loc.ops;
    zipper.siblings = newSiblings;
    zipper.metrics = newMetrics;
    zipper.isTransient = loc.isTransient;
    zipper.isChanged = true;
    zipper.parent = loc.parent;
    zipper.acc = loc.acc;

    if (loc.idx < newSiblings.size()) {
      zipper.idx = loc.idx;
      zipper.oacc = null;
      return zipper;
    }
    else {
      zipper.idx = loc.idx - 1;
      if (isRightmost(zipper)) {
        Metrics acc = zipper.parent.acc;
        for (int i = 0; i < zipper.idx; i++) {
          acc = zipper.ops.rf(acc, zipper.metrics.get(i));
        }
        zipper.acc = accumulateTillIdx(zipper, zipper.idx);
        while (isBranch(zipper)) {
          zipper = downRight(zipper);
          assert zipper != null;
          zipper.acc = accumulateTillIdx(zipper, zipper.idx);
        }
        zipper.oacc = loc.acc;
        return zipper;
      }
      else {
        return skip(zipper);
      }
    }
  }
}