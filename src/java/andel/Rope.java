package andel;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.Function;

public class Rope {

  public static class Node {
    public ArrayList<Object> children;
    // store precalculated accumulator values in childrenMetrics to use binary search
    public Object metrics;
    public ArrayList<Object> childrenMetrics;

    public Node(Object metrics, ArrayList<Object> children, ArrayList<Object> childrenMetrics) {
      this.children = children;
      this.metrics = metrics;
      this.childrenMetrics = childrenMetrics;
    }
  }

  public static class Leaf {
    public Object metrics;
    public Object data;
  }

  public interface ZipperOps {

    Object calculateMetrics(Object data);

    Object emptyMetrics();

    Object rf(Object m1, Object m2);

    default Object rf(List<Object> metrics) {
      Object r = emptyMetrics();
      for (Object m : metrics) {
        r = rf(r, m);
      }
      return r;
    }

    boolean isLeafOverflown(Object leafData);

    boolean isLeafUnderflown(Object leafData);

    Object mergeLeaves(Object leafData1, Object leafData22);

    List<Object> splitLeaf(Object leaf);

    int splitThreshold();
  }

  public static class Zipper implements Cloneable {
    public ZipperOps ops;

    public Zipper parent;
    public ArrayList<Object> siblings;
    public ArrayList<Object> metrics;
    public int idx = 0;

    public Object acc;
    public Object oacc;

    public boolean isChanged = false;
    public boolean isTransient = false;
    public boolean isRoot = false; // suspiciously useless flag

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

    public static Zipper zipper(Node node, ZipperOps ops) {
      Zipper z = new Zipper();
      z.parent = null;
      z.ops = ops;
      z.idx = 0;
      z.siblings = singletonList(node);
      z.metrics = singletonList(node.metrics);
      z.acc = ops.emptyMetrics();
      z.oacc = null;
      z.isRoot = true;
      return z;
    }
  }

  private static ArrayList<Object> singletonList(Object object) {
    ArrayList<Object> l = new ArrayList<>(1);
    l.add(object);
    return l;
  }

  public static Node node(Zipper loc) {
    return (Node)loc.siblings.get(loc.idx);
  }

  public static Leaf leaf(Zipper loc) {
    return (Leaf)loc.siblings.get(loc.idx);
  }

  public static Object currentAcc(Zipper loc) {
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

  public static Node makeNode(ArrayList<Object> children, ZipperOps ops) {
    ArrayList<Object> childrenMetrics = new ArrayList<>(children.size());
    for (Object child : children) {
      childrenMetrics.add(metrics(child));
    }
    return new Node(ops.rf(childrenMetrics), children, childrenMetrics);
  }

  public static Leaf makeLeaf(Object data, ZipperOps ops) {
    Leaf l = new Leaf();
    l.data = data;
    l.metrics = ops.calculateMetrics(data);
    return l;
  }

  static boolean splitNeeded(ArrayList<Object> children, ZipperOps ops) {
    if (children.get(0) instanceof Node) {
      for (Object child : children) {
        if (getChildren(child).size() > ops.splitThreshold()) {
          return true;
        }
      }
    }
    else {
      for (Object child : children) {
        if (ops.isLeafOverflown(((Leaf)child).data)) {
          return true;
        }
      }
    }
    return false;
  }

  private static void partitionList(ArrayList<ArrayList<Object>> result, ArrayList<Object> source, int from, int to, int thresh) {
    int length = to - from;
    if (length <= thresh) {
      ArrayList<Object> part = new ArrayList<Object>(thresh);
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

  static ArrayList<Object> splitChildren(ArrayList<Object> children, ZipperOps ops) {
    if (splitNeeded(children, ops)) {
      ArrayList<Object> result = new ArrayList<>(children.size() / 2);
      for (Object child : children) {
        if (child instanceof Node && getChildren(child).size() > ops.splitThreshold()) {
          ArrayList<ArrayList<Object>> partition = partitionChildren(getChildren(child), ops.splitThreshold());
          for (ArrayList<Object> part : partition) {
            result.add(makeNode(part, ops));
          }
        }
        else if (child instanceof Leaf && ops.isLeafOverflown(((Leaf)child).data)) {
          for (Object o : ops.splitLeaf(((Leaf)child).data)) {
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

  static boolean isMergeNeeded(ArrayList<Object> children, ZipperOps ops) {
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
        if (ops.isLeafUnderflown(((Leaf)child).data)) {
          return true;
        }
      }
    }
    return false;
  }

  static ArrayList<Object> mergeChildren(ArrayList<Object> children, ZipperOps ops) {
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
        Object leftData = ((Leaf)children.get(0)).data;

        for (int i = 1; i < children.size(); i++) {
          Object rightData = ((Leaf)children.get(i)).data;
          if (ops.isLeafUnderflown(leftData) || ops.isLeafUnderflown(rightData)) {
            Object mergedData = ops.mergeLeaves(leftData, rightData);
            if (ops.isLeafOverflown(mergedData)) {
              List<Object> split = ops.splitLeaf(mergedData);
              Object dataLeft = split.get(0);
              Object dataRight = split.get(1);
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

  static ArrayList<Object> balanceChildren(ArrayList<Object> children, ZipperOps ops) {
    return mergeChildren(splitChildren(children, ops), ops);
  }

  public static Node growTree(ArrayList<Object> children, ZipperOps ops) {
    ArrayList<Object> balanced = balanceChildren(children, ops);
    if (balanced.size() > ops.splitThreshold()) {
      return growTree(singletonList(makeNode(balanced, ops)), ops);
    }
    else {
      return makeNode(balanced, ops);
    }
  }

  static Node shrinkTree(Node node) {
    ArrayList<Object> children = getChildren(node);
    if (children.size() == 1 && children.get(0) instanceof Node) {
      return shrinkTree((Node)children.get(0));
    }
    return node;
  }

  static boolean isMutable(Zipper loc) {
    return loc.isTransient && loc.isChanged;
  }

  public static Zipper replace(Zipper loc, Object node) {
    Zipper copy = loc.clone();
    copy.isChanged = true;
    if (isMutable(copy)) {
      copy.siblings.set(copy.idx, node);
    }
    else {
      ArrayList<Object> siblingsCopy = (ArrayList<Object>)copy.siblings.clone();
      ArrayList<Object> metricsCopy = (ArrayList<Object>)copy.metrics.clone();
      siblingsCopy.set(copy.idx, node);
      metricsCopy.set(copy.idx, metrics(node));
      copy.siblings = siblingsCopy;
      copy.metrics = metricsCopy;
    }
    return copy;
  }

  public static ArrayList<Object> wrapNode(Object node, ZipperOps ops) {
    ArrayList<Object> al = new ArrayList<>(ops.splitThreshold() / 2);
    al.add(node);
    return al;
  }

  public static Zipper up(Zipper loc) {
    if (loc.isChanged) {
      if (loc.parent == null) {
        Zipper zipper = new Zipper();
        zipper.ops = loc.ops;
        zipper.isTransient = loc.isTransient;
        zipper.idx = 0;
        Node node = shrinkTree(growTree(wrapNode(node(loc), loc.ops), loc.ops));
        zipper.siblings = wrapNode(node, loc.ops);
        zipper.metrics = wrapNode(node.metrics, loc.ops);
        zipper.isRoot = true;
        return zipper;
      }
      else {
        return replace(loc.parent, makeNode(balanceChildren(loc.siblings, loc.ops), loc.ops));
      }
    }
    return loc.parent;
  }

  /*
  * moves to next direct sibling and adds metrics of current node to accumulator
  * returns null if it is last child
  * */
  public static Zipper right(Zipper loc) {
    if (loc.idx < loc.siblings.size() - 1) {
      Zipper zipper = new Zipper();
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
  public static Zipper downLeft(Zipper loc) {
    if (isBranch(loc)) {
      Node n = (Node)loc.siblings.get(loc.idx);
      if (n.children.isEmpty()) {
        return null;
      }
      Zipper zipper = new Zipper();
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
  public static Zipper downRight(Zipper loc) {
    if (isBranch(loc)) {
      Node n = (Node)loc.siblings.get(loc.idx);
      if (n.children.isEmpty()) {
        return null;
      }
      Zipper zipper = new Zipper();
      zipper.ops = loc.ops;
      zipper.siblings = n.children;
      zipper.metrics = n.childrenMetrics;
      zipper.idx = n.children.size() - 1;
      zipper.parent = loc;
      return zipper;
    }
    return null;
  }

  public static Node root(Zipper loc) {
    Zipper parent = up(loc);
    return parent == null ? node(loc) : root(parent);
  }


  public static boolean hasNext(Zipper loc) {
    if (loc.parent == null && node(loc).children.size() > 0) {
      return true;
    }

    return isBranch(loc) || !isRightmost(loc);
  }

  public static Zipper next(Zipper loc) {
    if (isBranch(loc)) {
      Zipper df = downLeft(loc);
      if (df != null) {
        return df;
      }
    }

    Zipper right = right(loc);
    if (right != null) {
      return right;
    }

    if (isRightmost(loc)) {
      throw new NoSuchElementException();
    }

    Zipper p = loc;
    while (true) {
      Zipper up = up(p);
      if (up != null) {
        Zipper r = right(up);
        if (r != null) {
          return r;
        }
        p = up;
      }
      else {
        Zipper zipper = new Zipper();
        zipper.ops = p.ops;
        zipper.idx = 0;
        zipper.isTransient = p.isTransient;
        Node node = node(p);
        zipper.siblings = wrapNode(node, p.ops);
        zipper.metrics = wrapNode(metrics(node), p.ops);
        return zipper;
      }
    }
  }

  public static Zipper skip(Zipper loc) {
    Zipper right = right(loc);
    if (right != null) {
      return right;
    }

    if (isRightmost(loc)) {
      throw new NoSuchElementException();
    }

    Zipper p = loc;
    while (true) {
      Zipper up = up(p);
      if (up != null) {
        Zipper r = right(up);
        if (r != null) {
          return r;
        }
        p = up;
      }
      else {
        Zipper zipper = new Zipper();
        zipper.ops = p.ops;
        zipper.idx = 0;
        zipper.isTransient = p.isTransient;
        Node node = node(p);
        zipper.siblings = wrapNode(node, p.ops);
        zipper.metrics = wrapNode(metrics(node), p.ops);
        return zipper;
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
      new_loc.isRoot = loc.isRoot;
      new_loc.idx = loc.idx - 1;
      new_loc.acc = null;
      new_loc.oacc = null;
      return new_loc;
    }
    return null;
  }

  public static Zipper prev(Zipper loc) {
    //if (loc.isEnd) {
    //  return loc;
    //}

    if (isBranch(loc)) {
      Zipper df = downRight(loc);
      if (df != null) {
        return df;
      }
    }

    Zipper left = left(loc);
    if (left != null) {
      return left;
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
        zipper.ops = loc.ops;
        zipper.idx = 0;
        zipper.isTransient = loc.isTransient;
        zipper.siblings = wrapNode(e, loc.ops);
        zipper.metrics = wrapNode(metrics(e), loc.ops);
        return zipper;
      }
    }
  }

  static Zipper prevLeaf(Zipper loc) {
    // todo fix
    return null;
  }

  //---------------------------------------------------------------------------

  public static Zipper edit(Zipper loc, Function<Object, Object> fn) {
    // todo maybe Function<TData, TData> is a better fit
    return replace(loc, fn.apply(loc.siblings.get(loc.idx)));
  }

  public static Zipper nextLeaf(Zipper loc) {
    do {
      loc = next(loc);
    }
    while (isBranch(loc));
    return loc;
  }

  /*
   * stops in a leaf or in root node if the tree is empty, should never return null
   */
  public static Zipper scan(Zipper loc, BiFunction<Object, Object, Boolean> pred) {
    while (loc != null) {
      Zipper nextLoc = null;
      if (loc.isRoot) {
        if (node(loc).children.isEmpty()) {
          return loc;
        }
        nextLoc = loc;
      }
      else {
        Object acc = loc.acc == null ? loc.ops.emptyMetrics() : loc.acc;
        int siblingsCount = loc.siblings.size();
        for (int i = loc.idx; i < siblingsCount; i++) {
          if (pred.apply(acc, loc.metrics.get(i))) {
            Zipper zipper = new Zipper();
            zipper.ops = loc.ops;
            zipper.siblings = loc.siblings;
            zipper.metrics = loc.metrics;
            zipper.isTransient = loc.isTransient;
            zipper.isChanged = loc.isChanged;
            zipper.parent = loc.parent;
            zipper.isRoot = loc.isRoot;
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
        Zipper u = up(loc);
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

  private static Object accumulateTillIdx(Zipper zipper, int idx) {
    Object acc = zipper.parent.acc;
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
  public static Zipper remove(Zipper loc) {
    while (loc.siblings.size() == 1) {
      if (loc.parent == null) {
        return replace(loc, new Node(loc.ops.emptyMetrics(), new ArrayList<>(), new ArrayList<>()));
      }
      else {
        loc = up(loc);
      }
    }

    ArrayList<Object> newSiblings;
    ArrayList<Object> newMetrics;
    if (isMutable(loc)) {
      newSiblings = loc.siblings;
      newMetrics = loc.metrics;
    }
    else {
      newSiblings = (ArrayList<Object>)loc.siblings.clone();
      newMetrics = (ArrayList<Object>)loc.metrics.clone();
    }
    newMetrics.remove(loc.idx);
    newSiblings.remove(loc.idx);


    Zipper zipper = new Zipper();
    zipper.ops = loc.ops;
    zipper.siblings = newSiblings;
    zipper.metrics = newMetrics;
    zipper.isTransient = loc.isTransient;
    zipper.isChanged = true;
    zipper.parent = loc.parent;
    zipper.isRoot = loc.isRoot;
    zipper.acc = loc.acc;

    if (loc.idx < newSiblings.size()) {
      zipper.idx = loc.idx;
      zipper.oacc = null;
      return zipper;
    }
    else {
      zipper.idx = loc.idx - 1;
      if (isRightmost(zipper)) {
        Object acc = zipper.parent.acc;
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