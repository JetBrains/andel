package andel;

import java.util.ArrayList;
import java.util.List;
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

    public boolean isChanged = false;
    public boolean isTransient = false;
    public Object acc;
    public Object oacc;

    public boolean isEnd = false;
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
    ArrayList<Object> l = new ArrayList<>();
    l.add(object);
    return l;
  }

  public static Object currentNode(Zipper loc) {
    return loc.siblings.get(loc.idx);
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

  public static boolean isNode(Object node) {
    return node instanceof Node;
  }

  public static boolean isLeaf(Object leaf) {
    return !isNode(leaf);
  }

  public static boolean isBranch(Zipper loc) {
    return isNode(currentNode(loc));
  }

  public static ArrayList<Object> getChildren(Object node) {
    return ((Node)node).children;
  }

  public static Object getMetrics(Object node) {
    if (isNode(node)) {
      return ((Node)node).metrics;
    }
    else {
      return ((Leaf)node).metrics;
    }
  }

  public static Node makeNode(ArrayList<Object> children, ZipperOps ops) {
    ArrayList<Object> childrenMetrics = new ArrayList<>(children.size());
    for (Object child : children) {
      childrenMetrics.add(getMetrics(child));
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
    int splitThreshold = ops.splitThreshold();
    if (isNode(children.get(0))) {
      for (Object child : children) {
        if (splitThreshold < getChildren(child).size()) {
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

  static ArrayList<ArrayList<Object>> partitionChildren(ArrayList<Object> children, int threshold) {
    int partitionCount = children.size() / threshold;
    int totalPartitions = partitionCount + (children.size() % threshold != 0 ? 1 : 0);

    ArrayList<ArrayList<Object>> partition = new ArrayList<>(totalPartitions);
    int i = 0;
    while (i < children.size()) {
      int capacity = (children.size() - i) % threshold;
      ArrayList<Object> part = new ArrayList<>(capacity);
      part.addAll(children.subList(i, capacity));
      partition.add(part);
      i += capacity;
    }

    return partition;
  }

  static ArrayList<Object> splitChildren(ArrayList<Object> children, ZipperOps ops) {
    if (splitNeeded(children, ops)) {
      ArrayList<Object> result = new ArrayList<>(children.size() / 2);
      for (Object child : children) {
        if (isNode(child) && getChildren(child).size() >= ops.splitThreshold()) {
          ArrayList<ArrayList<Object>> partition = partitionChildren(getChildren(child), ops.splitThreshold());
          for (ArrayList<Object> part : partition) {
            result.add(makeNode(part, ops));
          }
        }
        else if (isLeaf(child) && ops.isLeafOverflown(((Leaf)child).data)) {
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
    if (isNode(children.get(0))) {
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
      if (isNode(children.get(0))) {
        Object left = children.get(0);
        for (int i = 1; i < children.size(); i++) {
          Object right = children.get(i);
          ArrayList<Object> leftChildren = getChildren(left);
          ArrayList<Object> rightChildren = getChildren(right);

          if (leftChildren.size() < mergeThreshold || rightChildren.size() < mergeThreshold) {
            if (leftChildren.size() + rightChildren.size() >= ops.splitThreshold()) {
              int n = (leftChildren.size() + rightChildren.size()) / 2;
              // todo no actual need in tmp array here
              ArrayList<Object> tmp = joinChildren(leftChildren, rightChildren);

              ArrayList<Object> newLeft = new ArrayList<>(n);
              newLeft.addAll(tmp.subList(0, n));
              ArrayList<Object> newRight = new ArrayList<>(tmp.size() - n);
              newRight.addAll(tmp.subList(n, tmp.size() - n));

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
    if (balanced.size() >= ops.splitThreshold()) {
      return growTree(singletonList(makeNode(balanced, ops)), ops);
    }
    else {
      return makeNode(balanced, ops);
    }
  }

  static Node shrinkTree(Node node) {
    ArrayList<Object> children = getChildren(node);
    if (children.size() == 1 && isNode(children.get(0))) {
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
      metricsCopy.set(copy.idx, getMetrics(node));
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
        Node node = shrinkTree(growTree(wrapNode(currentNode(loc), loc.ops), loc.ops));
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

  public static Zipper downForward(Zipper loc) {
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
      zipper.idx = 0;
      zipper.parent = loc;
      return zipper;
    }
    return null;
  }

  public static Zipper downBackward(Zipper loc) {
    if (isBranch(loc)) {
      Node n = (Node)loc.siblings.get(loc.idx);
      if (n.children.isEmpty()){
        return null;
      }
      Zipper zipper = new Zipper();
      zipper.ops = loc.ops;
      zipper.siblings = n.children;
      zipper.metrics = n.childrenMetrics;
      zipper.idx = n.children.size() - 1;
      zipper.parent = loc;
    }
    return null;
  }

  public static Object root(Zipper loc) {
    if (loc.isEnd) {
      return currentNode(loc);
    }

    Zipper parent = up(loc);
    if (parent != null) {
      return root(parent);
    }

    return currentNode(loc);
  }

  public static Zipper next(Zipper loc) {
    if (loc.isEnd) {
      return loc;
    }

    if (isBranch(loc)) {
      Zipper df = downForward(loc);
      if (df != null) {
        return df;
      }
    }

    Zipper right = right(loc);
    if (right != null) {
      return right;
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
        Object node = currentNode(p);
        zipper.siblings = wrapNode(node, p.ops);
        zipper.metrics = wrapNode(getMetrics(node), p.ops);
        zipper.isEnd = true;
        return zipper;
      }
    }
  }

  public static Zipper skip(Zipper loc) {
    if (loc.isEnd) {
      return loc;
    }

    Zipper right = right(loc);
    if (right != null) {
      return right;
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
        Object node = currentNode(p);
        zipper.siblings = wrapNode(node, p.ops);
        zipper.metrics = wrapNode(getMetrics(node), p.ops);
        zipper.isEnd = true;
        return zipper;
      }
    }
  }

  //------ don't use these functions unless you know what you are doing -------


  public static Zipper left(Zipper loc) {
    if (0 < loc.idx) {
      Zipper new_loc = new Zipper();
      new_loc.ops = loc.ops;
      new_loc.parent = loc.parent;
      new_loc.siblings = loc.siblings;
      new_loc.metrics = loc.metrics;
      new_loc.isChanged = loc.isChanged;
      new_loc.isTransient = loc.isTransient;
      new_loc.isEnd = loc.isEnd;
      new_loc.isRoot = loc.isRoot;
      new_loc.idx = loc.idx - 1;
      new_loc.acc = null;
      new_loc.oacc = null;
      return new_loc;
    }
    return null;
  }

  public static Zipper prev(Zipper loc) {
    if (loc.isEnd) {
      return loc;
    }

    if (isBranch(loc)) {
      Zipper df = downBackward(loc);
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

        Object e = currentNode(loc);
        Zipper zipper = new Zipper();
        zipper.ops = loc.ops;
        zipper.idx = 0;
        zipper.isTransient = loc.isTransient;
        zipper.siblings = wrapNode(e, loc.ops);
        zipper.metrics = wrapNode(getMetrics(e), loc.ops);
        zipper.isEnd = true;
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
    return replace(loc, fn.apply(currentNode(loc)));
  }

  public static Zipper nextLeaf(Zipper loc) {
    do {
      loc = next(loc);
    }
    while (!isLeaf(currentNode(loc)) && !loc.isEnd);
    return loc;
  }
  /*
  * stops in a leaf or in the end location, should never return null
  */
  public static Zipper scan(Zipper loc, BiFunction<Object, Object, Boolean> pred) {
    while (loc != null) {
      if (loc.isEnd) {
        return loc;
      }

      Zipper nextLoc = null;
      if (loc.isRoot) {
        nextLoc = loc;
      } else {
        Object acc = loc.acc == null ? loc.ops.emptyMetrics() : loc.acc;
        for (int i = loc.idx; i < loc.siblings.size(); i++) {
          if (pred.apply(acc, loc.metrics.get(i))) {
            Zipper zipper = new Zipper();
            zipper.ops = loc.ops;
            zipper.isEnd = loc.isEnd;
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

      if (nextLoc != null) {
        if (isBranch(nextLoc)) {
          Zipper down = downForward(nextLoc);
          loc = down == null ? skip(nextLoc) : down;
        }
        else {
          return nextLoc;
        }
      }
      else {
        loc = skip(up(loc));
      }
    }
    throw new IllegalStateException();
  }

  /*
   * removes current node preserving accumulated position
   * if the deleted node was rightest sibling zipper skips to next dfs node
   * if parent node has no more children it will be also deleted
   * */
  public static Zipper remove(Zipper loc) {
    while (loc.siblings.size() == 1) {
      if (loc.isRoot) {
        return replace(loc, new Node(loc.ops.emptyMetrics(), new ArrayList<>(), new ArrayList<>()));
      }

      loc = up(loc);
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
    zipper.isEnd = loc.isEnd;
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
      Zipper skip = skip(zipper);
      if (skip.isEnd) {
        Object acc = loc.parent.acc;
        for (int i = 0; i < zipper.idx; i++) {
          acc = loc.ops.rf(acc, loc.metrics.get(i));
        }
        zipper.acc = acc;
        zipper.oacc = loc.acc;
        return zipper;
      } else {
        return skip;
      }
    }
  }
}