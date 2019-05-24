package andel;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.BiFunction;

public class Rope {

  public static class Node<Metrics> {
    public ArrayList<Object> children;
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

  public static class Zipper<Metrics, Data> {
    public ZipperOps<Metrics, Data> ops;

    public Zipper<Metrics, Data> parent;

    //TODO why not use arrays?
    public ArrayList<Object> siblings;
    public ArrayList<Metrics> metrics;
    public int idx = 0;

    public Metrics acc;
    public Metrics oacc;

    public boolean isChanged = false;
    public boolean isTransient = false;

    public Zipper() { }

    public void copyTo(Zipper<Metrics, Data> that) {
      that.ops = this.ops;
      that.parent = this.parent;
      that.siblings = this.siblings;
      that.metrics = this.metrics;
      that.idx = this.idx;
      that.acc = this.acc;
      that.oacc = this.oacc;
      that.isChanged = this.isChanged;
      that.isTransient = this.isTransient;
    }

    public Zipper<Metrics, Data> copy() {
      Zipper<Metrics, Data> zipper = new Zipper<>();
      this.copyTo(zipper);
      return zipper;
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

  public static <T> ArrayList<T> singletonList(T object) {
    ArrayList<T> l = new ArrayList<>(1);
    l.add(object);
    return l;
  }

  public static <Metrics, Data> Node<Metrics> node(Zipper<Metrics, Data> loc) {
    //noinspection unchecked
    return (Node<Metrics>)loc.siblings.get(loc.idx);
  }

  public static <Metrics, Data> Leaf<Metrics, Data> leaf(Zipper<Metrics, Data> loc) {
    //noinspection unchecked
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

  public static boolean isLeaf(Zipper loc) {
    return loc.siblings.get(loc.idx) instanceof Leaf;
  }

  public static boolean isRoot(Zipper loc) {
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

  static boolean isChildrenMutable(Zipper loc) {
    return loc.isTransient && loc.isChanged;
  }

  public static <Metrics, Data> Zipper<Metrics, Data> toTransient(Zipper<Metrics, Data> zipper) {
    if (zipper.isTransient) {
      return zipper;
    }
    else {
      Zipper<Metrics, Data> copy = zipper.copy();
      copy.isTransient = true;
      return copy;
    }
  }

  public static <Metrics, Data> Zipper<Metrics, Data> toPersistent(Zipper<Metrics, Data> zipper) {
    zipper.isTransient = false;
    return zipper;
  }

  public static <Metrics, Data> Zipper<Metrics, Data> replace(Zipper<Metrics, Data> zipper, Object node) {
    ArrayList<Object> siblings = isChildrenMutable(zipper) ? zipper.siblings : (ArrayList<Object>)zipper.siblings.clone();
    ArrayList<Metrics> metrics = isChildrenMutable(zipper) ? zipper.metrics : (ArrayList<Metrics>)zipper.metrics.clone();
    siblings.set(zipper.idx, node);
    metrics.set(zipper.idx, (Metrics)metrics(node));

    Zipper<Metrics, Data> result = zipper.isTransient ? zipper : zipper.copy();
    result.isChanged = true;
    result.siblings = siblings;
    result.metrics = metrics;
    return result;
  }

  public static <Metrics, Data> Zipper<Metrics, Data> up(Zipper<Metrics, Data> loc) {
    if (loc.isChanged) {
      if (loc.parent == null) {
        Zipper<Metrics, Data> result = loc.isTransient ? loc : loc.copy();
        result.ops = loc.ops;
        result.idx = 0;
        result.acc = null;
        result.oacc = null;
        result.isChanged = false;  // this way we break recursion in root
        //TODO move tree growth code to root?
        Node<Metrics> node = shrinkTree(growTree(singletonList(node(loc)), loc.ops));
        result.siblings = singletonList(node);
        result.metrics = singletonList(node.metrics);
        return result;
      }
      else {
        // TODO can reuse metrics from loc.metrics if children are balanced
        return replace(loc.parent, makeNode(balanceChildren(loc.siblings, loc.ops), loc.ops));
      }
    }
    else {
      if (loc.parent != null) {
        if (loc.isTransient && !loc.parent.isTransient) {
          return toTransient(loc.parent);
        }
        //TODO can avoid allocation
        //result.copyTo(input);
        //input.isTransient = true;
        //return input;

        if (!loc.isTransient && loc.parent.isTransient) {
          return toPersistent(loc.parent);
        }
      }
      return loc.parent;
    }
  }

  public static boolean hasRight(Zipper zipper) {
    return zipper.idx < zipper.siblings.size() - 1;
  }

  /*
   * moves to next direct sibling and adds metrics of current node to accumulator
   * returns null if it is last child
   * */
  public static <Metrics, Data> Zipper<Metrics, Data> right(Zipper<Metrics, Data> zipper) {
    if (zipper.idx < zipper.siblings.size() - 1) {
      Zipper<Metrics, Data> result = zipper.isTransient ? zipper : zipper.copy();
      result.acc = zipper.acc == null
                   ? zipper.metrics.get(zipper.idx)
                   : zipper.ops.rf(zipper.acc, zipper.metrics.get(zipper.idx));
      result.idx = zipper.idx + 1;
      result.oacc = null;
      return result;
    }
    return null;
  }

  /*
   * moves to first child of current node
   * returns null for leaves and empty nodes
   * */
  public static <Metrics, Data> Zipper<Metrics, Data> downLeft(Zipper<Metrics, Data> zipper) {
    if (isBranch(zipper)) {
      Node<Metrics> n = node(zipper);
      if (n.children.isEmpty()) {
        return null;
      }
      Zipper<Metrics, Data> result = new Zipper<>();
      result.ops = zipper.ops;
      result.siblings = n.children;
      result.metrics = n.childrenMetrics;
      result.acc = zipper.acc;
      result.oacc = null;
      result.idx = 0;
      result.parent = zipper;
      result.isTransient = zipper.isTransient;
      return result;
    }
    return null;
  }

  /*
   * moves to last child of current node
   * returns null for leaves and empty nodes
   *
   * CAUTION: drops accumulated position
   * */
  public static <Metrics, Data> Zipper<Metrics, Data> downRight(Zipper<Metrics, Data> zipper) {
    if (isBranch(zipper)) {
      Node<Metrics> n = node(zipper);
      if (n.children.isEmpty()) {
        return null;
      }
      Zipper<Metrics, Data> result = new Zipper<>();
      result.ops = zipper.ops;
      result.siblings = n.children;
      result.metrics = n.childrenMetrics;
      result.idx = n.children.size() - 1;
      result.acc = null;
      result.oacc = null;
      result.parent = zipper;
      result.isTransient = zipper.isTransient;
      return result;
    }
    return null;
  }

  public static <Metrics, Data> Node<Metrics> root(Zipper<Metrics, Data> loc) {
    Zipper<Metrics, Data> parent = up(loc);
    return parent == null ? node(loc) : root(parent);
  }


  public static <Metrics, Data> boolean hasNext(Zipper<Metrics, Data> loc) {
    if (loc.parent == null && node(loc).children.size() > 0) {
      return true;
    }

    return isBranch(loc) || !isRightmost(loc);
  }

  public static <Metrics, Data> boolean hasPrev(Zipper<Metrics, Data> loc) {
    if (loc.parent == null && node(loc).children.size() > 0) {
      return true;
    }

    return isBranch(loc) || !isLeftmost(loc);
  }

  public static <Metrics, Data> Zipper<Metrics, Data> next(Zipper<Metrics, Data> zipper) {
    assert hasNext(zipper);
    if (isBranch(zipper)) {
      return downLeft(zipper);
    }

    if (isRightmost(zipper)) {
      throw new NoSuchElementException();
    }

    boolean isTransient = zipper.isTransient;
    Zipper<Metrics, Data> p = toTransient(zipper);
    while (true) {
      Zipper<Metrics, Data> r = right(p);
      if (r != null) {
        return isTransient ? r : toPersistent(r);
      }
      else {
        p = up(p);
      }
    }
  }

  public static <Metrics, Data> Zipper<Metrics, Data> skip(Zipper<Metrics, Data> zipper) {
    assert hasNext(zipper);
    if (isRightmost(zipper)) {
      throw new NoSuchElementException();
    }

    boolean isTransient = zipper.isTransient;
    Zipper<Metrics, Data> p = toTransient(zipper);
    while (true) {
      Zipper<Metrics, Data> r = right(p);
      if (r != null) {
        return isTransient ? r : toPersistent(r);
      }
      else {
        p = up(p);
      }
    }
  }

  public static <Metrics, Data> Zipper<Metrics, Data> left(Zipper<Metrics, Data> zipper) {
    if (0 < zipper.idx) {
      Zipper<Metrics, Data> result = zipper.isTransient ? zipper : zipper.copy();
      result.idx = zipper.idx - 1;
      result.acc = null; // acc is removed!
      result.oacc = null; // acc is removed!
      return result;
    }
    return null;
  }

  public static <Metrics, Data> Zipper<Metrics, Data> prev(Zipper<Metrics, Data> zipper) {
    if (isBranch(zipper)) {
      Zipper<Metrics, Data> dr = downRight(zipper);
      if (dr != null) {
        return dr;
      }
    }

    if (isLeftmost(zipper)) {
      throw new NoSuchElementException();
    }

    boolean isTransient = zipper.isTransient;
    Zipper<Metrics, Data> p = toTransient(zipper);
    while (true) {
      Zipper<Metrics, Data> l = left(p);
      if (l != null) {
        return isTransient ? l : toPersistent(l);
      }
      p = up(p);
    }
  }

  static <Metrics, Data> Zipper<Metrics, Data> prevLeaf(Zipper<Metrics, Data> zipper) {
    boolean isTransient = zipper.isTransient;
    zipper = toTransient(zipper);
    do {
      zipper = prev(zipper);
    }
    while (isBranch(zipper));
    return isTransient ? zipper : toPersistent(zipper);
  }

  public static <Metrics, Data> Zipper<Metrics, Data> nextLeaf(Zipper<Metrics, Data> zipper) {
    boolean isTransient = zipper.isTransient;
    zipper = toTransient(zipper);
    do {
      zipper = next(zipper);
    }
    while (isBranch(zipper));
    return isTransient ? zipper : toPersistent(zipper);
  }

  /*
   * stops in a leaf or in root node if the tree is empty, should never return null
   */
  public static <Metrics, Data> Zipper<Metrics, Data> scan(Zipper<Metrics, Data> zipper, BiFunction<Metrics, Metrics, Boolean> pred) {
    // TODO return null even if it is empty root
    if (isRoot(zipper) && node(zipper).children.isEmpty()) {
      return zipper;
    }
    // fast path
    if (isLeaf(zipper) && pred.apply(zipper.acc == null ? zipper.ops.emptyMetrics() : zipper.acc,
                                     zipper.metrics.get(zipper.idx))) {
      return zipper;
    }

    boolean isTransient = zipper.isTransient;
    zipper = toTransient(zipper);
    while (true) {

      Zipper<Metrics, Data> found = null;
      Metrics acc = zipper.acc == null ? zipper.ops.emptyMetrics() : zipper.acc;
      int siblingsCount = zipper.siblings.size();

      for (int i = zipper.idx; i < siblingsCount; ++i) {
        if (pred.apply(acc, zipper.metrics.get(i))){
          zipper.idx = i;
          zipper.acc = acc;
          found = zipper;
          break;
        } else {
          acc = zipper.ops.rf(acc, zipper.metrics.get(i));
        }
      }

      if (found == null) {
        if (isRightmost(zipper)){
          return null;
        } else {
          zipper = skip(zipper);
        }
      }
      else {
        if (isBranch(found)){
          zipper = downLeft(found);
        } else {
          return isTransient ? found : toPersistent(found);
        }
      }
    }
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
  public static <Metrics, Data> Zipper<Metrics, Data> remove(Zipper<Metrics, Data> zipper) {
    while (zipper.siblings.size() == 1) {
      if (isRoot(zipper)) {
        return replace(zipper, new Node<>(zipper.ops.emptyMetrics(), new ArrayList<>(), new ArrayList<>()));
      }
      else {
        zipper = up(zipper);
      }
    }

    ArrayList<Object> newSiblings;
    ArrayList<Metrics> newMetrics;
    if (isChildrenMutable(zipper)) {
      newSiblings = zipper.siblings;
      newMetrics = zipper.metrics;
    }
    else {
      newSiblings = (ArrayList<Object>)zipper.siblings.clone();
      newMetrics = (ArrayList<Metrics>)zipper.metrics.clone();
    }
    newMetrics.remove(zipper.idx);
    newSiblings.remove(zipper.idx);

    boolean isTransient = zipper.isTransient;
    Metrics initialAcc = zipper.acc;
    int initialIdx = zipper.idx;
    Zipper<Metrics, Data> result = isTransient ? zipper : zipper.copy();
    result.siblings = newSiblings;
    result.metrics = newMetrics;
    result.isChanged = true;

    if (zipper.idx < newSiblings.size()) {
      result.idx = initialIdx;
      result.oacc = null;
      return result;
    }
    else {
      result.isTransient = true;
      result.idx = initialIdx - 1;
      if (isRightmost(result)) {
        Metrics acc = result.parent.acc;
        for (int i = 0; i < result.idx; i++) {
          acc = result.ops.rf(acc, result.metrics.get(i));
        }
        result.acc = accumulateTillIdx(result, result.idx);
        while (isBranch(result)) {
          result = downRight(result);
          assert result != null;
          result.acc = accumulateTillIdx(result, result.idx);
        }
        result.oacc = initialAcc;
        return isTransient ? result : toPersistent(result);
      }
      else {
        Zipper<Metrics,Data> s = skip(result);
        return isTransient ? s : toPersistent(s);
      }
    }
  }
}
