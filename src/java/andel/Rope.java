package andel;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.BiFunction;

public class Rope {

  public static class Tree<Metrics> {
    public Tree(Node<Metrics> root, Metrics metrics){
      this.root = root;
      this.metrics = metrics;
    }

    public final Node<Metrics> root;
    public final Metrics metrics;
  }

  public static class Node<Metrics> {
    public ArrayList<Object> children;
    public ArrayList<Metrics> metrics;

    public Node(ArrayList<Object> children, ArrayList<Metrics> childrenMetrics) {
      this.children = children;
      this.metrics = childrenMetrics;
    }
  }

  public interface ZipperOps<Metrics, Data> {

    Metrics calculateMetrics(Data data);

    Metrics emptyMetrics();

    int splitThreshold();

    Metrics rf(Metrics m1, Metrics m2);

    default Metrics rf(List<Metrics> metrics) {
      Metrics r = emptyMetrics();
      for (Metrics m : metrics) {
        r = rf(r, m);
      }
      return r;
    }

    default boolean isLeafOverflown(Data leafData){
      return false;
    }

    default boolean isLeafUnderflown(Data leafData){
      return false;
    }

    default Data mergeLeaves(Data leafData1, Data leafData2){
      throw new UnsupportedOperationException();
    }

    default List<Data> splitLeaf(Data leaf){
      throw new UnsupportedOperationException();
    }
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

    public static <Metrics, Data> Zipper<Metrics, Data> zipper(Tree<Metrics> tree, ZipperOps<Metrics, Data> ops) {
      Zipper<Metrics, Data> z = new Zipper<>();
      z.parent = null;
      z.ops = ops;
      z.idx = 0;
      z.siblings = singletonList(tree.root);
      z.metrics = singletonList(tree.metrics);
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

  public static <Metrics, Data> Data data(Zipper<Metrics, Data> loc) {
    //noinspection unchecked
    return (Data)loc.siblings.get(loc.idx);
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
    return !isBranch(loc);
  }

  public static boolean isRoot(Zipper loc) {
    return loc.parent == null;
  }

  public static ArrayList<Object> getChildren(Object node) {
    return ((Node)node).children;
  }

  public static <Metrics, Data> Metrics metrics(Zipper<Metrics, Data> zipper) {
    return zipper.metrics.get(zipper.idx);
  }

  public static boolean isRightmost(Zipper location) {
    return location.idx == location.siblings.size() - 1 && (location.parent == null || isRightmost(location.parent));
  }

  public static boolean isLeftmost(Zipper location) {
    return location.idx == 0 && (location.parent == null || isLeftmost(location.parent));
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
        if (ops.isLeafOverflown((Data)child)) {
          return true;
        }
      }
    }
    return false;
  }

  private static <Metrics> void splitNode(ArrayList<Node<Metrics>> result, Node<Metrics> source, int from, int to, int thresh) {
    int length = to - from;
    if (length <= thresh) {
      result.add(new Node<>(new ArrayList<>(source.children.subList(from, to)),
                            new ArrayList<>(source.metrics.subList(from, to))));
    }
    else {
      int half = length / 2;
      splitNode(result, source, from, from + half, thresh);
      splitNode(result, source, from + half, to, thresh);
    }
  }

  static <Metrics> ArrayList<Node<Metrics>> splitNode(Node<Metrics> node, int threshold) {
    ArrayList<Node<Metrics>> result = new ArrayList<>();
    splitNode(result, node, 0, node.children.size(), threshold);
    return result;
  }

  /*
  * ensures every child of this node won't be wider than split threshold (still might be underflown),
  * but node itself might become overflown
  * */
  static <Metrics, Data> Node<Metrics> splitChildren(Node<Metrics> node, ZipperOps<Metrics, Data> ops) {
    ArrayList<Metrics> metrics = node.metrics;
    ArrayList<Object> children = node.children;
    if (splitNeeded(children, ops)) {
      ArrayList<Object> newChildren = new ArrayList<>(children.size());
      ArrayList<Metrics> newMetrics = new ArrayList<>(children.size());
      if (children.get(0) instanceof Node){
        for (int i = 0; i < children.size(); i++) {
          Node<Metrics> child = (Node<Metrics>)children.get(i);
          if (child.children.size() > ops.splitThreshold()) {
            ArrayList<Node<Metrics>> partition = splitNode(child, ops.splitThreshold());
            for (Node<Metrics> part : partition) {
              newChildren.add(part);
              newMetrics.add(ops.rf(part.metrics));
            }
          }
          else {
            newChildren.add(child);
            newMetrics.add(metrics.get(i));
          }
        }
      } else {
        for (int i = 0; i < children.size(); i++) {
          Data child = (Data)children.get(i);
          if (ops.isLeafOverflown(child)) {
            for (Data o : ops.splitLeaf(child)) {
              newChildren.add(o);
              newMetrics.add(ops.calculateMetrics(o));
            }
          }
          else {
            newChildren.add(child);
            newMetrics.add(metrics.get(i));
          }
        }
      }
      return new Node<>(newChildren, newMetrics);
    }
    return node;
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
        if (ops.isLeafUnderflown((Data)child)) {
          return true;
        }
      }
    }
    return false;
  }



  static <Metrics, Data> Node<Metrics> mergeChildren(Node<Metrics> node, ZipperOps<Metrics, Data> ops) {
    final int mergeThreshold = ops.splitThreshold() / 2;
    if (isMergeNeeded(node.children, ops)) {
      ArrayList<Object> newChildren = new ArrayList<>(mergeThreshold);
      ArrayList<Metrics> newMetrics = new ArrayList<>(mergeThreshold);
      if (node.children.get(0) instanceof Node) {
        Node<Metrics> left = (Node<Metrics>)node.children.get(0);
        Metrics leftMetrics = node.metrics.get(0);
        for (int i = 1; i < node.children.size(); i++) {
          Node<Metrics> right = (Node<Metrics>)node.children.get(i);
          Metrics rightMetrics = node.metrics.get(i);
          if (left.children.size() < mergeThreshold || right.children.size() < mergeThreshold) {
            if (left.children.size() + right.children.size() > ops.splitThreshold()) {
              int n = (left.children.size() + right.children.size());
              int halfn = n / 2;
              ArrayList<Object> newLeft = new ArrayList<>(halfn);
              ArrayList<Metrics> newLeftMetrics = new ArrayList<>(halfn);
              ArrayList<Object> newRight = new ArrayList<>(n - halfn);
              ArrayList<Metrics> newRightMetrics = new ArrayList<>(n - halfn);
              int leftCesure = Math.min(halfn, left.children.size());
              int rightCesure = Math.max(0, halfn - left.children.size());

              newLeft.addAll(left.children.subList(0, leftCesure));
              newLeft.addAll(right.children.subList(0, rightCesure));
              newLeftMetrics.addAll(left.metrics.subList(0, leftCesure));
              newLeftMetrics.addAll(right.metrics.subList(0, rightCesure));

              newRight.addAll(left.children.subList(leftCesure, left.children.size()));
              newRight.addAll(right.children.subList(rightCesure, right.children.size()));
              newRightMetrics.addAll(left.metrics.subList(leftCesure, left.children.size()));
              newRightMetrics.addAll(right.metrics.subList(rightCesure, right.children.size()));

              Node<Metrics> mergedLeft = mergeChildren(new Node<>(newLeft, newLeftMetrics), ops);
              newChildren.add(mergedLeft);
              newMetrics.add(ops.rf(mergedLeft.metrics));

              left = new Node<>(newRight, newRightMetrics);
              leftMetrics = ops.rf(newRightMetrics);
            }
            else {
              left = mergeChildren(new Node<>(join(left.children, right.children), join(left.metrics, right.metrics)), ops);
              leftMetrics = ops.rf(left.metrics);
            }
          }
          else {
            newChildren.add(left);
            newMetrics.add(leftMetrics);
            left = right;
            leftMetrics = rightMetrics;
          }
        }
        newChildren.add(left);
        newMetrics.add(leftMetrics);
      }
      else {
        Data leftData = (Data)node.children.get(0);
        Metrics leftMetrics = node.metrics.get(0);
        for (int i = 1; i < node.children.size(); i++) {
          Data rightData = (Data)node.children.get(i);
          Metrics rightMetrics = node.metrics.get(i);
          if (ops.isLeafUnderflown(leftData) || ops.isLeafUnderflown(rightData)) {
            Data mergedData = ops.mergeLeaves(leftData, rightData);
            if (ops.isLeafOverflown(mergedData)) {
              List<Data> split = ops.splitLeaf(mergedData);
              Data dataLeft = split.get(0);
              Data dataRight = split.get(1);
              newChildren.add(dataLeft);
              newMetrics.add(ops.calculateMetrics(dataLeft));
              leftData = dataRight;
              leftMetrics = ops.calculateMetrics(dataRight);
            }
            else {
              leftData = mergedData;
              leftMetrics = ops.calculateMetrics(mergedData);
            }
          }
          else {
            newChildren.add(leftData);
            newMetrics.add(leftMetrics);
            leftData = rightData;
            leftMetrics = rightMetrics;
          }
        }
        newChildren.add(leftData);
        newMetrics.add(leftMetrics);
      }
      return new Node<>(newChildren, newMetrics);
    }
    return node;
  }

  static <T> ArrayList<T> join(ArrayList<T> left, ArrayList<T> right) {
    ArrayList<T> tmp = new ArrayList<>(left.size() + right.size());
    tmp.addAll(left);
    tmp.addAll(right);
    return tmp;
  }

  static <Metrics, Data> Node<Metrics> balanceChildren(Node<Metrics> node, ZipperOps<Metrics, Data> ops) {
    if (node.children.isEmpty()){
      return node;
    }
    return mergeChildren(splitChildren(node, ops), ops);
  }

  public static <Metrics, Data> Node<Metrics> growTree(Node<Metrics> node, ZipperOps<Metrics, Data> ops) {
    Node<Metrics> balanced = balanceChildren(node, ops);
    if (balanced.children.size() > ops.splitThreshold()) {
      return growTree(new Node<>(singletonList(balanced),
                                 singletonList(ops.rf(balanced.metrics))), ops);
    }
    else {
      return balanced;
    }
  }

  static <Metrics> Node<Metrics> shrinkTree(Node<Metrics> node) {
    ArrayList<Object> children = node.children;
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
    Zipper<Metrics, Data> z = zipper;
    while (z != null && z.isTransient){
      z.isTransient = false;
      z = z.parent;
    }
    return zipper;
  }

  public static <Metrics, Data> Zipper<Metrics, Data> replace(Zipper<Metrics, Data> zipper, Object node, Metrics nodeMetrics) {
    ArrayList<Object> siblings = isChildrenMutable(zipper) ? zipper.siblings : (ArrayList<Object>)zipper.siblings.clone();
    ArrayList<Metrics> metrics = isChildrenMutable(zipper) ? zipper.metrics : (ArrayList<Metrics>)zipper.metrics.clone();
    siblings.set(zipper.idx, node);
    metrics.set(zipper.idx, nodeMetrics);

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
        Node<Metrics> node = shrinkTree(growTree(node(loc), loc.ops));
        result.siblings = singletonList(node);
        result.metrics = singletonList(loc.ops.rf(node.metrics));
        return result;
      }
      else {
        // TODO can reuse metrics if children are balanced
        Node<Metrics> balanced = balanceChildren(new Node<>(loc.siblings, loc.metrics), loc.ops);
        return replace(loc.parent, balanced, loc.ops.rf(balanced.metrics));
      }
    }
    else {
      if (loc.parent != null && loc.isTransient && !loc.parent.isTransient) {
        return toTransient(loc.parent);
      } else {
        return loc.parent;
      }
    }
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
      result.metrics = n.metrics;
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
      result.metrics = n.metrics;
      result.idx = n.children.size() - 1;
      result.acc = null;
      result.oacc = null;
      result.parent = zipper;
      result.isTransient = zipper.isTransient;
      return result;
    }
    return null;
  }

  public static <Metrics, Data> Tree<Metrics> root(Zipper<Metrics, Data> loc) {
    Zipper<Metrics, Data> parent = up(loc);
    return parent == null ? new Tree<>(node(loc), metrics(loc)) : root(parent);
  }


  public static <Metrics, Data> boolean hasNext(Zipper<Metrics, Data> loc) {
    if (loc.parent == null && node(loc).children.size() > 0) {
      return true;
    }

    return !(isLeaf(loc) && isRightmost(loc));
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
   * stops in a data or in root node if the tree is empty, should never return null
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
   * will move to end position of nearest left data if there is no `next` node to go
   * if parent node has no more children it will be removed recursively
   * */
  public static <Metrics, Data> Zipper<Metrics, Data> remove(Zipper<Metrics, Data> zipper) {
    while (zipper.siblings.size() == 1) {
      if (isRoot(zipper)) {
        return replace(zipper, new Node<>(new ArrayList<>(), new ArrayList<>()), zipper.ops.emptyMetrics());
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

  public static <Metrics, Data> Zipper<Metrics, Data> insertLeft(Zipper<Metrics, Data> zipper, Object newNode, Metrics nodeMetrics) {
    assert !isRoot(zipper);
    Zipper<Metrics, Data> result = zipper.isTransient ? zipper : zipper.copy();
    ArrayList<Object> siblings = isChildrenMutable(zipper)
                                 ? zipper.siblings
                                 : (ArrayList<Object>)zipper.siblings.clone();
    ArrayList<Metrics> metrics = isChildrenMutable(zipper)
                                 ? zipper.metrics
                                 : (ArrayList<Metrics>)zipper.metrics.clone();
    siblings.add(zipper.idx, newNode);
    metrics.add(zipper.idx, nodeMetrics);

    result.siblings = siblings;
    result.idx = zipper.idx + 1;
    result.isChanged = true;
    result.acc = zipper.ops.rf(zipper.acc, nodeMetrics);
    result.oacc = null;
    return result;
  }
}
