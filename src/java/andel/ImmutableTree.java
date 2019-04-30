package andel;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class ImmutableTree {

  public static class Node {
    public ArrayList<Object> children;
    // store precalculated accumulator values in metrics to use binary search
    public ArrayList<Object> metrics;
  }

  interface ZipperOps {

    Object calculateMetrics(Object data);
    Object emptyMetrics();

    Object rf(Object m1, Object m2);
    default Object rf(Object[] metrics){
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
    public int idx = 0;


    boolean isChanged = false;
    boolean isTransient = false;
    Object acc;
    Object oacc;

    boolean isEnd = false;
    boolean isRoot = false; // suspiciously useless flag

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
  }

  static Object currentNode(Zipper loc) {
    return loc.siblings.get(loc.idx);
  }

  static Object currentAcc(Zipper loc) {
    if (loc.oacc != null) return loc.oacc;
    return loc.acc;
  }

  static boolean isNode(Object node) {
    return node instanceof Node;
  }

  static boolean isLeaf(Object leaf) {
    return !isNode(leaf);
  }

  static boolean isBranch(Zipper loc) {
    return isNode(currentNode(loc));
  }

  static ArrayList<Object> getChildren(Object node){
    return ((Node) node).children;
  }

  private static boolean splitNeeded(ArrayList<Object> children, ZipperOps ops) {
    int splitThreshold = ops.splitThreshold();
    if (isNode(children.get(0))){
      for (Object child : children){
        if (splitThreshold < getChildren(child).size()){
          return true;
        }
      }
    } else {
      for (Object child : children){
        if (ops.isLeafOverflown(child)){
          return true;
        }
      }
    }
    return false;
  }

  // todo write unit test
  private static ArrayList<ArrayList<Object>> partitionChildren(ArrayList<Object> children, int threshold) {
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

  private static ArrayList<Object> splitChildren(ArrayList<Object> children, ZipperOps ops) {
    if (splitNeeded(children, ops)) {
      ArrayList<Object> result = new ArrayList<>(children.size() / 2);
      for (Object child : children) {
        if (isNode(child) && ops.splitThreshold() < getChildren(child).size()) {
          ArrayList<ArrayList<Object>> partition = partitionChildren(getChildren(child), ops.splitThreshold());
          for (ArrayList<Object> part : partition) {
            result.add(makeNode(part));
          }
        }
        else if (isLeaf(child) && ops.isLeafOverflown(child)) {
          result.addAll(ops.splitLeaf(child));
        }
        else {
          result.add(child);
        }
      }
      return result;
    }
    return children;
  }

  private static boolean isMergeNeeded(ArrayList<Object> children, ZipperOps ops) {
    int mergeThreshold = ops.splitThreshold() / 2;
    if (isNode(children.get(0))){
      for (Object child : children){
        if (getChildren(child).size() < mergeThreshold){
          return true;
        }
      }
    } else {
      for (Object child : children){
        if (ops.isLeafUnderflown(child)){
          return true;
        }
      }
    }
    return false;
  }

  private static ArrayList<Object> mergeChildren(ArrayList<Object> children, ZipperOps ops) {
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
            if (ops.splitThreshold() <= leftChildren.size() + rightChildren.size()) {
              int n = (leftChildren.size() + rightChildren.size()) / 2;
              // todo no actual need in tmp array here
              ArrayList<Object> tmp = joinChildren(leftChildren, rightChildren);

              ArrayList<Object> newLeft = new ArrayList<>(n);
              newLeft.addAll(tmp.subList(0, n));
              ArrayList<Object> newRight = new ArrayList<>(tmp.size() - n);
              newRight.addAll(tmp.subList(n, tmp.size() - n));

              result.add(makeNode(mergeChildren(newLeft, ops)));
              left = makeNode(newRight);
            }
            else {
              left = makeNode(mergeChildren(joinChildren(leftChildren, rightChildren), ops));
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
        Object leftData = children.get(0);

        for (int i = 1; i < children.size(); i++) {
          Object rightData = children.get(i);
          if (ops.isLeafUnderflown(leftData) || ops.isLeafUnderflown(rightData)) {
            Object mergedData = ops.mergeLeaves(leftData, rightData);
            if (ops.isLeafOverflown(mergedData)) {
              List<Object> split = ops.splitLeaf(mergedData);
              Object dataLeft = split.get(0);
              Object dataRight = split.get(1);
              result.add(dataLeft);
              leftData = dataRight;
            }
            else {
              leftData = mergedData;
            }
          }
          else {
            result.add(leftData);
            leftData = rightData;
          }
        }
        result.add(leftData);
      }
      return result;
    }
    return children;
  }

  private static ArrayList<Object> joinChildren(ArrayList<Object> leftChildren, ArrayList<Object> rightChildren) {
    ArrayList<Object> tmp = new ArrayList<>(leftChildren.size() + rightChildren.size());
    tmp.addAll(leftChildren);
    tmp.addAll(rightChildren);
    return tmp;
  }

  private static ArrayList<Object> balanceChildren(ArrayList<Object> children, ZipperOps ops) {
    return mergeChildren(splitChildren(children, ops), ops);
  }

  private static Node growTree(ArrayList<Object> children, ZipperOps ops) {
    ArrayList<Object> balanced = balanceChildren(children, ops);
    if (balanced.size() < ops.splitThreshold()) {
      return makeNode(balanced);
    }
    else {
      return growTree(balanced, ops);
    }
  }

  private static Node shrinkTree(Node node) {
    ArrayList<Object> children = getChildren(node);
    if (children.size() == 1 && isNode(children.get(0))) {
      return shrinkTree((Node)children.get(0));
    }
    return node;
  }

  private static boolean isMutable(Zipper loc) {
    return loc.isTransient && loc.isChanged;
  }

  static Zipper replaceNode(Zipper loc, Object node) {
    Zipper newLoc = loc.clone();
    if (isMutable(newLoc)) {
      newLoc.siblings.set(newLoc.idx, node);
    }
    else {
      ArrayList<Object> copy = (ArrayList<Object>)newLoc.siblings.clone();
      copy.set(newLoc.idx, node);
      newLoc.siblings = copy;
    }
    return newLoc;
  }

  private static ArrayList<Object> wrapNode(Object node, ZipperOps ops) {
    ArrayList<Object> al = new ArrayList<>(ops.splitThreshold() / 2);
    al.add(node);
    return al;
  }

  static Zipper up(Zipper loc) {
    if (loc.isChanged) {
      if (loc.parent != null) {
        return replaceNode(loc.parent, makeNode(balanceChildren(getChildren(loc), loc.ops)));
      }
      else {
        Zipper zipper = new Zipper();
        zipper.ops = loc.ops;
        zipper.isTransient = loc.isTransient;
        zipper.idx = 0;
        zipper.siblings = wrapNode(shrinkTree(growTree(wrapNode(currentNode(loc), loc.ops), loc.ops)), loc.ops);
        zipper.isRoot = true;
        return zipper;
      }
    }
    return loc.parent;
  }

  static Zipper right(Zipper loc) {
    if (loc.idx < loc.siblings.size() - 1) {
      Zipper zipper = new Zipper();
      zipper.ops = loc.ops;
      zipper.idx = loc.idx + 1;
      zipper.siblings = loc.siblings;
      zipper.acc = loc.ops.rf(currentAcc(loc), currentNode(loc).getMetrics());
      zipper.oacc = null;
      zipper.parent = loc.parent;
      zipper.isChanged = loc.isChanged;
      return zipper;
    }
    return null;
  }

  static Zipper downForward(Zipper loc) {
    if (isBranch(loc)) {
      Zipper zipper = new Zipper();
      zipper.ops = loc.ops;
      zipper.siblings = getChildren(loc);
      zipper.idx = 0;
      zipper.parent = loc;
      return zipper;
    }
    return null;
  }

  static Zipper downBackward(Zipper loc) {
    if (isBranch(loc)) {
      Zipper zipper = new Zipper();
      ArrayList<Object> children = getChildren(loc);
      zipper.ops = loc.ops;
      zipper.siblings = children;
      zipper.idx = children.size() - 1;
      zipper.parent = loc;
    }
    return null;
  }

  static Object root(Zipper loc) {
    if (loc.isEnd) {
      return currentNode(loc);
    }

    Zipper parent = up(loc);
    if (parent != null) {
      return root(parent);
    }

    return currentNode(loc);
  }

  static Zipper next(Zipper loc) {
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

    Zipper p = up(loc);
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
        zipper.ops = loc.ops;
        zipper.idx = 0;
        zipper.isTransient = loc.isTransient;
        zipper.siblings = wrapNode(currentNode(loc), loc.ops)
        zipper.isEnd = true;
        return zipper;
      }
    }
  }

  static Zipper skip(Zipper loc) {
    if (loc.isEnd) {
      return loc;
    }

    Zipper right = right(loc);
    if (right != null) {
      return right;
    }

    Zipper p = up(loc);
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
        zipper.ops = loc.ops;
        zipper.idx = 0;
        zipper.isTransient = loc.isTransient;
        zipper.siblings = wrapNode(currentNode(loc), loc.ops);
        zipper.isEnd = true;
        return zipper;
      }
    }
  }

  //------ don't use these functions unless you know what you are doing -------


  static Zipper left(Zipper loc) {
    if (0 < loc.idx) {
      return new ZipperBuilder(loc)
        .setIdx(loc.idx - 1)
        .setAcc(null)
        .setOacc(null)
        .build();
    }
    return null;
  }

  static Zipper prev(Zipper loc) {
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
        ArrayList<Object> newSiblings = new ArrayList<>();
        newSiblings.add(currentNode(loc));
        Zipper zipper = new Zipper();
        zipper.ops = loc.ops;
        zipper.idx = 0;
        zipper.isTransient = loc.isTransient;
        zipper.siblings = newSiblings;
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

  static Zipper edit(Zipper loc, Function<Object, Object> fn) {
    return replaceNode(loc, fn.apply(currentNode(loc)));
  }

  static Zipper nextLeaf(Zipper loc) {
    while (!isLeaf(currentNode(loc)) || !loc.isRoot) {
      loc = next(loc);
    }
    return loc;
  }

  // todo get rid of this POJ
  static class accAndMetrics {
    final public Object acc;
    final public Object metrics;

    public accAndMetrics(Object acc, Object metrics) {
      this.acc = acc;
      this.metrics = metrics;
    }
  }

  static Zipper scan(Zipper loc, Function<accAndMetrics, Boolean> pred) {
    while (loc != null) {

      if (loc.isEnd) {
        return loc;
      }

      Zipper nextLoc = null;
      if (loc.isRoot) {
        nextLoc = loc;
      }
      else {
        Object acc = loc.acc == null ? loc.ops.emptyMetrics() : loc.acc;
        for (int i = loc.idx; i < loc.siblings.size(); i++) {
          if (pred.apply(new accAndMetrics(acc, loc.siblings.get(i).getMetrics()))) {
            Zipper zipper = new Zipper();
            zipper.ops = loc.ops;
            zipper.isEnd = loc.isEnd;
            zipper.siblings = loc.siblings;
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
            acc = loc.ops.rf(acc, loc.siblings.get(i).getMetrics());
          }
        }
      }

      if (nextLoc != null) {
        if (isBranch(nextLoc)) {
          loc = downForward(nextLoc);
        }
        else {
          return nextLoc;
        }
      }
      else {
        loc = skip(up(loc));
      }
    }
    return null;
  }

  static Zipper remove(Zipper loc) {

    while (loc.siblings.size() == 1) {
      if (loc.isRoot) {
        return replaceNode(loc, makeNode(new ArrayList<>()));
      }

      loc = up(loc);
    }

    ArrayList<Object> newSiblings;
    if (isMutable(loc)) {
      newSiblings = loc.siblings;
    }
    else {
      newSiblings = (ArrayList<Object>)loc.siblings.clone();
    }
    newSiblings.remove(loc.idx);


    if (loc.idx < loc.siblings.size() - 1) {
      Zipper zipper = new Zipper();
      zipper.ops = loc.ops;
      zipper.isEnd = loc.isEnd;
      zipper.siblings = newSiblings;
      zipper.isTransient = loc.isTransient;
      zipper.isChanged = true;
      zipper.parent = loc.parent;
      zipper.isRoot = loc.isRoot;
      zipper.acc = loc.acc;
      zipper.oacc = loc.oacc;
      zipper.idx = loc.idx;
      return zipper;
    }
    else {
      Zipper zipper = new Zipper();
      zipper.ops = loc.ops;
      zipper.isEnd = loc.isEnd;
      zipper.siblings = newSiblings;
      zipper.isTransient = loc.isTransient;
      zipper.isChanged = true;
      zipper.parent = loc.parent;
      zipper.isRoot = loc.isRoot;
      zipper.acc = null;
      zipper.oacc = loc.oacc;
      zipper.idx = loc.idx - 1;
      return zipper;
    }
  }
}