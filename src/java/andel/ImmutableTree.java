package andel;

import java.util.ArrayList;
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
    Object rf(Object[] metrics);
    Object rf(Object m1, Object m2);

    boolean isLeafOverflown(Object leafData);
    boolean isLeafUnderflown(Object leafData);
    Object mergeLeaves(Object leafData1, Object leafData22);
    Object[] splitLeaf(Object leaf);

    int splitThreshold();
  }

  public static class Zipper {
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

  static ArrayList<Object> getChildren(Zipper loc) {
    return ((Node)currentNode(loc)).children;
  }

  private static boolean splitNeeded(ArrayList<Object> children, ZipperOps ops) {
    for (Object child : children) {
      // todo looks like this two expressions can be extracted to INode.isOverflown(ops)
      int splitThreshold = ops.splitThreshold();
      Object node = children.get(0);
      if (isNode(node) && splitThreshold < children.size() ||
          !isNode(node) && ops.isLeafOverflown(child)) {
        return true;
      }
    }
    return false;
  }

  // todo write unit test
  private static ArrayList<ArrayList<INode>> partitionChildren(ArrayList<INode> children, int threshold) {
    int partitionCount = children.size() / threshold;
    int totalPartitions = partitionCount + (children.size() % threshold != 0 ? 1 : 0);

    ArrayList<ArrayList<INode>> partition = new ArrayList<>(totalPartitions);
    int i = 0;
    while (i < children.size()) {
      int capacity = (children.size() - i) % threshold;
      ArrayList<INode> part = new ArrayList<>(capacity);
      part.addAll(children.subList(i, capacity));
      partition.add(part);
      i += capacity;
    }

    return partition;
  }

  private static ArrayList<INode> splitChildren(ArrayList<INode> children, ZipperOps ops) {
    if (splitNeeded(children, ops)) {
      ArrayList<INode> result = new ArrayList<>(children.size() / 2);
      for (INode child : children) {
        if (isNode(child) && ops.splitThreshold() < ops.children(child).size()) {
          ArrayList<ArrayList<INode>> partition = partitionChildren(ops.children(child), ops.splitThreshold());
          for (ArrayList<INode> part : partition) {
            result.add(ops.makeNode(part));
          }
        }
        else if (isLeaf(child) && ops.isOverflown(((Leaf)child).getData())) {
          for (Data part : ops.splitLeaf(((Leaf)child).getData())) {
            result.add(ops.makeLeaf(part));
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

  private static boolean isMergeNeeded(ArrayList<INode> children, ZipperOps ops) {
    int mergeThreshold = ops.splitThreshold() / 2;
    for (INode child : children) {
      // todo looks like this two expressions can be extracted to INode.isUnderflown(ops)
      if (isNode(children.get(0)) && children.size() < mergeThreshold ||
          !isNode(children.get(0)) && ops.isUnderflown(((Leaf)child).getData())) {
        return true;
      }
    }
    return false;
  }

  private static ArrayList<INode> mergeChildren(ArrayList<INode> children, ZipperOps ops) {
    final int mergeThreshold = ops.splitThreshold() / 2;
    if (isMergeNeeded(children, ops)) {
      ArrayList<INode> result = new ArrayList<>(mergeThreshold);
      if (isNode(children.get(0))) {
        INode left = children.get(0);
        for (int i = 1; i < children.size(); i++) {
          INode right = children.get(i);
          ArrayList<INode> leftChildren = ops.children(left);
          ArrayList<INode> rightChildren = ops.children(right);

          if (leftChildren.size() < mergeThreshold || rightChildren.size() < mergeThreshold) {
            if (ops.splitThreshold() <= leftChildren.size() + rightChildren.size()) {
              int n = (leftChildren.size() + rightChildren.size()) / 2;
              // todo no actual need in tmp array here
              ArrayList<INode> tmp = joinChildren(leftChildren, rightChildren);

              ArrayList<INode> newLeft = new ArrayList<>(n);
              newLeft.addAll(tmp.subList(0, n));
              ArrayList<INode> newRight = new ArrayList<>(tmp.size() - n);
              newRight.addAll(tmp.subList(n, tmp.size() - n));

              result.add(ops.makeNode(mergeChildren(newLeft, ops)));
              left = ops.makeNode(newRight);
            }
            else {
              left = ops.makeNode(mergeChildren(joinChildren(leftChildren, rightChildren), ops));
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
        Data leftData = ((Leaf)children.get(0)).getData();

        for (int i = 1; i < children.size(); i++) {
          Data rightData = ((Leaf)children.get(i)).getData();
          if (ops.isUnderflown(leftData) || ops.isUnderflown(rightData)) {
            Data mergedData = ops.mergeLeaves(leftData, rightData);
            if (ops.isOverflown(mergedData)) {
              Data[] split = ops.splitLeaf(mergedData);
              Data dataLeft = split[0];
              Data dataRight = split[1];
              result.add(ops.makeLeaf(dataLeft));
              leftData = dataRight;
            }
            else {
              leftData = mergedData;
            }
          }
          else {
            result.add(ops.makeLeaf(leftData));
            leftData = rightData;
          }
        }
        result.add(ops.makeLeaf(leftData));
      }
      return result;
    }
    return children;
  }

  private static ArrayList<INode> joinChildren(ArrayList<INode> leftChildren, ArrayList<INode> rightChildren) {
    ArrayList<INode> tmp = new ArrayList<>(leftChildren.size() + rightChildren.size());
    tmp.addAll(leftChildren);
    tmp.addAll(rightChildren);
    return tmp;
  }

  private static ArrayList<INode> balanceChildren(ArrayList<INode> children, ZipperOps ops) {
    return mergeChildren(splitChildren(children, ops), ops);
  }

  private static Node growTree(ArrayList<INode> children, ZipperOps ops) {
    ArrayList<INode> balanced = balanceChildren(children, ops);
    if (balanced.size() < ops.splitThreshold()) {
      return ops.makeNode(balanced);
    }
    else {
      return growTree(balanced, ops);
    }
  }

  private static Node shrinkTree(Node node) {
    if (isNode(node.getChildren().get(0)) && node.getChildren().size() == 1) {
      return shrinkTree((Node)node.getChildren().get(0));
    }
    return node;
  }

  private static boolean isMutable(Zipper loc) {
    return loc.isTransient && loc.isChanged;
  }

  static Zipper replaceNode(Zipper loc, INode node) {
    Zipper newLoc = loc.clone();
    if (isMutable(newLoc)) {
      newLoc.siblings.set(newLoc.idx, node);
    }
    else {
      ArrayList<INode> copy = (ArrayList<INode>)newLoc.siblings.clone();
      copy.set(newLoc.idx, node);
      newLoc.siblings = copy;
    }
    return newLoc;
  }

  private static ArrayList<INode> wrapNode(INode node, ZipperOps ops) {
    ArrayList<INode> al = new ArrayList<>(ops.splitThreshold() / 2);
    al.add(node);
    return al;
  }

  static Zipper up(Zipper loc) {
    if (loc.isChanged) {
      if (loc.parent != null) {
        return replaceNode(loc.parent, loc.ops.makeNode(balanceChildren(getChildren(loc), loc.ops)));
      }
      else {
        return new ZipperBuilder(loc.ops)
          .setIsTransient(loc.isTransient)
          .setIdx(0)
          .setSiblings(wrapNode(shrinkTree(growTree(wrapNode(currentNode(loc), loc.ops), loc.ops)), loc.ops))
          .setIsRoot(true)
          .build();
      }
    }
    return loc.parent;
  }

  static Zipper right(Zipper loc) {
    if (loc.idx < loc.siblings.size() - 1) {
      return new ZipperBuilder(loc)
        .setIdx(loc.idx + 1)
        .setAcc(loc.ops.rf(currentAcc(loc), currentNode(loc).getMetrics()))
        .setOacc(null)
        .build();
    }
    return null;
  }

  static Zipper downForward(Zipper loc) {
    if (isBranch(loc)) {
      return new ZipperBuilder(loc.ops)
        .setSiblings(getChildren(loc))
        .setIdx(0)
        .setParent(loc)
        .build();
    }
    return null;
  }

  static Zipper downBackward(Zipper loc) {
    if (isBranch(loc)) {
      return new ZipperBuilder(loc.ops)
        .setSiblings(getChildren(loc))
        .setIdx(getChildren(loc).size() - 1)
        .setParent(loc)
        .build();
    }
    return null;
  }

  static INode root(Zipper loc) {
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

        return new ZipperBuilder(loc.ops)
          .setIdx(0)
          .setIsTransient(loc.isTransient)
          .setSiblings(wrapNode(currentNode(loc), loc.ops))
          .setIsEnd(true)
          .build();
      }
    }
  }

  static Zipper skip(Zipper loc) {
    if (loc.isEnd) {
      return loc;
    }

    // todo extract shared code with next
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
        return new ZipperBuilder(loc.ops)
          .setIdx(0)
          .setIsTransient(loc.isTransient)
          .setSiblings(wrapNode(currentNode(loc), loc.ops))
          .setIsEnd(true)
          .build();
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
        ArrayList<INode> newSiblings = new ArrayList<>();
        newSiblings.add(currentNode(loc));
        return new ZipperBuilder(loc.ops)
          .setIdx(0)
          .setIsTransient(loc.isTransient)
          .setSiblings(newSiblings)
          .setIsEnd(true)
          .build();
      }
    }
  }

  static Zipper prevLeaf(Zipper loc) {
    // todo fix
    return null;
  }

  //---------------------------------------------------------------------------

  static Zipper edit(Zipper loc, Function<INode, INode> fn) {
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
    final public Metrics acc;
    final public Metrics metrics;

    public accAndMetrics(Metrics acc, Metrics metrics) {
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
        Metrics acc = loc.acc == null ? loc.ops.rf() : loc.acc;
        for (int i = loc.idx; i < loc.siblings.size(); i++) {
          if (pred.apply(new accAndMetrics(acc, loc.siblings.get(i).getMetrics()))) {
            nextLoc = new ZipperBuilder(loc)
              .setAcc(acc)
              .setIdx(i)
              .build();
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
        return replaceNode(loc, loc.ops.makeNode(new ArrayList<>()));
      }

      loc = up(loc);
    }

    ArrayList<INode> newSiblings;
    if (isMutable(loc)) {
      newSiblings = loc.siblings;
    }
    else {
      newSiblings = (ArrayList<INode>)loc.siblings.clone();
    }
    newSiblings.remove(loc.idx);


    if (loc.idx < loc.siblings.size() - 1) {
      return new ZipperBuilder(loc)
        .setIsChanged(true)
        .setSiblings(newSiblings)
        .build();
    }
    else {
      return new ZipperBuilder(loc)
        .setIsChanged(true)
        .setSiblings(newSiblings)
        .setAcc(null)
        .setIdx(loc.idx - 1)
        .build();
    }
  }
}