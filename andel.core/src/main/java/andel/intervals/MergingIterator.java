package andel.intervals;

import andel.IntervalsIterator;

import java.util.Comparator;

public class MergingIterator<T> implements IntervalsIterator<T> {

  private IntervalsIterator<T> first;
  private IntervalsIterator<T> second;
  private final Comparator<IntervalsIterator> comparator;
  private boolean myFirstTime = true;

  public MergingIterator(IntervalsIterator<T> first, IntervalsIterator<T> second, Comparator<IntervalsIterator> comparator) {
    this.first = first;
    this.second = second;
    this.comparator = comparator;
  }

  @Override
  public boolean closedLeft() {
    return first.closedLeft();
  }

  @Override
  public boolean closedRight() {
    return first.closedRight();
  }

  @Override
  public long from() {
    return first.from();
  }

  @Override
  public long to() {
    return first.to();
  }

  @Override
  public long id() {
    return first.id();
  }

  @Override
  public T data() {
    return first.data();
  }

  @Override
  public boolean next() {
    if (myFirstTime) {
      myFirstTime = false;
      boolean firstNext = first.next();
      boolean secondNext = second.next();
      if (firstNext && secondNext) {
        if (comparator.compare(first, second) > 0) {
          IntervalsIterator<T> tmp = second;
          second = first;
          first = tmp;
        }
        return true;
      }
      else if (firstNext) {
        second = null;
        return true;
      }
      else if (secondNext) {
        first = second;
        second = null;
        return true;
      }
      else {
        return false;
      }
    }
    else {
      if (first.next()) {
        if (second != null) {
          if (comparator.compare(first, second) > 0) {
            IntervalsIterator<T> tmp = second;
            second = first;
            first = tmp;
          }
        }
        return true;
      }
      else {
        first = second;
        second = null;
        return first != null;
      }
    }
  }
}
