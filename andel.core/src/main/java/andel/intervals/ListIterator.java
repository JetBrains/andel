package andel.intervals;

import andel.Interval;
import andel.IntervalsIterator;

import java.util.List;

public class ListIterator<T> implements IntervalsIterator<T> {
  final List<Interval<T>> list;
  int idx;

  public ListIterator(List<Interval<T>> list) {
    this.list = list;
    this.idx = -1;
  }

  @Override
  public boolean closedLeft() {
    return list.get(idx).closedLeft;
  }

  @Override
  public boolean closedRight() {
    return list.get(idx).closedRight;
  }

  @Override
  public long from() {
    return list.get(idx).from;
  }

  @Override
  public long to() {
    return list.get(idx).to;
  }

  @Override
  public long id() {
    return list.get(idx).id;
  }

  @Override
  public T data() {
    return list.get(idx).data;
  }

  @Override
  public boolean next() {
    return ++idx < list.size();
  }
}
