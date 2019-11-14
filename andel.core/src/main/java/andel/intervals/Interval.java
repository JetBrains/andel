package andel.intervals;

import java.util.Comparator;
import java.util.Objects;

public class Interval<T> {
  public static final Comparator<Interval> CMP_ENDS = Comparator.comparingLong((i) -> i.to);
  
  public long id;
  public long from;
  public long to;
  public T data;
  public boolean closedLeft;
  public boolean closedRight;

  public Interval(long id, long from, long to, boolean closedLeft, boolean closedRight, T data) {
    assert from <= to : ("Interval to <= from: " + to + " <= " + from);
    this.id = id;
    this.from = from;
    this.to = to;
    this.data = data;
    this.closedLeft = closedLeft;
    this.closedRight = closedRight;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Interval<?> interval = (Interval<?>)o;
    return id == interval.id &&
           from == interval.from &&
           to == interval.to &&
           closedLeft == interval.closedLeft &&
           closedRight == interval.closedRight &&
           Objects.equals(data, interval.data);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, from, to, data, closedLeft, closedRight);
  }

  @Override
  public String toString() {
    return "id=" + id + " " + (closedLeft ? "[" : "(") + from + ", " + to + (closedRight ? "]" : ")") + " " + data;
  }
}
