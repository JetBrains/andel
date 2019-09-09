package andel;

import andel.intervals.Impl;
import andel.text.TextRope;

public interface Intervals<T> extends Component {

  static <T> Intervals<T> empty() {
    return Impl.empty(32);
  }

  interface Batch<T> {
    void add(long id, long from, long to, boolean closedLeft, boolean closedRight, T data);
    Intervals<T> commit();
  }

  Batch<T> batch();

  Interval<T> findById(long id);

  Intervals<T> removeByIds(Iterable<Long> ids);

  IntervalsIterator<T> query(long start, long end);

  IntervalsIterator<T> queryReverse(long start, long end);

  Intervals<T> expand(long offset, long length);

  Intervals<T> collapse(long offset, long length);

  @Override
  default Component edit(TextRope before, TextRope after, Edit edit) {
    long offset = 0;
    Intervals<T> res = this;
    for (Object op : edit.ops) {
      if (op instanceof Edit.Retain) {
        offset += ((Edit.Retain)op).count;
      }
      else if (op instanceof Edit.Delete) {
        String text = ((Edit.Delete)op).text;
        res = res.collapse(offset, text.codePointCount(0, text.length()));
      }
      else if (op instanceof Edit.Insert) {
        String text = ((Edit.Insert)op).text;
        int length = text.codePointCount(0, text.length());
        res = res.expand(offset, length);
        offset += length;
      }
    }
    return res;
  }

  default Intervals<T> addIntervals(Iterable<Interval<T>> intervals) {
    Batch<T> batch = this.batch();
    for (Interval<T> interval : intervals) {
      batch.add(interval.id,
                interval.from, interval.to,
                interval.closedLeft, interval.closedRight,
                interval.data);
    }
    return batch.commit();
  }
}
