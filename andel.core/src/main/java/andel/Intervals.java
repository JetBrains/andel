package andel;

public interface Intervals<T> extends Component {
    static<T> Intervals<T> empty() {
        return Impl.empty(32);
    }

    Intervals<T> addIntervals(Iterable<Interval<T>> intervals);

    Interval<T> findById(long id);

    Intervals<T> removeByIds(Iterable<Long> ids);

    IntervalsIterator<T> query(long start, long end);

    IntervalsIterator<T> queryReverse(long start, long end);

    Intervals<T> expand(long offset, long length);

    Intervals<T> collapse(long offset, long length);

    @Override
    default Component edit(Text before, Text after, Edit edit) {
        long offset = 0;
        Intervals<T> res = this;
        for (Object op : edit.ops) {
            if (op instanceof Edit.Retain) {
                offset += ((Edit.Retain) op).count;
            } else if (op instanceof Edit.Delete) {
                String text = ((Edit.Delete) op).text;
                res = res.collapse(offset, text.codePointCount(0, text.length()));
            } else if (op instanceof Edit.Insert) {
                String text = ((Edit.Insert) op).text;
                int length = text.codePointCount(0, text.length());
                res = res.expand(offset, length);
                offset += length;
            }
        }
        return res;
    }
}
