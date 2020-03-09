package andel;

import io.lacuna.bifurcan.List;
import io.lacuna.bifurcan.Lists;
import io.lacuna.bifurcan.Map;

import java.util.Objects;

public class Log {

  public static Log EMPTY = new Log();

  public long timestamp;
  public List<Entry> entries;
  public Map<Object, List<UndoInfo>> undoStack;

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Log log = (Log)o;
    return timestamp == log.timestamp &&
           entries.equals(log.entries) &&
           undoStack.equals(log.undoStack);
  }

  @Override
  public int hashCode() {
    return Objects.hash(timestamp, entries, undoStack);
  }

  public Log(List<Entry> entries, Map<Object, List<UndoInfo>> undoStack, long timestamp) {
    this.timestamp = timestamp;
    this.entries = entries;
    this.undoStack = undoStack;
  }

  public Log() {
    this.timestamp = 0;
    this.entries = new List<Entry>();
    this.undoStack = new Map<Object, List<UndoInfo>>();
  }

  public static class UndoInfo {
    public long from;
    public long to;

    public UndoInfo(long from, long to) {
      this.from = from;
      this.to = to;
    }
  }

  public Log add(Op op, Object arg, Edit edit, Object author) {
    long next = this.timestamp + 1;
    Entry entry = new Entry(op, arg, edit, next, author);
    List<Entry> entries = this.entries.addLast(entry);
    Map<Object, List<UndoInfo>> stack;
    if (op == Op.UNDO || op == Op.REDO || breaksLog(op)) {
      stack = this.undoStack;
    } else {
      stack = this.undoStack.remove(author);
    }
    return new Log(entries,
                   stack,
                   next);
  }

  public java.util.List<Entry> entriesSince(long timestamp) {
    // todo binarysearch here
    int start = 0;
    for (int i = 0; i < this.entries.size(); i++) {
      Entry e = this.entries.nth(i);
      if (e.timestamp > timestamp) {
        start = i;
        break;
      }
    }

    return Lists.toList(this.entries.slice(start, this.entries.size()));
  }

  public java.util.List<Entry> entries() {
    return Lists.toList(this.entries);
  }

  private long authorPosition(Object author) {
    if (undoStack.get(author).isPresent() && undoStack.get(author, null).size() != 0) {
      return undoStack.get(author, null).last().from - 1;
    } else {
      return entries.size() - 1;
    }
  }

  private static boolean breaksLog(Op op) {
    return op == Op.ADD_CARET
           || op == Op.MOVE_CARETS
           || op == Op.DROP_SELECTIONS;
  }

  private long undoRangeEnd(Object author) {
    long idx = authorPosition(author);
    while (idx > 0) {
      Entry e = entries.nth(idx);
      if (e.author != author || breaksLog(e.op)) {
        idx -= 1;
      } else if (e.op == Op.UNDO) {
        idx = ((UndoInfo)e.arg).from - 1;
      } else {
        return idx + 1;
      }
    }
    return -1;
  }

  private long undoRangeStart(Object author, long end) {
    long idx = end - 1;
    while (idx > 0) {
      Entry e = entries.nth(idx);
      if (e.author == author && !breaksLog(e.op)) {
        if (e.op == Op.UNDO) {
          idx = ((UndoInfo)e.arg).from - 1;
        } else {
          idx -= 1;
        }
      } else {
        return idx + 1;
      }
    }

    return 0;
  }

  public Range nextUndoRange(Object author) {
    long to = undoRangeEnd(author);
    if (to < 0) {
      return null;
    }

    long from = undoRangeStart(author, to);
    return new Range(from, to);
  }

  public Log pushUndo(UndoInfo info, Object author) {
    return new Log(this.entries,
                   this.undoStack.update(author, infos -> (infos == null ? new List<UndoInfo>() : infos).addLast(info)),
                   this.timestamp);
  }

  public Log popUndo(Object author) {
    return new Log(this.entries,
                   this.undoStack.update(author, infos -> (infos == null ? null : infos.removeLast())),
                   this.timestamp);
  }

  public static class Entry {
    public final Op op;
    public final Object arg;
    public final Edit edit;
    public final long timestamp;
    public final Object author;

    public Entry(Op op, Object arg, Edit edit, long timestamp, Object author) {
      this.op = op;
      this.arg = arg;
      this.edit = edit;
      this.timestamp = timestamp;
      this.author = author;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Entry entry = (Entry)o;
      return op == entry.op &&
             Objects.equals(arg, entry.arg) &&
             Objects.equals(edit, entry.edit) &&
             timestamp == entry.timestamp &&
             Objects.equals(author, entry.author);
    }

    @Override
    public int hashCode() {
      return Objects.hash(op, arg, edit, timestamp, author);
    }

    @Override
    public String toString() {
      return "LogEntry{" +
             "op=" + op +
             ", arg=" + arg +
             ", edit=" + edit +
             ", timestamp=" + timestamp +
             ", author=" + author +
             '}';
    }
  }

  public static class Range {

    public final long from;
    public final long to;

    public Range(long from, long to) {
      this.from = from;
      this.to = to;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Range range = (Range)o;
      return from == range.from &&
             to == range.to;
    }

    @Override
    public int hashCode() {
      return Objects.hash(from, to);
    }
  }
}
