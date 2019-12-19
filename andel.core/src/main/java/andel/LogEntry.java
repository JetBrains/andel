package andel;

import java.util.Objects;

public class LogEntry {
  public final Op op;
  public final Object arg;
  public final Edit edit;
  public final long timestamp;

  public LogEntry(Op op, Object arg, Edit edit, long timestamp) {
    this.op = op;
    this.arg = arg;
    this.edit = edit;
    this.timestamp = timestamp;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    LogEntry entry = (LogEntry)o;
    return op == entry.op &&
           Objects.equals(arg, entry.arg);
  }

  @Override
  public int hashCode() {
    return Objects.hash(op, arg);
  }

  @Override
  public String toString() {
    return "LogEntry{" +
           "op=" + op +
           ", arg=" + arg +
           ", edit=" + edit +
           ", timestamp=" + timestamp +
           '}';
  }
}
