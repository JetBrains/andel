package andel;

import java.util.Objects;

public class LogEntry {
  public final Op op;
  public final Object arg;

  public LogEntry(Op op, Object arg) {
    this.op = op;
    this.arg = arg;
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
           '}';
  }
}
