package andel;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class Edit {

  public static Edit empty() {
    return new Edit(new Object[0], false);
  }

  public static Insert insert(String text) {
    return new Edit.Insert(text);
  }

  public static Retain retain(long count) {
    return new Edit.Retain(count);
  }

  public static Delete delete(String text) {
    return new Edit.Delete(text);
  }

  public static Edit insert(long offset, String text) {
    return new Edit(new Object[]{new Edit.Retain(offset), new Edit.Insert(text)}, true);
  }

  public static Edit delete(long offset, String text) {
    return new Edit(new Object[]{new Edit.Retain(offset), new Edit.Delete(text)}, true);
  }

  public static Edit fromList(List<Object> ops, boolean shiftExact)  {
    return new Edit(ops.toArray(), shiftExact);
  }

  public static long shiftOffset(long offset, Edit edit, boolean shiftExact) {
    long editingOffset = 0;
    for (Object op : edit.ops) {
      if (editingOffset < offset || (editingOffset == offset && shiftExact)) {
        if (op instanceof Edit.Retain) {
          editingOffset += ((Edit.Retain)op).count;
        }
        else if (op instanceof Edit.Delete) {
          String text = ((Edit.Delete)op).text;
          int delta = text.codePointCount(0, text.length());
          offset = Math.max(offset - delta, editingOffset);
        }
        else if (op instanceof Edit.Insert) {
          String text = ((Edit.Insert)op).text;
          int delta = text.codePointCount(0, text.length());
          editingOffset += delta;
          offset += delta;
        }
        else {
          throw new AssertionError();
        }
      }
      else {
        return offset;
      }
    }
    return offset;
  }

  public final Object[] ops;
  public final boolean shiftExactCarets;

  public Edit(Object[] ops, boolean shiftExactCarets) {
    this.ops = ops;
    this.shiftExactCarets = shiftExactCarets;
  }

  public static boolean isIdentity(Edit edit) {
    for (Object op : edit.ops) {
      if (!(op instanceof Retain || Objects.equals(op, Delete.NOOP) || Objects.equals(op, Insert.NOOP))) {
        return false;
      }
    }
    return true;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Edit edit = (Edit)o;
    return Arrays.equals(ops, edit.ops);
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(ops);
  }

  @Override
  public String toString() {
    return Arrays.toString(ops);
  }

  public static class Retain {
    public final long count;

    public Retain(long count) {
      this.count = count;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Retain retain = (Retain)o;
      return count == retain.count;
    }

    @Override
    public int hashCode() {
      return Objects.hash(count);
    }

    @Override
    public String toString() {
      return "[retain " + count + ']';
    }
  }

  public static class Delete {

    public static final Delete NOOP = new Delete("");

    public final String text;

    public Delete(String text) {
      this.text = text;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Delete delete = (Delete)o;
      return Objects.equals(text, delete.text);
    }

    @Override
    public int hashCode() {
      return Objects.hash(text);
    }

    @Override
    public String toString() {
      return "[delete \"" + text + "\"";
    }
  }

  public static class Insert {

    public static final Insert NOOP = new Insert("");

    public final String text;

    public Insert(String text) {
      this.text = text;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Insert insert = (Insert)o;
      return Objects.equals(text, insert.text);
    }

    @Override
    public int hashCode() {
      return Objects.hash(text);
    }

    @Override
    public String toString() {
      return "[insert \"" + text + "\"";
    }
  }
}
