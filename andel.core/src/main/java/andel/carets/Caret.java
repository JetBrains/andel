package andel.carets;

import java.util.Objects;

public class Caret {
  public final Object id;
  public final long offset;
  public final long selectionStart;
  public final long selectionEnd;
  public final long vCol;

  public Caret(Object id, long offset, long selectionStart, long selectionEnd, long vCol) {
    this.id = id;
    this.offset = offset;
    this.selectionStart = selectionStart;
    this.selectionEnd = selectionEnd;
    this.vCol = vCol;
  }

  @Override
  public String toString() {
    return "Caret{" +
           "id=" + id +
           ", offset=" + offset +
           ", selectionStart=" + selectionStart +
           ", selectionEnd=" + selectionEnd +
           ", vCol=" + vCol +
           '}';
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Caret caret = (Caret)o;
    return id == caret.id &&
           offset == caret.offset &&
           selectionStart == caret.selectionStart &&
           selectionEnd == caret.selectionEnd &&
           vCol == vCol;
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, offset, selectionStart, selectionEnd, vCol);
  }
}
