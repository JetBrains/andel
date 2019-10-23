package andel.carets;

import java.util.Objects;

public class CaretMovement {
  public final long offsetDelta;
  public final long selectionStartDelta;
  public final long selectionEndDelta;
  public final boolean keepVCol;

  public static CaretMovement move(Caret caret, long targetOffset, boolean extendSelection, boolean keepVCol) {
    if (extendSelection) {
      return new CaretMovement(targetOffset - caret.offset,
                               targetOffset < caret.selectionStart ? targetOffset - caret.selectionStart : 0,
                               targetOffset > caret.selectionEnd ? targetOffset - caret.selectionEnd : 0,
                               keepVCol);
    }
    else {
      assert caret.offset == caret.selectionStart &&
             caret.offset == caret.selectionEnd;
      return new CaretMovement(targetOffset - caret.offset,
                               targetOffset - caret.selectionStart,
                               targetOffset - caret.selectionEnd,
                               keepVCol);
    }
  }

  public static CaretMovement expandSelection(Caret caret, long start, long end) {
    return new CaretMovement(0,
                             start - caret.selectionStart,
                             end - caret.selectionEnd,
                             true);
  }

  public CaretMovement(long offsetDelta, long selectionStartDelta, long selectionEndDelta, boolean keepVCol) {
    this.offsetDelta = offsetDelta;
    this.selectionStartDelta = selectionStartDelta;
    this.selectionEndDelta = selectionEndDelta;
    this.keepVCol = keepVCol;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    CaretMovement movement = (CaretMovement)o;
    return offsetDelta == movement.offsetDelta &&
           selectionStartDelta == movement.selectionStartDelta &&
           selectionEndDelta == movement.selectionEndDelta &&
           keepVCol == movement.keepVCol;
  }

  @Override
  public int hashCode() {
    return Objects.hash(offsetDelta, selectionStartDelta, selectionEndDelta, keepVCol);
  }

  @Override
  public String toString() {
    return "CaretMovement{" +
           "offsetDelta=" + offsetDelta +
           ", selectionStartDelta=" + selectionStartDelta +
           ", selectionEndDelta=" + selectionEndDelta +
           ", keepVCol=" + keepVCol +
           '}';
  }
}
