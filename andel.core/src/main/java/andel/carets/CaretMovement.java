package andel.carets;

import java.util.Objects;

public class CaretMovement {
  public final long offsetDelta;
  public final long selectionStartDelta;
  public final long selectionEndDelta;

    public static CaretMovement move(Caret caret, long targetOffset, boolean extendSelection) {
        if (extendSelection) {
            return new CaretMovement(targetOffset - caret.offset,
                                     targetOffset < caret.selectionStart ? targetOffset - caret.selectionStart : 0,
                                     targetOffset > caret.selectionEnd ? targetOffset - caret.selectionEnd : 0);
        } else {
            assert caret.offset == caret.selectionStart &&
                caret.offset == caret.selectionEnd;
            return new CaretMovement(targetOffset - caret.offset,
                                     targetOffset - caret.selectionStart,
                                     targetOffset - caret.selectionEnd);
        }
    }

  public CaretMovement(long offsetDelta, long selectionStartDelta, long selectionEndDelta) {
    this.offsetDelta = offsetDelta;
    this.selectionStartDelta = selectionStartDelta;
    this.selectionEndDelta = selectionEndDelta;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    CaretMovement movement = (CaretMovement)o;
    return offsetDelta == movement.offsetDelta &&
           selectionStartDelta == movement.selectionStartDelta &&
           selectionEndDelta == movement.selectionEndDelta;
  }

  @Override
  public int hashCode() {
    return Objects.hash(offsetDelta, selectionStartDelta, selectionEndDelta);
  }
}
