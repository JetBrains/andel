package andel.impl.carets;

import andel.Component;
import andel.Controller;
import andel.Edit;
import andel.carets.Caret;
import andel.carets.Carets;
import andel.text.Text;

import java.util.Collections;
import java.util.Iterator;


public class SingleCaret implements Carets {

  private final Caret caret;

  public SingleCaret(Caret caret) {
    this.caret = caret;
  }

  @Override
  public Iterable<Caret> getCarets() {
    return Collections.singletonList(this.caret);
  }

  @Override
  public Caret getCaret(Object id) {
    if (id.equals(this.caret.id)) {
      return this.caret;
    }
    else {
      return null;
    }
  }

  @Override
  public Carets merge(Iterable<Caret> carets) {
    Iterator<Caret> iterator = carets.iterator();
    if (!iterator.hasNext()) {
      return this;
    }
    else {
      Caret c = iterator.next();
      if (iterator.hasNext()) {
        throw new AssertionError();
      }
      else {
        if (c.id.equals(this.caret.id)) {
          return new SingleCaret(c);
        }
        else {
          throw new AssertionError();
        }
      }
    }
  }

  @Override
  public Carets remove(Iterable<Object> ids) {
    if (ids.iterator().hasNext()) {
      throw new AssertionError();
    }
    else {
      return this;
    }
  }

  @Override
  public Component edit(Text before, Text after, Edit edit) {
    if (this.caret.selectionStart == this.caret.selectionEnd) {
      long offsetPrime = Edit.shiftOffset(this.caret.offset, edit, true);
      return new SingleCaret(new Caret(this.caret.id, offsetPrime, offsetPrime, offsetPrime, after.offsetToGeomCol(offsetPrime)));
    } else {
      long offsetPrime = Edit.shiftOffset(this.caret.offset, edit, this.caret.offset != this.caret.selectionEnd);
      long selectionStartPrime = Edit.shiftOffset(this.caret.selectionStart, edit, true);
      long selectionEndPrime = Edit.shiftOffset(this.caret.selectionEnd, edit, false);
      return new SingleCaret(new Caret(this.caret.id, offsetPrime, selectionStartPrime, selectionEndPrime, after.offsetToGeomCol(offsetPrime)));
    }
  }

  @Override
  public String toString() {
    return "SingleCaret{" +
           "caret=" + caret +
           '}';
  }
}
