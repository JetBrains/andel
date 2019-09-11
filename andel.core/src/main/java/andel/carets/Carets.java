package andel.carets;

import andel.Component;
import andel.impl.carets.SingleCaret;

import java.util.Comparator;

public interface Carets extends Component {
  static Carets singleCaret(Caret caret) {
    return new SingleCaret(caret);
  }

  Comparator<Caret> COMPARE_BY_OFFSET = Comparator.comparingLong(value -> value.selectionStart);

  Iterable<Caret> getCarets();

  Carets merge(Iterable<Caret> carets);

  Caret getCaret(Object id);

  Carets remove(Iterable<Object> ids);
}
