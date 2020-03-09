package andel.carets;

import andel.Component;
import andel.impl.carets.SingleCaret;

import java.util.Comparator;

public interface MultiCaret extends Component {
  static MultiCaret singleCaret(Caret caret) {
    return new SingleCaret(caret);
  }

  Comparator<Caret> COMPARE_BY_OFFSET = Comparator.comparingLong(value -> value.selectionStart);

  Iterable<Caret> getCarets();

  MultiCaret merge(Iterable<Caret> carets);

  Caret getCaret(Object id);

  MultiCaret remove(Iterable<Object> ids);
}
