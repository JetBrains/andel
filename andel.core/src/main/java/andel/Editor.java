package andel;

import andel.carets.MultiCaret;
import io.lacuna.bifurcan.Map;

import java.util.Objects;

public class Editor {

  public static final Attr<MultiCaret> CARETS = new Attr<>("CARETS");

  public final Object caretsKey;
  public final Composite composite;

  public Editor(Object caretsKey, Composite composite) {
    this.caretsKey = caretsKey;
    this.composite = composite;
  }

  public Editor withComposite(Composite composite) {
    return new Editor(this.caretsKey, composite);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Editor editor = (Editor)o;
    return Objects.equals(caretsKey, editor.caretsKey) &&
           Objects.equals(composite, editor.composite);
  }

  @Override
  public int hashCode() {
    return Objects.hash(caretsKey, composite);
  }

  public MultiCaret getCarets() {
    return (MultiCaret)this.composite.getComponent(caretsKey);
  }

  public Editor putCarets(MultiCaret carets) {
    return new Editor(caretsKey, this.composite.assoc(caretsKey, carets));
  }

  public Editor log(Op op, Object arg, Edit edit) {
    return this.withComposite(this.composite.log(op, arg, edit));
  }

  public Editor edit(Edit edit) {
    return this.withComposite(this.composite.edit(edit));
  }
}
