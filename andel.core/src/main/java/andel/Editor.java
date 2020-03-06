package andel;

import andel.carets.MultiCaret;

import java.util.Objects;

public class Editor {
  private final Attr<MultiCaret> caretsId;
  public final Composite composite;

  public Editor(Composite composite, Attr<MultiCaret> caretsId) {
    this.caretsId = caretsId;
    this.composite = composite;
  }

  public Editor withComposite(Composite composite) {
    return new Editor(composite, caretsId);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Editor editor = (Editor)o;
    return Objects.equals(caretsId, editor.caretsId) &&
           Objects.equals(composite, editor.composite);
  }

  @Override
  public int hashCode() {
    return Objects.hash(caretsId, composite);
  }

  public MultiCaret getCarets() {
    return composite.get(caretsId);
  }

  public Editor putCarets(MultiCaret multiCaret) {
    return withComposite(composite.assoc(caretsId, multiCaret));
  }

  public Editor log(Op op, Object arg, Edit edit) {
    return this.withComposite(this.composite.log(op, arg, edit));
  }

  public Editor edit(Edit edit) {
    return this.withComposite(this.composite.edit(edit));
  }
}
