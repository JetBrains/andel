package andel;

import andel.carets.MultiCaret;

import java.util.Objects;

public class Editor {
  public final Object caretsKey;
  public final Object editsAuthor;
  public final Composite composite;

  public Editor(Object caretsKey, Object editsAuthor, Composite composite) {
    this.caretsKey = caretsKey;
    this.editsAuthor = editsAuthor;
    this.composite = composite;
  }

  public Editor withComposite(Composite composite) {
    return new Editor(this.caretsKey, editsAuthor, composite);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Editor editor = (Editor)o;
    return Objects.equals(caretsKey, editor.caretsKey) &&
           Objects.equals(composite, editor.composite) &&
           Objects.equals(editsAuthor, editor.editsAuthor);
  }

  @Override
  public int hashCode() {
    return Objects.hash(caretsKey, composite, editsAuthor);
  }

  public MultiCaret getCarets() {
    return (MultiCaret)this.composite.getComponent(caretsKey);
  }

  public Editor putCarets(MultiCaret carets) {
    return new Editor(this.caretsKey, this.editsAuthor, this.composite.assoc(this.caretsKey, carets));
  }

  public Editor log(Op op, Object arg, Edit edit) {
    return this.withComposite(this.composite.log(op, arg, edit, this.editsAuthor));
  }

  public Editor edit(Edit edit) {
    return this.withComposite(this.composite.edit(edit));
  }
}
