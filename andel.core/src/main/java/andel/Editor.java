package andel;

import andel.carets.MultiCaret;
import io.lacuna.bifurcan.Map;

import java.util.Objects;

public class Editor {

  public static final Attr<MultiCaret> CARETS = new Attr<>("CARETS");

  public final Map identities;
  public final Composite composite;

  public Editor(Map identities, Composite composite) {
    this.identities = identities;
    this.composite = composite;
  }

  public Editor withComposite(Composite composite) {
    return new Editor(this.identities, composite);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Editor editor = (Editor)o;
    return Objects.equals(identities, editor.identities) &&
           Objects.equals(composite, editor.composite);
  }

  @Override
  public int hashCode() {
    return Objects.hash(identities, composite);
  }

  @SuppressWarnings({"unchecked", "OptionalGetWithoutIsPresent"})
  public <T extends Component> T get(Attr<T> attr) {
    return (T)this.composite.getComponent(identities.get(attr).get());
  }

  @SuppressWarnings({"unchecked", "OptionalGetWithoutIsPresent"})
  public <T extends Component> Editor assoc(Attr<T> attr, T val) {
    return this.withComposite(this.composite.assoc(this.identities.get(attr).get(), val));
  }

  public MultiCaret getCarets() {
    return this.get(CARETS);
  }

  public Editor putCarets(MultiCaret carets) {
    return this.assoc(CARETS, carets);
  }

  public Editor log(Op op, Object arg, Edit edit) {
    return this.withComposite(this.composite.log(op, arg, edit));
  }

  public Editor edit(Edit edit) {
    return this.withComposite(this.composite.edit(edit));
  }
}
