package andel;

import andel.carets.Carets;
import io.lacuna.bifurcan.Map;

public class Editor {

  public static final Attr<Carets> CARETS = new Attr<>("CARETS");

  public final Map identities;

  public Editor(Map identities) {
    this.identities = identities;
  }

  @SuppressWarnings({"unchecked", "OptionalGetWithoutIsPresent"})
  public <T extends Component> T get(Composite composite, Attr<T> attr) {
    return (T)composite.components.get(identities.get(attr).get(), null);
  }

  @SuppressWarnings({"unchecked", "OptionalGetWithoutIsPresent"})
  public <T extends Component> Composite assoc(Composite composite, Attr<T> attr, T val) {
    return composite.assoc(this.identities.get(attr).get(), val);
  }

  public Carets getCarets(Composite composite) {
    return this.get(composite, CARETS);
  }

  public Composite putCarets(Composite composite, Carets carets) {
    return this.assoc(composite, CARETS, carets);
  }
}
