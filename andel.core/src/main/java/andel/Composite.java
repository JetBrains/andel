package andel;

import andel.text.Text;
import io.lacuna.bifurcan.IEntry;
import io.lacuna.bifurcan.List;
import io.lacuna.bifurcan.Map;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;
import java.util.function.Function;

public class Composite {
  public static final Attr<List<Log.Entry>> LOG = new Attr<>("LOG");
  public static final Attr<Object> EDIT_AUTHOR = new Attr<>("AUTHOR");

  public final Map<Attr<?>, Component> components;
  public final Text text;
  public final Log log;
  public final Map meta;

  public Composite(Map<Attr<?>, Component> components, Text text, Map meta) {
    this(components, text, meta, new Log());
  }

  public Composite(Map<Attr<?>, Component> components, Text text, Map meta, Log log) {
    this.components = components;
    this.text = text;
    this.meta = meta;
    this.log = log;
  }

  public Composite edit(Edit edit) {
    Text textAfter = this.text.edit(edit);
    Map<Attr<?>, Component> components = this.components
      .mapValues((key, component) -> component.edit(this.text, textAfter, edit));
    return new Composite(components, textAfter, this.meta);
  }

  public <T extends Component> Composite assoc(Attr<T> key, T component) {
    return new Composite(this.components.put(key, component),
                         this.text,
                         this.meta);
  }

  public <T extends Component> T get(Attr<T> key) {
    return (T) components.get(key, null);
  }

  public <T extends Component> Collection<Attr<T>> getComponentsOfType(Class<T> componentClass) {
    ArrayList<Attr<T>> result = new ArrayList<Attr<T>>();
    for (IEntry<Attr<?>, Component> component : components) {
      if (componentClass.isInstance(component.value())) {
        result.add((Attr<T>)component.key());
      }
    }
    return result;
  }

  @SuppressWarnings("unchecked")
  public Composite log(Op op, Object arg, Edit edit) {
    return new Composite(components, text, meta, this.log.add(op, arg, edit, meta.get(EDIT_AUTHOR, null)));
  }

  public Composite varyMeta(Function<Map, Map> f) {
    return new Composite(this.components, this.text, f.apply(this.meta));
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Composite composite = (Composite)o;
    return Objects.equals(components, composite.components) &&
           Objects.equals(text, composite.text) &&
           Objects.equals(meta, composite.meta) &&
           Objects.equals(log, composite.log);
  }

  @Override
  public int hashCode() {
    return Objects.hash(components, text, meta, log);
  }
}
