package andel;

import andel.text.Text;
import io.lacuna.bifurcan.List;
import io.lacuna.bifurcan.Map;

import java.util.Objects;
import java.util.function.Function;

public class Composite {
  public static final Attr<List<LogEntry>> LOG = new Attr<>("LOG");

  public final Map<Object, Component> components;
  public final Text text;
  public final Map meta;

  public Composite(Map<Object, Component> components, Text text, Map meta) {
    this.components = components;
    this.text = text;
    this.meta = meta;
  }

  public Composite edit(Edit edit) {
    Text textAfter = this.text.edit(edit);
    Map<Object, Component> components = this.components
      .mapValues((key, component) -> component.edit(this.text, textAfter, edit));
    return new Composite(components, textAfter, this.meta);
  }

  public Composite assoc(Object key, Component component) {
    return new Composite(this.components.put(key, component),
                         this.text,
                         this.meta);
  }

  @SuppressWarnings("unchecked")
  public Composite log(LogEntry entry) {
    return this.varyMeta((m) -> m.update(LOG, l -> (l == null ? new List() : (List)l).addLast(entry)));
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
           Objects.equals(meta, composite.meta);
  }

  @Override
  public int hashCode() {
    return Objects.hash(components, text, meta);
  }
}
