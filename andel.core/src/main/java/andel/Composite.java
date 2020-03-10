package andel;

import andel.text.Text;
import io.lacuna.bifurcan.Map;

import java.util.Objects;

public class Composite {
    public final Map<Object, Component> components;
    public final Log log;
    public final Text text;
    public final Map meta;

    public Composite(Map<Object, Component> components, Text text, Log log, Map meta) {
        this.components = components;
        this.text = text;
        this.meta = meta;
        this.log = log;
    }

    public Composite edit(Edit edit) {
        Text textAfter = this.text.edit(edit);
        Map<Object, Component> components = this.components.mapValues((key, component) ->
                component.edit(this.text, textAfter, edit));
        return new Composite(components, textAfter, log, this.meta);
    }

    public Component getComponent(Object key) {
        return components.get(key, null);
    }

    public Composite assoc(Object key, Component component) {
        return new Composite(this.components.put(key, component), this.text, this.log, this.meta);
    }

    public Composite log(Op op, Object arg, Edit edit, Object editsAuthor) {
        return new Composite(
                this.components,
                this.text,
                this.log.add(op, arg, edit, editsAuthor),
                this.meta);
    }

    public Composite withLog(Log log) {
        return new Composite(this.components, this.text, log, this.meta);
    }

    public Composite withMeta(Map meta) {
        return new Composite(this.components, this.text, this.log, meta);
    }

    public Composite withComponents(Map<Object, Component> components) {
        return new Composite(components, this.text, this.log, this.meta);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Composite composite = (Composite) o;
        return Objects.equals(this.components, composite.components) &&
                Objects.equals(this.text, composite.text) &&
                Objects.equals(this.meta, composite.meta) &&
                Objects.equals(this.log, composite.log);
    }

    @Override
    public int hashCode() {
        return Objects.hash(components, text, meta, log);
    }
}
