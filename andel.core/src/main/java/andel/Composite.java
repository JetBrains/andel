package andel;

import andel.text.Text;

import java.util.Map;

public interface Composite {
    Composite edit(Edit edit);

    Text getText();

    Map<Object, Component> components();

    Composite assocComponent(Object key, Component c);
}
