package andel;

import andel.text.TextRope;

import java.util.Map;

public interface Composite {
    Composite edit(Edit edit);

    TextRope getText();

    Map<Object, Component> components();

    Composite assocComponent(Object key, Component c);
}
