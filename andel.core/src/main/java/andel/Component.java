package andel;

import andel.text.Text;

public interface Component {
    Component edit(Text before, Text after, Edit edit);
}
