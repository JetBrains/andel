package andel;

import andel.text.TextRope;

public interface Component {
    Component edit(TextRope before, TextRope after, Edit edit);
}
