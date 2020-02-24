package andel;

import andel.text.Text;

import java.util.Objects;

public class Utils {
    private Utils() {}

    public static class Position {
        public final long line;
        public final long column;

        public Position(long line, long column) {
            this.line = line;
            this.column = column;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Position position = (Position)o;
            return line == position.line &&
                   column == position.column;
        }

        @Override
        public int hashCode() {
            return Objects.hash(line, column);
        }

        @Override
        public String toString() {
            return "Position{" +
                   "line=" + line +
                   ", column=" + column +
                   '}';
        }
    }

    public static long offsetToLine(long offset, Text text) {
        return text.zipper().scanToCodepoint(offset).lineNumber();
    }

    public static long lineStartOffset(long line, Text text) {
        return text.zipper().scanToLineStart(line).codePointsOffset();
    }

    public static Position offsetToPosition(long offset, Text text) {
        final long line = offsetToLine(offset, text);
        final long lineStartOffset = lineStartOffset(line, text);
        final long column = offset - lineStartOffset;
        return new Position(line, column);
    }
}
