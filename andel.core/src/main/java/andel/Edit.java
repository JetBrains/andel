package andel;

import java.util.Arrays;
import java.util.Objects;

public class Edit {
    public final Object[] ops;

    public Edit(Object[] ops) {
        this.ops = ops;
    }

    public static boolean isIdentity(Edit edit) {
        for (Object op : edit.ops) {
            if (!(op instanceof Retain || Objects.equals(op, Delete.NOOP) || Objects.equals(op, Insert.NOOP))) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Edit edit = (Edit) o;
        return Arrays.equals(ops, edit.ops);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(ops);
    }

    @Override
    public String toString() {
        return Arrays.toString(ops);
    }

    public static class Retain {
        final long count;

        public Retain(long count) {
            this.count = count;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Retain retain = (Retain) o;
            return count == retain.count;
        }

        @Override
        public int hashCode() {
            return Objects.hash(count);
        }

        @Override
        public String toString() {
            return "[retain " + count + ']';
        }
    }

    public static class Delete {

        public static final Delete NOOP = new Delete("");

        final String text;

        public Delete(String text) {
            this.text = text;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Delete delete = (Delete) o;
            return Objects.equals(text, delete.text);
        }

        @Override
        public int hashCode() {
            return Objects.hash(text);
        }

        @Override
        public String toString() {
            return "[delete \"" + text + "\"";
        }
    }

    public static class Insert {

        public static final Insert NOOP = new Insert("");

        final String text;

        public Insert(String text) {
            this.text = text;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Insert insert = (Insert) o;
            return Objects.equals(text, insert.text);
        }

        @Override
        public int hashCode() {
            return Objects.hash(text);
        }

        @Override
        public String toString() {
            return "[insert \"" + text + "\"";
        }
    }
}
