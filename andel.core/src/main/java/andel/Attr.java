package andel;

import java.util.Objects;

public class Attr<T> {
  public final Object key;

  public Attr(Object key) {
    this.key = key;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Attr<?> attr = (Attr<?>)o;
    return Objects.equals(key, attr.key);
  }

  @Override
  public int hashCode() {
    return Objects.hash(key);
  }

  @Override
  public String toString() {
    return "Attr{" +
           "key=" + key +
           '}';
  }
}
