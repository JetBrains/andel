package andel;

import java.util.Arrays;

public class LongArrayList {
  private long[] buffer;
  private int size;

  public LongArrayList() {
    this(8);
  }

  public LongArrayList(int capacity) {
    buffer = new long[capacity];
    size = 0;
  }

  public LongArrayList(long[] array) {
    buffer = array;
    size = array.length;
  }

  public int size() {
    return size;
  }

  public long get(int idx) {
    return buffer[idx];
  }

  public void set(int idx, long v) {
    assert idx < size;
    buffer[idx] = v;
  }

  public void add(long v) {
    if (buffer.length == size) {
      this.buffer = Arrays.copyOf(buffer, buffer.length * 2);
    }
    this.buffer[this.size] = v;
    this.size += 1;
  }

  public void add(int idx, long v) {
    assert idx <= size;
    if (buffer.length == size) {
      long[] newBuffer = new long[size * 2];
      System.arraycopy(buffer, 0, newBuffer, 0, idx);
      System.arraycopy(buffer, idx, newBuffer, idx + 1, size - idx);
      buffer = newBuffer;
    }
    else {
      System.arraycopy(buffer, idx, buffer, idx + 1, size - idx);
    }
    this.buffer[idx] = v;
    this.size += 1;
  }

  public void removeRange(int from, int to) {
    if (to > this.size) {
      throw new IndexOutOfBoundsException();
    }
    if (to < this.size) {
      System.arraycopy(buffer, to, buffer, from, size - to);
    }
    size -= to - from;
  }

  public void remove(int idx) {
    removeRange(idx, idx + 1);
  }

  @Override
  public String toString() {
    return Arrays.toString(Arrays.copyOfRange(buffer, 0, size));
  }

  public LongArrayList copy(int capacity) {
    LongArrayList r = new LongArrayList();
    r.buffer = Arrays.copyOf(this.buffer, capacity);
    r.size = this.size;
    return r;
  }

  public LongArrayList copy() {
    return this.copy(this.size);
  }

  public int binarySearch(long key) {
    return Arrays.binarySearch(buffer, 0, size, key);
  }

  public int binarySearch(int idx, long key) {
    return Arrays.binarySearch(buffer, idx, size, key);
  }

  public LongArrayList subList(int from, int to) {
    LongArrayList r = new LongArrayList(to - from);
    System.arraycopy(this.buffer, from, r.buffer, 0, to - from);
    r.size = to - from;
    return r;
  }

  public int indexOf(long key) {
    for (int i = 0; i < this.size; i++) {
      if (this.buffer[i] == key) {
        return i;
      }
    }
    return -1;
  }
}
