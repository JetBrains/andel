package andel.impl.util;

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
    return this.size;
  }

  public long get(int idx) {
    return this.buffer[idx];
  }

  public void set(int idx, long v) {
    assert idx < this.size;
    this.buffer[idx] = v;
  }

  public void add(long v) {
    if (this.buffer.length == this.size) {
      this.buffer = Arrays.copyOf(this.buffer, this.buffer.length * 2);
    }
    this.buffer[this.size] = v;
    this.size += 1;
  }

  public void add(int idx, long v) {
    assert idx <= this.size;
    if (this.buffer.length == this.size) {
      long[] newBuffer = new long[this.size * 2];
      System.arraycopy(this.buffer, 0, newBuffer, 0, idx);
      System.arraycopy(this.buffer, idx, newBuffer, idx + 1, this.size - idx);
      buffer = newBuffer;
    }
    else {
      System.arraycopy(this.buffer, idx, this.buffer, idx + 1, this.size - idx);
    }
    this.buffer[idx] = v;
    this.size += 1;
  }

  public void removeRange(int from, int to) {
    if (to > this.size) {
      throw new IndexOutOfBoundsException();
    }
    if (to < this.size) {
      System.arraycopy(this.buffer, to, this.buffer, from, this.size - to);
    }
    size -= to - from;
  }

  public void remove(int idx) {
    removeRange(idx, idx + 1);
  }

  @Override
  public String toString() {
    return Arrays.toString(Arrays.copyOfRange(this.buffer, 0, size));
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
    return Arrays.binarySearch(this.buffer, 0, this.size, key);
  }

  public int binarySearch(int idx, long key) {
    return Arrays.binarySearch(this.buffer, idx, this.size, key);
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
