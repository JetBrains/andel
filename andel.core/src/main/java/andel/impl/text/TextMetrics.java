package andel.impl.text;

public final class TextMetrics {

  public long length;
  public long geometricLength;
  public long newlinesCounts;
  public long charsCount;
  public long newlinePrefixGeomLength;
  public long newlineSuffixGeomLength;
  public long maxLineLength;

  public TextMetrics(long length,
                     long geometricLength,
                     long linesCount,
                     long charsCount,
                     long newlinePrefixGeomLength,
                     long newlineSuffixGeomLength,
                     long maxLineLength) {
    this.length = length;
    this.geometricLength = geometricLength;
    this.newlinesCounts = linesCount;
    this.charsCount = charsCount;
    this.newlinePrefixGeomLength = newlinePrefixGeomLength;
    this.newlineSuffixGeomLength = newlineSuffixGeomLength;
    this.maxLineLength = maxLineLength;
  }

  public TextMetrics() {
    this.length = 0;
    this.geometricLength = 0;
    this.newlinesCounts = 0;
    this.charsCount = 0;
    this.newlinePrefixGeomLength = 0;
    this.newlineSuffixGeomLength = 0;
    this.maxLineLength = 0;
  }

  public void merge(TextMetrics other){
    long length = this.length + other.length;
    long geomLength = this.geometricLength + other.geometricLength;
    long linesCount = this.newlinesCounts + other.newlinesCounts;
    long charsCount = this.charsCount + other.charsCount;
    long newlinePrefixGeomLength = this.newlinesCounts == 0
                                   ? this.newlinePrefixGeomLength + other.newlinePrefixGeomLength
                                   : this.newlinePrefixGeomLength;
    long maxLineLength = Math.max(Math.max(this.maxLineLength, other.maxLineLength),
                                  this.newlineSuffixGeomLength + other.newlinePrefixGeomLength);
    long newlineSuffixGeomLength = other.newlinesCounts == 0
                                   ? this.newlineSuffixGeomLength + other.newlineSuffixGeomLength
                                   : other.newlineSuffixGeomLength;
    this.length = length;
    this.geometricLength = geomLength;
    this.newlinesCounts = linesCount;
    this.charsCount = charsCount;
    this.newlinePrefixGeomLength = newlinePrefixGeomLength;
    this.newlineSuffixGeomLength = newlineSuffixGeomLength;
    this.maxLineLength = maxLineLength;
  }

  public TextMetrics add(TextMetrics other) {
    TextMetrics metrics = new TextMetrics(this.length,
                                          this.geometricLength,
                                          this.newlinesCounts,
                                          this.charsCount,
                                          this.newlinePrefixGeomLength,
                                          this.newlineSuffixGeomLength,
                                          maxLineLength);
    metrics.merge(other);
    return metrics;
  }

  @Override
  public String toString() {
    return "TextMetrics{" +
           "length=" + length +
           ", geometricLength=" + geometricLength +
           ", newlinesCounts=" + newlinesCounts +
           ", charsCount=" + charsCount +
           ", newlinePrefixGeomLength=" + newlinePrefixGeomLength +
           ", newlineSuffixGeomLength=" + newlineSuffixGeomLength +
           ", maxLineLength=" + maxLineLength +
           '}';
  }
}
