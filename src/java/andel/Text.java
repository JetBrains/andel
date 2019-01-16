package andel;

import clojure.lang.IFn.LLLL;

public class Text {

  public static class TextMetrics {

    public final long length;
    public final long geometricLength;
    public final long linesCount;
    public final long charsCount;
    public final long newlinePrefixGeomLength;
    public final long newlineSuffixGeomLength;
    public final long maxLineLength;

    public TextMetrics(long length,
                       long geometricLength,
                       long linesCount,
                       long charsCount,
                       long newlinePrefixGeomLength,
                       long newlineSuffixGeomLength,
                       long maxLineLength) {
      this.length = length;
      this.geometricLength = geometricLength;
      this.linesCount = linesCount;
      this.charsCount = charsCount;
      this.newlinePrefixGeomLength = newlinePrefixGeomLength;
      this.newlineSuffixGeomLength = newlineSuffixGeomLength;
      this.maxLineLength = maxLineLength;
    }
  }

  public static TextMetrics metricsTo(String str, LLLL pred) {
    long prevLineGeomOffset = 0;
    long maxLineLength = 0;
    long newlinePrefixGeomLength = 0;
    long codePointsCount = 0;
    int charsCount = 0;
    long geometricLength = 0;
    long linesCount = 0;

    while (pred.invokePrim(codePointsCount, geometricLength, charsCount) == 0) {
      int codepoint = str.codePointAt(charsCount);

      if (codepoint == '\n') {
        maxLineLength = Math.max(Math.max(maxLineLength, newlinePrefixGeomLength), codePointsCount - prevLineGeomOffset);
        newlinePrefixGeomLength = linesCount == 0 ? geometricLength : newlinePrefixGeomLength;
        prevLineGeomOffset = geometricLength;
        linesCount += 1;
      }

      codePointsCount += 1;
      geometricLength += codepoint == '\t' ? 4 : 1;

      charsCount += Character.charCount(codepoint);
    }

    long newlineSuffixGeomLength = geometricLength - prevLineGeomOffset - (linesCount == 0 ? 1 : 0);

    newlinePrefixGeomLength = linesCount == 0 ? codePointsCount : newlinePrefixGeomLength;
    maxLineLength = Math.max(maxLineLength, newlineSuffixGeomLength);

    return new TextMetrics(codePointsCount,
                           geometricLength,
                           linesCount,
                           charsCount,
                           newlinePrefixGeomLength,
                           newlineSuffixGeomLength,
                           maxLineLength);
  }
}