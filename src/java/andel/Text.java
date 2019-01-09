package andel;

import clojure.lang.IFn.LLLL;

public class Text {

  public static class TextMetrics {
    public long length = 0;
    public long geometricLength = 0;
    public long linesCount = 0;
    public long charsCount = 0;
    public long newlinePrefixGeomLength = 0;
    public long newlineSuffixGeomLength = 0;
    public long maxLineLength = 0;
  }

  public static TextMetrics metricsTo(String str, LLLL pred) {
    TextMetrics metrics = new TextMetrics();
    long prevLineGeomOffset = 0;

    while (pred.invokePrim(metrics.length, metrics.geometricLength, metrics.charsCount) == 0) {
      int codepoint = str.codePointAt((int) metrics.charsCount);

      if (codepoint == '\n') {
        metrics.maxLineLength = Math.max(Math.max(metrics.maxLineLength,
                                                  metrics.newlinePrefixGeomLength),
                                         metrics.length - prevLineGeomOffset);
        metrics.newlinePrefixGeomLength = metrics.linesCount == 0 ? metrics.geometricLength : metrics.newlinePrefixGeomLength;
        prevLineGeomOffset = metrics.geometricLength;
      }

      metrics.length += 1;
      metrics.geometricLength += codepoint == '\t' ? 4 : 1;
      metrics.linesCount += codepoint == '\n' ? 1 : 0;
      metrics.charsCount += Character.charCount(codepoint);
    }

    long newlineSuffixGeomLength = metrics.geometricLength - prevLineGeomOffset - (metrics.linesCount == 0 ? 1 : 0);

    metrics.newlinePrefixGeomLength = metrics.linesCount == 0 ? metrics.length : metrics.newlinePrefixGeomLength;
    metrics.maxLineLength = Math.max(metrics.maxLineLength, newlineSuffixGeomLength);
    metrics.newlineSuffixGeomLength = newlineSuffixGeomLength;

    return metrics;
  }

}