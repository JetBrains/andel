package andel;

import clojure.lang.IFn.LLL;

public class Text {

  public static class TextMetrics {
    public long length = 0;
    public long geometricLength = 0;
    public long linesCount = 0;
    public long charsCount = 0;
    public long newlinePrefixLength = 0;
    public long newlineSuffixLength = 0;
    public long maxLineLength = 0;
  }

  public static TextMetrics metricsTo(String str, LLL pred) {
    TextMetrics metrics = new TextMetrics();
    long prevLineOffset = 0;

    while (pred.invokePrim(metrics.charsCount, metrics.geometricLength) == 0) {
      int codepoint = str.codePointAt((int) metrics.charsCount);

      if (codepoint == '\n') {
        prevLineOffset = metrics.length;
      }

      metrics.maxLineLength = Math.max(Math.max(metrics.maxLineLength,
                                                metrics.newlinePrefixLength),
                                       metrics.length - prevLineOffset);
      metrics.newlinePrefixLength = metrics.linesCount == 0 ? metrics.length : metrics.newlinePrefixLength;
      metrics.length += 1;
      metrics.geometricLength += codepoint == '\t' ? 4 : 1;
      metrics.linesCount += codepoint == '\n' ? 1 : 0;
      metrics.charsCount += Character.charCount(codepoint);
    }

    long newlineSuffixLength = metrics.length - prevLineOffset - metrics.linesCount == 0 ? 1 : 0;

    metrics.newlinePrefixLength = metrics.linesCount == 0 ? metrics.length : metrics.newlinePrefixLength;
    metrics.maxLineLength = Math.max(metrics.maxLineLength, newlineSuffixLength);
    metrics.newlineSuffixLength = newlineSuffixLength;

    return metrics;
  }

}