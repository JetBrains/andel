package andel;

import clojure.lang.IFn.LDL;
import java.awt.Font;
import java.awt.geom.AffineTransform;
import java.awt.font.FontRenderContext;

public class Text {

  private static FontRenderContext context = new FontRenderContext(new AffineTransform(), true, true);
  private static Font font = new Font("Fira Code", Font.PLAIN, 14);
  private static double defaultWidth = font.getStringBounds("x", context).getWidth();

  public static class TextMetrics {
    public long length = 0;
    public double geometricLength = 0;
    public long linesCount = 0;
    public long charsCount = 0;
    public double newlinePrefixGeomLength = 0;
    public double newlineSuffixGeomLength = 0;
    public double maxLineLength = 0;
  }

  public static TextMetrics metricsTo(String str, LDL pred) {
    TextMetrics metrics = new TextMetrics();
    double prevLineGeomOffset = 0;

    while (pred.invokePrim(metrics.length, metrics.geometricLength) == 0) {
      int codepoint = str.codePointAt((int) metrics.charsCount);

      double width = defaultWidth;
      if (Character.charCount(codepoint) > 1) {
        width = font.getStringBounds(Character.toChars(codepoint), 0, 2, context).getWidth();
      }

      if (codepoint == '\n') {
        metrics.maxLineLength = Math.max(Math.max(metrics.maxLineLength,
                                                  metrics.newlinePrefixGeomLength),
                                         metrics.length - prevLineGeomOffset);
        metrics.newlinePrefixGeomLength = metrics.linesCount == 0 ? metrics.geometricLength : metrics.newlinePrefixGeomLength;
        prevLineGeomOffset = metrics.geometricLength;
      }

      metrics.length += 1;
      metrics.geometricLength += codepoint == '\t' ? 4 * width : width;
      metrics.linesCount += codepoint == '\n' ? 1 : 0;
      metrics.charsCount += Character.charCount(codepoint);
    }

    double newlineSuffixGeomLength = metrics.geometricLength - prevLineGeomOffset - (metrics.linesCount == 0 ? 1 : 0);

    metrics.newlinePrefixGeomLength = metrics.linesCount == 0 ? metrics.length : metrics.newlinePrefixGeomLength;
    metrics.maxLineLength = Math.max(metrics.maxLineLength, newlineSuffixGeomLength);
    metrics.newlineSuffixGeomLength = newlineSuffixGeomLength;

    return metrics;
  }

}