package andel;

public class Experimental {
//
//    interface Metric {
//
//        interface LLL {
//
//        }
//    }
//
//    public static class MetricsContainer {
//        Object[] lines;
//
//        private int metricIdx(Metric m) {
//            switch (System.identityHashCode(m)) {
//                case 0:
//                    return 0;
//            }
//            throw new IllegalArgumentException();
//        }
//
//        public void write(Metric m, int idx, long l1, long l2, long l3) {
//            int midx = metricIdx(m);
//            long[] l1s = (long[]) lines[midx];
//            long[] l2s = (long[]) lines[midx + 1];
//            long[] l3s = (long[]) lines[midx + 2];
//            l1s[idx] = l1;
//            l2s[idx] = l2;
//            l3s[idx] = l3;
//        }
//    }
//
//
//    public static class CharsCount {
//        public long read(MetricsContainer container, int idx) {
//
//        }
//
//        public void write(MetricsContainer container, int idx, String data) {
//
//        }
//    }

    

    public static class Metrics {
        public long length;
        public long geometricLength;
        public long newlinesCount;
        public long charsCount;
        public long newlinePrefixGeomLength;
        public long newlineSuffixGeomLength;
        public long maxLineLength;
    }

    public static class Flags {
        public static final int CODEPOINTS = 1;
        public static final int GEOMETRY = 2;
        public static final int NEWLINES = 4;
        public static final int CHARS = 8;
        public static final int MAX_LINE_LENGTH = 16;

        public static final int ALL = 31;

        public static boolean isSet(int flags, int flag){
            return (flags & flag) == flag;
        }
    }

    

    // public static class MetricsVector {

    //     public final long[] buffer;
    //     public final byte codepointsOffset = -1;
    //     public final byte geometryOffset = -1;
    //     public final byte newlinesOffset = -1;
    //     public final byte charsOffset = -1;
    //     public final byte maxLineLengthOffset = -1;
    //     public final byte newlinePrefixOffset = -1;
    //     public final byte newlineSuffixOffset = -1;
            
    //     public MetricsVector(int capacity, int metrics){
    //         int used = 0;
    //         if (Flags.isSet(metrics, Flags.CODEPOINTS)){
    //             codepointOffset = -1;
    //             used += 1;
    //         }
    //         if (Flags.isSet(metrics, Flags.GEOMETRY)){
    //             this.geometry = new long[capacity];
    //         }
    //         if (Flags.isSet(metrics, Flags.NEWLINES)){
    //             this.newlinex = new long[capacity];
    //         }
    //         if (Flags.isSet(metrics, Flags.CHARS)){
    //             this.chars = new long[capacity];
    //         }
    //         if (Flags.isSet(metrics, Flags.MAX_LINE_LENGTH)){
    //             this.maxLineLength = new long[capacity];
    //             this.newlinePrefix = new long[capacity];
    //             this.newlineSuffix = new long[capacity];
    //         }
    //     }

    //     public long get(int metric, int idx){
            
    //     }

    //     public void set(int metric, int idx, long value){
            
    //     }
    // }

    public interface StringReducer<T> {
        void consume(T m, int codepoint);
    }

    public static class MetricsReducer implements StringReducer<Metrics> {
        @Override
        public void consume(Metrics m, int codepoint) {
            if (codepoint == '\n') {
                m.maxLineLength = Math.max(Math.max(m.maxLineLength, m.newlinePrefixGeomLength), m.newlineSuffixGeomLength);
                m.newlineSuffixGeomLength = 0;
                m.newlinesCount += 1;
            } else {
                int gl = codepoint == '\t' ? 4 : 1;
                if (m.newlinesCount == 0) {
                    m.newlinePrefixGeomLength += gl;
                }
                m.newlineSuffixGeomLength += gl;
                m.geometricLength += gl;
            }
            m.length += 1;
            m.charsCount += Character.charCount(codepoint);
        }
    }
}
