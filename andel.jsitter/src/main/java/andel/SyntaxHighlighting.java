package andel;

import jsitter.api.ApiKt;
import jsitter.api.Terminal;
import jsitter.api.Zipper;

public class SyntaxHighlighting {

    public interface SyntaxHighlighter<T> {
        T highlight(jsitter.api.Zipper zipper);
    }

    public static <T> Intervals<T> highlightSyntax(Intervals<T> intervalsTree,
                                                   jsitter.api.Tree syntaxTree,
                                                   Rope.Tree<Text.TextMetrics, String> textTree,
                                                   int rangeStartByte,
                                                   int rangeEndByte,
                                                   SyntaxHighlighter<T> highlighter) {
        Intervals.EditingContext ctx = new Intervals.EditingContext(
                intervalsTree.nextInnerId,
                intervalsTree.maxChildren,
                intervalsTree.parentsMap.linear());
        jsitter.api.Zipper syntaxZ = nextToHighlight(syntaxTree.zipper(), rangeStartByte, rangeEndByte);
        Rope.Zipper<Text.TextMetrics, String> textZ = Text.scanToCharOffset(Text.zipper(textTree), syntaxZ.getByteOffset() / 2);
        long fromOffset = Text.offset(textZ);
        long toOffset = Text.offset(Text.scanToCharOffset(textZ, rangeEndByte / 2));
        Intervals.Zipper intervalsZ = Intervals.skipToOffset(Intervals.Zipper.create(intervalsTree.openRoot, ctx, true), 2 * fromOffset);
        boolean hasNext = Intervals.Zipper.hasNext(intervalsZ);
        textZ = Rope.toTransient(textZ);
        boolean intervalsDone = !hasNext;
        while (true) {
            int action = getAction(highlighter, syntaxZ, textZ, intervalsZ, intervalsDone);
            switch (action) {
                case RETURN: {
                    return new Intervals<>(
                            ctx.maxChildren,
                            Intervals.Zipper.root(intervalsZ),
                            intervalsTree.closedRoot,
                            ctx.parentsMap.forked(),
                            ctx.nextId);
                }
                case INSERT: {
                    T highlight = highlighter.highlight(syntaxZ);
                    if (highlight != null) {
                        long syntaxTo = Text.offset(Text.scanToCharOffset(Rope.toPersistent(textZ), (syntaxZ.getByteOffset() + syntaxZ.getByteSize()) / 2));
                        intervalsZ = Intervals.Zipper.insert(intervalsZ, Text.offset(textZ), syntaxTo, false, false, highlight);
                    }
                    syntaxZ = nextToHighlight(syntaxZ, rangeStartByte, rangeEndByte);
                    textZ = syntaxZ != null ? Text.scanToCharOffset(textZ, syntaxZ.getByteOffset() / 2) : null;
                    break;
                }
                case SKIP: {
                    syntaxZ = nextToHighlight(syntaxZ, rangeStartByte, rangeEndByte);
                    textZ = syntaxZ != null ? Text.scanToCharOffset(textZ, syntaxZ.getByteOffset() / 2) : null;
                    if (Intervals.Zipper.hasNext(intervalsZ)) {
                        intervalsZ = Intervals.Zipper.next(intervalsZ);
                        intervalsDone = toOffset <= Intervals.Zipper.from(intervalsZ);
                    } else {
                        intervalsDone = true;
                    }
                    break;
                }
                case REMOVE: {
                    intervalsZ = Intervals.Zipper.remove(intervalsZ);
                    if (Intervals.Zipper.hasNext(intervalsZ)) {
                        intervalsZ = Intervals.Zipper.next(intervalsZ);
                        intervalsDone = toOffset <= Intervals.Zipper.from(intervalsZ);
                    } else {
                        intervalsDone = true;
                    }
                }
            }
        }
    }

    private static final int RETURN = 0;
    private static final int INSERT = 1;
    private static final int SKIP = 2;
    private static final int REMOVE = 3;

    private static <T> int getAction(SyntaxHighlighter<T> highlighter, Zipper syntaxZ, Rope.Zipper<Text.TextMetrics, String> textZ, Intervals.Zipper intervalsZ, boolean intervalsDone) {
        if (syntaxZ == null) {
            if (intervalsDone) {
                return RETURN;
            } else {
                return REMOVE;
            }
        } else {
            if (intervalsDone) {
                return INSERT;
            } else if (Text.offset(textZ) == Intervals.Zipper.from(intervalsZ) &&
                    highlighter.highlight(syntaxZ) == Intervals.Zipper.data(intervalsZ) &&
                    Text.offset(Text.scanToCharOffset(Rope.toPersistent(textZ), (syntaxZ.getByteOffset() + syntaxZ.getByteSize()) / 2)) == Intervals.Zipper.to(intervalsZ)) {
                return SKIP;
            } else if (Text.offset(textZ) < Intervals.Zipper.from(intervalsZ)) {
                return INSERT;
            } else {
                return REMOVE;
            }
        }
    }

    private static jsitter.api.Zipper<?> nextToHighlight(Zipper<?> zipper, int rangeStartByte, int rangeEndByte) {
        jsitter.api.Zipper<?> syntaxZ = ApiKt.next(zipper);
        while (syntaxZ != null) {
            int fromByte = syntaxZ.getByteOffset();
            int toByte = syntaxZ.getByteOffset() + syntaxZ.getByteSize();
            if (toByte <= rangeStartByte) {
                syntaxZ = ApiKt.skip(syntaxZ);
            } else if (rangeStartByte <= fromByte && toByte <= rangeEndByte) {
                return syntaxZ;
            } else if (rangeEndByte <= fromByte){
                return null;
            } else if (syntaxZ.getNodeType() instanceof Terminal){
                return syntaxZ;
            } else {
                syntaxZ = ApiKt.next(syntaxZ);
            }
        }
        return null;
    }
}
