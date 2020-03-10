package andel;

import andel.carets.Caret;
import andel.carets.CaretMovement;
import andel.carets.MultiCaret;
import andel.text.Text;
import andel.text.TextZipper;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class CompositeController {
  public static Stream<Caret> selectCarets(Composite composite, Set<?> caretsIds) {
    return composite.components.values().stream()
      .filter((c) -> c instanceof MultiCaret)
      .flatMap((multiCaret) -> StreamSupport.stream(((MultiCaret)multiCaret).getCarets().spliterator(), false))
      .filter((caret) -> caretsIds.contains(caret.id))
      .sorted(MultiCaret.COMPARE_BY_OFFSET);
  }

  public static Composite updateCarets(Composite composite, Stream<Caret> carets) {
    List<Caret> caretList = carets.sorted(MultiCaret.COMPARE_BY_OFFSET).collect(Collectors.toList());
    return composite.withComponents(composite.components.mapValues((key, component) -> {
      if (component instanceof MultiCaret) {
        MultiCaret multiCaret = (MultiCaret)component;
        Iterable<Caret> iterable = caretList.stream().filter((caret) -> multiCaret.getCaret(caret.id) != null)::iterator;
        return multiCaret.merge(iterable);
      }
      else {
        return component;
      }
    }));
  }

  public static ArrayList<Object> createCaretsInsertionOperation(Composite composite, Map<Object, String> insertions) {
    Stream<Caret> carets = selectCarets(composite, insertions.keySet());
    long prevCaretOffset = 0;
    ArrayList<Object> ops = new ArrayList<>();
    for (Caret caret : (Iterable<Caret>)carets::iterator) {
      String text = insertions.get(caret.id);
      if (text != null) {
        ops.add(new Edit.Retain(caret.offset - prevCaretOffset));
        ops.add(new Edit.Insert(text));
        prevCaretOffset = caret.offset;
      }
    }
    Utils.retainToEnd(ops, composite.text.codePointsCount());
    return ops;
  }

  public static Composite insertBeforeCarets(Composite composite, Map<Object, String> insertions, Object editsAuthor) {
    ArrayList<Object> ops = createCaretsInsertionOperation(composite, insertions);
    Edit edit = new Edit(ops.toArray(), true);
    return composite.edit(edit).log(Op.INSERT_BEFORE_CARETS, insertions, edit, editsAuthor);
  }

  public static Composite insertAfterCarets(Composite composite, Map<Object, String> insertions, Object editsAuthor) {
    ArrayList<Object> ops = createCaretsInsertionOperation(composite, insertions);
    Edit edit = new Edit(ops.toArray(), false);
    return composite
      .edit(edit)
      .log(Op.INSERT_AFTER_CARETS, insertions, edit, editsAuthor);
  }

  public static Composite edit(Composite composite, Edit edit, Object editsAuthor) {
    return composite.edit(edit).log(Op.EDIT, edit, edit, editsAuthor);
  }

  public static Composite deleteSelectedText(Composite composite, Set<Object> caretIds, Object editsAuthor) {
    Iterable<Caret> carets = selectCarets(composite, caretIds)::iterator;
    long prevCaretSelectionEnd = 0;
    List<Object> ops = new ArrayList<>();
    TextZipper zipper = composite.text.zipper().asTransient();
    for (Caret caret : carets) {
      if (caretIds.contains(caret.id) && caret.hasSelection()) {
        ops.add(new Edit.Retain(caret.selectionMin() - prevCaretSelectionEnd));
        StringBuilder sb = new StringBuilder();
        zipper = zipper.scanToCodepoint(caret.selectionMin()).consume(caret.selectionMax() - caret.selectionMin(), sb::append);
        ops.add(new Edit.Delete(sb.toString()));
        prevCaretSelectionEnd = caret.selectionMax();
      }
    }
    Utils.retainToEnd(ops, composite.text.codePointsCount());
    Edit edit = new Edit(ops.toArray(), false);
    return composite.edit(edit).log(Op.DELETE_SELECTIONS, caretIds, edit, editsAuthor);
  }

  public static Composite deleteBeforeCarets(Map<Object, Long> caretIds, Composite composite, Object editsAuthor) {
    List<Object> ops = new ArrayList<>();
    long lastOffset = 0;
    TextZipper zipper = composite.text.zipper().asTransient();
    for (Caret caret : (Iterable<Caret>)selectCarets(composite, caretIds.keySet())::iterator) {
      Long count = caretIds.get(caret.id);
      if (count != null && count != 0) {
        long from = Math.max(caret.offset - count, 0);
        long to = caret.offset;
        StringBuilder sb = new StringBuilder();
        zipper = zipper.scanToCodepoint(from).consume(to - from, sb::append);
        ops.add(new Edit.Retain(from - lastOffset));
        ops.add(new Edit.Delete(sb.toString()));
        lastOffset = to;
      }
    }
    Utils.retainToEnd(ops, composite.text.codePointsCount());
    Edit edit = new Edit(ops.toArray(), false);
    return composite.edit(edit).log(Op.DELETE_BEFORE_CARETS, caretIds, edit, editsAuthor);
  }

  public static Composite deleteAfterCarets(Composite composite, Map<Object, Long> caretIds, Object editsAuthor) {
    Iterable<Caret> carets = selectCarets(composite, caretIds.keySet())::iterator;
    List<Object> ops = new ArrayList<>();
    long lastOffset = 0;
    TextZipper zipper = composite.text.zipper().asTransient();
    long codePointsCount = composite.text.codePointsCount();
    for (Caret caret : carets) {
      Long count = caretIds.get(caret.id);
      if (count != null && count != 0) {
        long from = caret.offset;
        long to = Math.min(caret.offset + count, codePointsCount);
        StringBuilder sb = new StringBuilder();
        zipper = zipper.scanToCodepoint(from).consume(to - from, sb::append);
        ops.add(new Edit.Retain(from - lastOffset));
        ops.add(new Edit.Delete(sb.toString()));
        lastOffset = to;
      }
    }
    Utils.retainToEnd(ops, composite.text.codePointsCount());
    Edit edit = new Edit(ops.toArray(), false);
    return composite.edit(edit).log(Op.DELETE_AFTER_CARETS, caretIds, edit, editsAuthor);
  }

  public static Composite dropSelections(Composite composite, Set<?> caretIds, Object editsAuthor) {
    return
      updateCarets(
        composite,
        selectCarets(composite, caretIds)
          .map(caret -> new Caret(caret.id, caret.offset, caret.offset, caret.offset, caret.vCol)))
        .log(Op.DROP_SELECTIONS, caretIds, Edit.empty(), editsAuthor);
  }

  public static Composite moveCarets(Composite composite, Map<Object, CaretMovement> movements, Object editsAuthor) {
    Text text = composite.text;
    long codePointsCount = text.codePointsCount();
    return updateCarets(
      composite,
      selectCarets(composite, movements.keySet())
        .map(caret -> {
          CaretMovement mv = movements.get(caret.id);
          long offset = clamp(caret.offset + mv.offsetDelta, codePointsCount);
          return new Caret(caret.id,
                           offset,
                           clamp(caret.selectionStart + mv.selectionStartDelta, codePointsCount),
                           clamp(caret.selectionEnd + mv.selectionEndDelta, codePointsCount),
                           mv.keepVCol ? caret.vCol : text.offsetToGeomCol(offset));
        }))
      .log(Op.MOVE_CARETS, movements, Edit.empty(), editsAuthor);
  }

  public static long clamp(long offset, long length) {
    return Math.min(Math.max(0, offset), length);
  }
}
