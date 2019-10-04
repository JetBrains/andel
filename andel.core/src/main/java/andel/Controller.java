package andel;

import andel.carets.Caret;
import andel.carets.CaretMovement;
import andel.carets.Carets;
import andel.text.TextZipper;

import java.util.*;

public class Controller {

  private static ArrayList<Object> createCaretsInsertionOperation(Editor editor, Map<Object, String> insertions) {
    Carets carets = editor.getCarets();
    long prevCaretOffset = 0;
    ArrayList<Object> ops = new ArrayList<>();
    for (Caret caret : carets.getCarets()) {
      String text = insertions.get(caret.id);
      if (text != null) {
        ops.add(new Edit.Retain(caret.offset - prevCaretOffset));
        ops.add(new Edit.Insert(text));
        prevCaretOffset = caret.offset;
      }
    }
    return ops;
  }

  public static Composite edit(Composite composite, Edit edit) {
    return composite.edit(edit).log(new LogEntry(Op.EDIT, edit));
  }

  public static Editor edit(Editor editor, Edit edit) {
    return editor.withComposite(editor.composite
                                .edit(edit)
                                .log(new LogEntry(Op.EDIT, edit)));
  }

  public static Editor insertBeforeCarets(Editor editor, Map<Object, String> insertions) {
    ArrayList<Object> ops = createCaretsInsertionOperation(editor, insertions);
    return editor.withComposite(editor.composite
                                  .edit(new Edit(ops.toArray(), true))
                                  .log(new LogEntry(Op.INSERT_BEFORE_CARETS, insertions)));
  }

  public static Editor insertAfterCarets(Editor editor, Map<Object, String> insertions) {
    ArrayList<Object> ops = createCaretsInsertionOperation(editor, insertions);
    return editor.withComposite(editor.composite
                                  .edit(new Edit(ops.toArray(), false))
                                  .log(new LogEntry(Op.INSERT_AFTER_CARETS, insertions)));
  }

  private static long restrictToLength(long offset, long length) {
    return Math.min(Math.max(0, offset), length);
  }

  public static Editor moveCarets(Editor editor, Map<Object, CaretMovement> movements) {
    Carets carets = editor.getCarets();
    List<Caret> caretsUpdate = new ArrayList<>();
    long codePointsCount = editor.composite.text.codePointsCount();
    for (Map.Entry<Object, CaretMovement> entry : movements.entrySet()) {
      Caret caret = carets.getCaret(entry.getKey());
      CaretMovement mv = entry.getValue();
      caretsUpdate.add(new Caret(caret.id,
                                 restrictToLength(caret.offset + mv.offsetDelta, codePointsCount),
                                 restrictToLength(caret.selectionStart + mv.selectionStartDelta, codePointsCount),
                                 restrictToLength(caret.selectionEnd + mv.selectionEndDelta, codePointsCount),
                                 -1));
    }
    caretsUpdate.sort(Carets.COMPARE_BY_OFFSET);
    return editor
      .putCarets(carets.merge(caretsUpdate))
      .log(new LogEntry(Op.MOVE_CARETS, movements));
  }

  public static Editor dropSelections(Editor editor, Iterable<Object> caretIds) {
    Carets carets = editor.getCarets();
    List<Caret> caretsUpdate = new ArrayList<>();
    for (Object caretId : caretIds) {
      Caret caret = carets.getCaret(caretId);
      caretsUpdate.add(new Caret(caret.id,
                                 caret.offset,
                                 caret.offset,
                                 caret.offset,
                                 -1));
    }
    caretsUpdate.sort(Carets.COMPARE_BY_OFFSET);
    return editor
      .putCarets(carets.merge(caretsUpdate))
      .log(new LogEntry(Op.DROP_SELECTIONS, caretIds));
  }

  public static Editor addCaret(Editor editor, Caret caret) {
    Carets carets = editor.getCarets();
    return editor
      .putCarets(carets.merge(Collections.singletonList(caret)))
      .log(new LogEntry(Op.ADD_CARET, caret));
  }

  public static Editor deleteSelectedText(Editor editor, Set<Object> caretIds) {
    Carets carets = editor.getCarets();
    long prevCaretSelectionEnd = 0;
    List<Object> ops = new ArrayList<>();
    TextZipper zipper = editor.composite.text.zipper().asTransient();
    for (Caret caret : carets.getCarets()) {
      if (caretIds.contains(caret.id) && caret.selectionStart != caret.selectionEnd) {
        ops.add(new Edit.Retain(caret.selectionStart - prevCaretSelectionEnd));
        StringBuilder sb = new StringBuilder();
        zipper = zipper.scanToCodepoint(caret.selectionStart).consume(caret.selectionEnd - caret.selectionStart, sb::append);
        ops.add(new Edit.Delete(sb.toString()));
        prevCaretSelectionEnd = caret.selectionEnd;
      }
    }
    return editor
      .edit(new Edit(ops.toArray(), false))
      .log(new LogEntry(Op.DELETE_SELECTIONS, caretIds));
  }

  public static Editor deleteBeforeCarets(Editor editor, Map<Object, Long> caretIds) {
    List<Object> ops = new ArrayList<>();
    long lastOffset = 0;
    TextZipper zipper = editor.composite.text.zipper().asTransient();
    for (Caret caret : editor.getCarets().getCarets()) {
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
    return editor
      .edit(new Edit(ops.toArray(), false))
      .log(new LogEntry(Op.DELETE_BEFORE_CARETS, caretIds));
  }

  public static Editor deleteAfterCarets(Editor editor, Map<Object, Long> caretIds) {
    List<Object> ops = new ArrayList<>();
    long lastOffset = 0;
    TextZipper zipper = editor.composite.text.zipper().asTransient();
    long codePointsCount = editor.composite.text.codePointsCount();
    for (Caret caret : editor.getCarets().getCarets()) {
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
    return editor
      .edit(new Edit(ops.toArray(), false))
      .log(new LogEntry(Op.DELETE_AFTER_CARETS, caretIds));
  }
}
