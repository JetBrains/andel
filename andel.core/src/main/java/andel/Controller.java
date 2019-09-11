package andel;

import andel.carets.Caret;
import andel.carets.CaretMovement;
import andel.carets.Carets;
import andel.text.TextZipper;

import java.util.*;

public class Controller {

  private static ArrayList<Object> createCaretsInsertionOperation(Composite composite, Editor editor, Map<Object, String> insertions) {
    Carets carets = editor.getCarets(composite);
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

  public static Composite insertBeforeCarets(Composite composite, Editor editor, Map<Object, String> insertions) {
    ArrayList<Object> ops = createCaretsInsertionOperation(composite, editor, insertions);
    return composite
      .edit(new Edit(ops.toArray(), true))
      .log(new LogEntry(Op.INSERT_BEFORE_CARETS, insertions));
  }

  public static Composite insertAfterCarets(Composite composite, Editor editor, Map<Object, String> insertions) {
    ArrayList<Object> ops = createCaretsInsertionOperation(composite, editor, insertions);
    return composite
      .edit(new Edit(ops.toArray(), false))
      .log(new LogEntry(Op.INSERT_AFTER_CARETS, insertions));
  }

  public static Composite moveCarets(Composite composite, Editor editor, Map<Object, CaretMovement> movements) {
    Carets carets = editor.getCarets(composite);
    List<Caret> caretsUpdate = new ArrayList<>();
    for (Map.Entry<Object, CaretMovement> entry : movements.entrySet()) {
      Caret caret = carets.getCaret(entry.getKey());
      CaretMovement mv = entry.getValue();
      caretsUpdate.add(new Caret(caret.id,
                                 caret.offset + mv.offsetDelta,
                                 caret.selectionStart + mv.selectionStartDelta,
                                 caret.selectionEnd + mv.selectionEndDelta,
                                 -1));
    }
    caretsUpdate.sort(Carets.COMPARE_BY_OFFSET);
    return editor
      .putCarets(composite, carets.merge(caretsUpdate))
      .log(new LogEntry(Op.MOVE_CARETS, movements));
  }

  public static Composite dropSelections(Composite composite, Editor editor, Iterable<Object> caretIds) {
    Carets carets = editor.getCarets(composite);
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
      .putCarets(composite, carets.merge(caretsUpdate))
      .log(new LogEntry(Op.DROP_SELECTIONS, caretIds));
  }

  public static Composite addCaret(Composite composite, Editor editor, Caret caret) {
    Carets carets = editor.getCarets(composite);
    return editor
      .putCarets(composite, carets.merge(Collections.singletonList(caret)))
      .log(new LogEntry(Op.ADD_CARET, caret));
  }

  public static Composite deleteSelections(Composite composite, Editor editor, Set<Object> caretIds) {
    Carets carets = editor.getCarets(composite);
    long prevCaretSelectionEnd = 0;
    List<Object> ops = new ArrayList<>();
    TextZipper zipper = composite.text.zipper().asTransient();
    for (Caret caret : carets.getCarets()) {
      if (caretIds.contains(caret.id) && caret.selectionStart != caret.selectionEnd) {
        ops.add(new Edit.Retain(caret.selectionStart - prevCaretSelectionEnd));
        StringBuilder sb = new StringBuilder();
        zipper = zipper.scanToCodepoint(caret.selectionStart).consume(caret.selectionEnd - caret.selectionStart, sb::append);
        ops.add(new Edit.Delete(sb.toString()));
        prevCaretSelectionEnd = caret.selectionEnd;
      }
    }
    return composite
      .edit(new Edit(ops.toArray(), false))
      .log(new LogEntry(Op.DELETE_SELECTIONS, caretIds));
  }

  public static Composite deleteBeforeCarets(Composite composite, Editor editor, Map<Object, Long> caretIds) {
    List<Object> ops = new ArrayList<>();
    long lastOffset = 0;
    TextZipper zipper = composite.text.zipper().asTransient();
    for (Caret caret : editor.getCarets(composite).getCarets()) {
      Long count = caretIds.get(caret.id);
      if (count != null && count != 0) {
        long from = caret.offset - count;
        long to = caret.offset;
        StringBuilder sb = new StringBuilder();
        zipper = zipper.scanToCodepoint(from).consume(count, sb::append);
        ops.add(new Edit.Retain(from - lastOffset));
        ops.add(new Edit.Delete(sb.toString()));
        lastOffset = to;
      }
    }
    return composite
      .edit(new Edit(ops.toArray(), false))
      .log(new LogEntry(Op.DELETE_BEFORE_CARETS, caretIds));
  }

  public static Composite deleteAfterCarets(Composite composite, Editor editor, Map<Object, Long> caretIds) {
    List<Object> ops = new ArrayList<>();
    long lastOffset = 0;
    TextZipper zipper = composite.text.zipper().asTransient();
    for (Caret caret : editor.getCarets(composite).getCarets()) {
      Long count = caretIds.get(caret.id);
      if (count != null && count != 0) {
        long from = caret.offset;
        long to = caret.offset + count;
        StringBuilder sb = new StringBuilder();
        zipper = zipper.scanToCodepoint(from).consume(count, sb::append);
        ops.add(new Edit.Retain(from - lastOffset));
        ops.add(new Edit.Delete(sb.toString()));
        lastOffset = to;
      }
    }
    return composite
      .edit(new Edit(ops.toArray(), false))
      .log(new LogEntry(Op.DELETE_AFTER_CARETS, caretIds));
  }
}
