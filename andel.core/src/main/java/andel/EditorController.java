package andel;

import andel.carets.Caret;
import andel.carets.CaretMovement;
import andel.carets.MultiCaret;

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public class EditorController {

  public static Editor edit(Editor editor, Edit edit) {
    return editor.withComposite(editor.composite.edit(edit).log(Op.EDIT, edit, edit, editor.editsAuthor));
  }

  public static Editor insertBeforeCarets(Editor editor, Map<Object, String> insertions) {
    return editor.withComposite(CompositeController.insertBeforeCarets(editor.composite, insertions, editor.editsAuthor));
  }

  public static Editor insertAfterCarets(Editor editor, Map<Object, String> insertions) {
    return editor.withComposite(CompositeController.insertAfterCarets(editor.composite, insertions, editor.editsAuthor));
  }

  @Deprecated
  public static Editor moveCarets(Editor editor, Map<Object, CaretMovement> movements) {
    return editor.withComposite(CompositeController.moveCarets(editor.composite, movements, editor.editsAuthor));
  }

  public static Editor dropSelections(Editor editor, Iterable<Object> caretIds) {
    Set<Object> caretIdsSet = StreamSupport.stream(caretIds.spliterator(), false).collect(Collectors.toSet());
    return editor.withComposite(CompositeController.dropSelections(editor.composite, caretIdsSet, editor.editsAuthor));
  }

  public static Editor addCaret(Editor editor, Caret caret) {
    MultiCaret carets = editor.getMultiCaret();
    return editor
      .putMultiCaret(carets.merge(Collections.singletonList(caret)))
      .log(Op.ADD_CARET, caret, Edit.empty());
  }

  public static Editor deleteSelectedText(Editor editor, Set<Object> caretIds) {
    return editor.withComposite(CompositeController.deleteSelectedText(editor.composite, caretIds, editor.editsAuthor));
  }

  public static Editor deleteBeforeCarets(Editor editor, Map<Object, Long> caretIds) {
    return editor.withComposite(CompositeController.deleteBeforeCarets(caretIds, editor.composite, editor.editsAuthor));
  }

  public static Editor deleteAfterCarets(Editor editor, Map<Object, Long> caretIds) {
    return editor.withComposite(CompositeController.deleteAfterCarets(editor.composite, caretIds, editor.editsAuthor));
  }
}
