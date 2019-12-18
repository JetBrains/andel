package andel;

import andel.text.Text;
import io.lacuna.bifurcan.List;

import java.util.UUID;

public class EditLog implements Component {

  public static class AuthoredEdit {
    public final UUID author;
    public final Edit edit;

    public AuthoredEdit(UUID author, Edit edit) {
      this.author = author;
      this.edit = edit;
    }

    @Override
    public String toString() {
      return "[" + author + ", " + edit + "]";
    }

  }

  public final List<AuthoredEdit> log;
  public final UUID author;

  public EditLog(List<AuthoredEdit> log, UUID author) {this.log = log;
    this.author = author;
  }

  @Override
  public Component edit(Text before, Text after, Edit edit) {
    return new EditLog(log.addLast(new AuthoredEdit(this.author, edit)), this.author);
  }


}
