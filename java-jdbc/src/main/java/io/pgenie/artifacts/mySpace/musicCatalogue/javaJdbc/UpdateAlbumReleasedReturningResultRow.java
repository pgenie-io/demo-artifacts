package io.pgenie.artifacts.mySpace.musicCatalogue.javaJdbc;

import java.sql.Date;
import java.util.Optional;

public final class UpdateAlbumReleasedReturningResultRow implements Comparable<UpdateAlbumReleasedReturningResultRow> {
  public final long id;
  public final String name;
  public final Optional<Date> released;

  UpdateAlbumReleasedReturningResultRow(long id, String name, Optional<Date> released) {
    this.id = id;
    this.name = name;
    this.released = released;
  }

  public boolean equals(Object that) {
    return that instanceof UpdateAlbumReleasedReturningResultRow && equals((UpdateAlbumReleasedReturningResultRow) that);
  }
  /**
   * Equality check specialized to only the instances of this class.
   * <p>
   * Unlike the Object-generalized version it avoids instance checks,
   * since that is resolved by the type system.
   */
  public boolean equals(UpdateAlbumReleasedReturningResultRow that) {
    return
      id == that.id &&
      name.equals(that.name) &&
      released.equals(that.released);
  }

  public int hashCode() {
    int hash = UpdateAlbumReleasedReturningResultRow.class.hashCode();
    hash = (hash << 5) - hash + (int) (this.id ^ (this.id >>> 32));
    hash = (hash << 5) - hash + this.name.hashCode();
    hash = (hash << 5) - hash + this.released.hashCode();
    return hash;
  }

  /**
   * Serialize to compact JSON representation.
   */
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append('{');
    builder.append("\"id\":");
    builder.append(this.id);
    builder.append(',');
    builder.append("\"name\":");
    builder.append('"');
    builder.append(this.name.replace("\\", "\\\\").replace("\"", "\\\"").replace("\b", "\\b").replace("\f", "\\f").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t"));
    builder.append('"');
    builder.append(',');
    builder.append("\"released\":");
    if (this.released.isPresent()) {
      Date releasedPresent = this.released.get();
      builder.append('"');
      builder.append(releasedPresent.toString());
      builder.append('"');
    } else {
      builder.append("null");
    }
    builder.append('}');
    return builder.toString();
  }

  public int compareTo(UpdateAlbumReleasedReturningResultRow that) {
    int status;
    status = Long.compare(this.id, that.id);
    if (status != 0) return status;
    status = this.name.compareTo(that.name);
    if (status != 0) return status;
    if (this.released.isPresent()) {
      if (that.released.isPresent()) {
        status = this.released.get().compareTo(that.released.get());
        if (status != 0) return status;
      } else return 1;
    } else {
      if (that.released.isPresent()) return -1;
    }
    return 0;
  }
}