package io.pgenie.artifacts.mySpace.musicCatalogue.javaJdbc;


public final class InsertAlbumResultRow implements Comparable<InsertAlbumResultRow> {
  public final long id;

  InsertAlbumResultRow(long id) {
    this.id = id;
  }

  public boolean equals(Object that) {
    return that instanceof InsertAlbumResultRow && equals((InsertAlbumResultRow) that);
  }
  /**
   * Equality check specialized to only the instances of this class.
   * <p>
   * Unlike the Object-generalized version it avoids instance checks,
   * since that is resolved by the type system.
   */
  public boolean equals(InsertAlbumResultRow that) {
    return
      id == that.id;
  }

  public int hashCode() {
    int hash = InsertAlbumResultRow.class.hashCode();
    hash = (hash << 5) - hash + (int) (this.id ^ (this.id >>> 32));
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
    builder.append('}');
    return builder.toString();
  }

  public int compareTo(InsertAlbumResultRow that) {
    int status;
    status = Long.compare(this.id, that.id);
    if (status != 0) return status;
    return 0;
  }
}