package io.pgenie.artifacts.mySpace.musicCatalogue.javaJdbc;


public final class SelectGenreByArtistResultRow implements Comparable<SelectGenreByArtistResultRow> {
  public final int id;
  public final String name;

  SelectGenreByArtistResultRow(int id, String name) {
    this.id = id;
    this.name = name;
  }

  public boolean equals(Object that) {
    return that instanceof SelectGenreByArtistResultRow && equals((SelectGenreByArtistResultRow) that);
  }
  /**
   * Equality check specialized to only the instances of this class.
   * <p>
   * Unlike the Object-generalized version it avoids instance checks,
   * since that is resolved by the type system.
   */
  public boolean equals(SelectGenreByArtistResultRow that) {
    return
      id == that.id &&
      name.equals(that.name);
  }

  public int hashCode() {
    int hash = SelectGenreByArtistResultRow.class.hashCode();
    hash = (hash << 5) - hash + this.id;
    hash = (hash << 5) - hash + this.name.hashCode();
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
    builder.append('}');
    return builder.toString();
  }

  public int compareTo(SelectGenreByArtistResultRow that) {
    int status;
    status = Integer.compare(this.id, that.id);
    if (status != 0) return status;
    status = this.name.compareTo(that.name);
    if (status != 0) return status;
    return 0;
  }
}