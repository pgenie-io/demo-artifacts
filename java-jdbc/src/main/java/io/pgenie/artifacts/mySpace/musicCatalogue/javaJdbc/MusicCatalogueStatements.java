package io.pgenie.artifacts.mySpace.musicCatalogue.javaJdbc;

import java.io.Closeable;
import java.math.BigDecimal;
import java.sql.*;
import java.sql.Date;
import java.util.*;
import java.util.function.*;
import java.util.stream.Collector;
import org.postgresql.geometric.PGpoint;
import org.postgresql.util.PGInterval;

public final class MusicCatalogueStatements implements Closeable {
  private final PreparedStatement insertAlbumStatement;
  private final PreparedStatement selectAlbumByArtistStatement;
  private final PreparedStatement selectGenreByArtistStatement;

  public MusicCatalogueStatements(Connection conn) throws SQLException {
    insertAlbumStatement =
      conn.prepareStatement(
        "insert into album (name, released)\nvalues (?, ?)\nreturning id\n"
      );
    selectAlbumByArtistStatement =
      conn.prepareStatement(
        "select album.*\nfrom album\nleft join album_artist on album_artist.album = album.id\nwhere artist = ?\n"
      );
    selectGenreByArtistStatement =
      conn.prepareStatement(
        "select id, genre.name\nfrom genre\nleft join album_genre on album_genre.genre = genre.id\nleft join album_artist on album_artist.album = album_genre.album\nwhere album_artist.artist = ?\n"
      );
  }

  /**
   * Close all the resources acquired internally by this class.
   * <p>
   * It does not close the JDBC connection,
   * since it is initialized outside and as such is presumed to be managed outside as well.
   */
  public void close() {
    try {insertAlbumStatement.close();} catch (SQLException ignored) {}
    try {selectAlbumByArtistStatement.close();} catch (SQLException ignored) {}
    try {selectGenreByArtistStatement.close();} catch (SQLException ignored) {}
  }

  /**
   * Run the following statement, processing rows using the provided collector.
   *
   * <pre>{@code
   * insert into album (name, released)
   * values ($name, $released)
   * returning id
   * }</pre>
   *
   * @param nameParam Value for the "$name" placeholder in the query
   * @param releasedParam Value for the "$released" placeholder in the query
   * @param collector Specification of how to process result rows
   * @return Result of the collector
   */
  public <acc, res> res insertAlbum(Optional<String> nameParam, Optional<Date> releasedParam, Collector<InsertAlbumResultRow, acc, res> collector) throws SQLException {
    if (nameParam.isPresent()) {
      insertAlbumStatement.setString(1, nameParam.get());
    } else {
      insertAlbumStatement.setNull(1, Types.VARCHAR);
    }
    if (releasedParam.isPresent()) {
      insertAlbumStatement.setDate(2, releasedParam.get());
    } else {
      insertAlbumStatement.setNull(2, Types.DATE);
    }
    insertAlbumStatement.execute();
  
    acc container = collector.supplier().get();
    BiConsumer<acc, InsertAlbumResultRow> accumulator = collector.accumulator();
  
    try (ResultSet resultSet = insertAlbumStatement.getResultSet()) {
      while (resultSet.next()) {
        long idCol = resultSet.getLong(1);
        accumulator.accept(container, new InsertAlbumResultRow(idCol));
      }
    }
  
    return collector.finisher().apply(container);
  }
  
  /**
   * Run the following statement, calling the provided consumer on each result row.
   *
   * <pre>{@code
   * insert into album (name, released)
   * values ($name, $released)
   * returning id
   * }</pre>
   *
   * @param nameParam Value for the "$name" placeholder in the query
   * @param releasedParam Value for the "$released" placeholder in the query
   * @param consumer Consumer to be called on each row
   */
  public void insertAlbum(Optional<String> nameParam, Optional<Date> releasedParam, Consumer<InsertAlbumResultRow> consumer) throws SQLException {
    if (nameParam.isPresent()) {
      insertAlbumStatement.setString(1, nameParam.get());
    } else {
      insertAlbumStatement.setNull(1, Types.VARCHAR);
    }
    if (releasedParam.isPresent()) {
      insertAlbumStatement.setDate(2, releasedParam.get());
    } else {
      insertAlbumStatement.setNull(2, Types.DATE);
    }
    insertAlbumStatement.execute();
  
    try (ResultSet resultSet = insertAlbumStatement.getResultSet()) {
      while (resultSet.next()) {
        long idCol = resultSet.getLong(1);
        consumer.accept(new InsertAlbumResultRow(idCol));
      }
    }
  }
  
  /**
   * Run the following statement, packing the result rows in ArrayList.
   *
   * <pre>{@code
   * insert into album (name, released)
   * values ($name, $released)
   * returning id
   * }</pre>
   *
   * @param nameParam Value for the "$name" placeholder in the query
   * @param releasedParam Value for the "$released" placeholder in the query
   * @return ArrayList of result row objects specialized to this statement
   */
  public ArrayList<InsertAlbumResultRow> insertAlbum(Optional<String> nameParam, Optional<Date> releasedParam) throws SQLException {
    if (nameParam.isPresent()) {
      insertAlbumStatement.setString(1, nameParam.get());
    } else {
      insertAlbumStatement.setNull(1, Types.VARCHAR);
    }
    if (releasedParam.isPresent()) {
      insertAlbumStatement.setDate(2, releasedParam.get());
    } else {
      insertAlbumStatement.setNull(2, Types.DATE);
    }
    insertAlbumStatement.execute();
  
    ArrayList<InsertAlbumResultRow> list = new ArrayList<>();
  
    try (ResultSet resultSet = insertAlbumStatement.getResultSet()) {
      while (resultSet.next()) {
        long idCol = resultSet.getLong(1);
        list.add(new InsertAlbumResultRow(idCol));
      }
    }
  
    return list;
  }
  
  /**
   * Run the following statement, decoding only the first result row if there are any.
   *
   * <pre>{@code
   * insert into album (name, released)
   * values ($name, $released)
   * returning id
   * }</pre>
   *
   * @param nameParam Value for the "$name" placeholder in the query
   * @param releasedParam Value for the "$released" placeholder in the query
   * @return The first result row if there are any
   */
  public Optional<InsertAlbumResultRow> insertAlbumFirst(Optional<String> nameParam, Optional<Date> releasedParam) throws SQLException {
    if (nameParam.isPresent()) {
      insertAlbumStatement.setString(1, nameParam.get());
    } else {
      insertAlbumStatement.setNull(1, Types.VARCHAR);
    }
    if (releasedParam.isPresent()) {
      insertAlbumStatement.setDate(2, releasedParam.get());
    } else {
      insertAlbumStatement.setNull(2, Types.DATE);
    }
    insertAlbumStatement.execute();
  
    try (ResultSet resultSet = insertAlbumStatement.getResultSet()) {
      if (resultSet.next()) {
        long idCol = resultSet.getLong(1);
        return Optional.of(new InsertAlbumResultRow(idCol));
      } else {
        return Optional.empty();
      }
    }
  }
  
  /**
   * Run the following statement, processing rows using the provided collector.
   *
   * <pre>{@code
   * select album.*
   * from album
   * left join album_artist on album_artist.album = album.id
   * where artist = $artist
   * }</pre>
   *
   * @param artistParam Value for the "$artist" placeholder in the query
   * @param collector Specification of how to process result rows
   * @return Result of the collector
   */
  public <acc, res> res selectAlbumByArtist(OptionalInt artistParam, Collector<SelectAlbumByArtistResultRow, acc, res> collector) throws SQLException {
    if (artistParam.isPresent()) {
      selectAlbumByArtistStatement.setInt(1, artistParam.getAsInt());
    } else {
      selectAlbumByArtistStatement.setNull(1, Types.INTEGER);
    }
    selectAlbumByArtistStatement.execute();
  
    acc container = collector.supplier().get();
    BiConsumer<acc, SelectAlbumByArtistResultRow> accumulator = collector.accumulator();
  
    try (ResultSet resultSet = selectAlbumByArtistStatement.getResultSet()) {
      while (resultSet.next()) {
        long idCol = resultSet.getLong(1);
        String nameCol = resultSet.getString(2);
        Optional<Date> releasedCol = Optional.ofNullable(resultSet.getDate(3));
        accumulator.accept(container, new SelectAlbumByArtistResultRow(idCol, nameCol, releasedCol));
      }
    }
  
    return collector.finisher().apply(container);
  }
  
  /**
   * Run the following statement, calling the provided consumer on each result row.
   *
   * <pre>{@code
   * select album.*
   * from album
   * left join album_artist on album_artist.album = album.id
   * where artist = $artist
   * }</pre>
   *
   * @param artistParam Value for the "$artist" placeholder in the query
   * @param consumer Consumer to be called on each row
   */
  public void selectAlbumByArtist(OptionalInt artistParam, Consumer<SelectAlbumByArtistResultRow> consumer) throws SQLException {
    if (artistParam.isPresent()) {
      selectAlbumByArtistStatement.setInt(1, artistParam.getAsInt());
    } else {
      selectAlbumByArtistStatement.setNull(1, Types.INTEGER);
    }
    selectAlbumByArtistStatement.execute();
  
    try (ResultSet resultSet = selectAlbumByArtistStatement.getResultSet()) {
      while (resultSet.next()) {
        long idCol = resultSet.getLong(1);
        String nameCol = resultSet.getString(2);
        Optional<Date> releasedCol = Optional.ofNullable(resultSet.getDate(3));
        consumer.accept(new SelectAlbumByArtistResultRow(idCol, nameCol, releasedCol));
      }
    }
  }
  
  /**
   * Run the following statement, packing the result rows in ArrayList.
   *
   * <pre>{@code
   * select album.*
   * from album
   * left join album_artist on album_artist.album = album.id
   * where artist = $artist
   * }</pre>
   *
   * @param artistParam Value for the "$artist" placeholder in the query
   * @return ArrayList of result row objects specialized to this statement
   */
  public ArrayList<SelectAlbumByArtistResultRow> selectAlbumByArtist(OptionalInt artistParam) throws SQLException {
    if (artistParam.isPresent()) {
      selectAlbumByArtistStatement.setInt(1, artistParam.getAsInt());
    } else {
      selectAlbumByArtistStatement.setNull(1, Types.INTEGER);
    }
    selectAlbumByArtistStatement.execute();
  
    ArrayList<SelectAlbumByArtistResultRow> list = new ArrayList<>();
  
    try (ResultSet resultSet = selectAlbumByArtistStatement.getResultSet()) {
      while (resultSet.next()) {
        long idCol = resultSet.getLong(1);
        String nameCol = resultSet.getString(2);
        Optional<Date> releasedCol = Optional.ofNullable(resultSet.getDate(3));
        list.add(new SelectAlbumByArtistResultRow(idCol, nameCol, releasedCol));
      }
    }
  
    return list;
  }
  
  /**
   * Run the following statement, decoding only the first result row if there are any.
   *
   * <pre>{@code
   * select album.*
   * from album
   * left join album_artist on album_artist.album = album.id
   * where artist = $artist
   * }</pre>
   *
   * @param artistParam Value for the "$artist" placeholder in the query
   * @return The first result row if there are any
   */
  public Optional<SelectAlbumByArtistResultRow> selectAlbumByArtistFirst(OptionalInt artistParam) throws SQLException {
    if (artistParam.isPresent()) {
      selectAlbumByArtistStatement.setInt(1, artistParam.getAsInt());
    } else {
      selectAlbumByArtistStatement.setNull(1, Types.INTEGER);
    }
    selectAlbumByArtistStatement.execute();
  
    try (ResultSet resultSet = selectAlbumByArtistStatement.getResultSet()) {
      if (resultSet.next()) {
        long idCol = resultSet.getLong(1);
        String nameCol = resultSet.getString(2);
        Optional<Date> releasedCol = Optional.ofNullable(resultSet.getDate(3));
        return Optional.of(new SelectAlbumByArtistResultRow(idCol, nameCol, releasedCol));
      } else {
        return Optional.empty();
      }
    }
  }
  
  /**
   * Run the following statement, processing rows using the provided collector.
   *
   * <pre>{@code
   * select id, genre.name
   * from genre
   * left join album_genre on album_genre.genre = genre.id
   * left join album_artist on album_artist.album = album_genre.album
   * where album_artist.artist = $artist
   * }</pre>
   *
   * @param artistParam Value for the "$artist" placeholder in the query
   * @param collector Specification of how to process result rows
   * @return Result of the collector
   */
  public <acc, res> res selectGenreByArtist(OptionalInt artistParam, Collector<SelectGenreByArtistResultRow, acc, res> collector) throws SQLException {
    if (artistParam.isPresent()) {
      selectGenreByArtistStatement.setInt(1, artistParam.getAsInt());
    } else {
      selectGenreByArtistStatement.setNull(1, Types.INTEGER);
    }
    selectGenreByArtistStatement.execute();
  
    acc container = collector.supplier().get();
    BiConsumer<acc, SelectGenreByArtistResultRow> accumulator = collector.accumulator();
  
    try (ResultSet resultSet = selectGenreByArtistStatement.getResultSet()) {
      while (resultSet.next()) {
        int idCol = resultSet.getInt(1);
        String nameCol = resultSet.getString(2);
        accumulator.accept(container, new SelectGenreByArtistResultRow(idCol, nameCol));
      }
    }
  
    return collector.finisher().apply(container);
  }
  
  /**
   * Run the following statement, calling the provided consumer on each result row.
   *
   * <pre>{@code
   * select id, genre.name
   * from genre
   * left join album_genre on album_genre.genre = genre.id
   * left join album_artist on album_artist.album = album_genre.album
   * where album_artist.artist = $artist
   * }</pre>
   *
   * @param artistParam Value for the "$artist" placeholder in the query
   * @param consumer Consumer to be called on each row
   */
  public void selectGenreByArtist(OptionalInt artistParam, Consumer<SelectGenreByArtistResultRow> consumer) throws SQLException {
    if (artistParam.isPresent()) {
      selectGenreByArtistStatement.setInt(1, artistParam.getAsInt());
    } else {
      selectGenreByArtistStatement.setNull(1, Types.INTEGER);
    }
    selectGenreByArtistStatement.execute();
  
    try (ResultSet resultSet = selectGenreByArtistStatement.getResultSet()) {
      while (resultSet.next()) {
        int idCol = resultSet.getInt(1);
        String nameCol = resultSet.getString(2);
        consumer.accept(new SelectGenreByArtistResultRow(idCol, nameCol));
      }
    }
  }
  
  /**
   * Run the following statement, packing the result rows in ArrayList.
   *
   * <pre>{@code
   * select id, genre.name
   * from genre
   * left join album_genre on album_genre.genre = genre.id
   * left join album_artist on album_artist.album = album_genre.album
   * where album_artist.artist = $artist
   * }</pre>
   *
   * @param artistParam Value for the "$artist" placeholder in the query
   * @return ArrayList of result row objects specialized to this statement
   */
  public ArrayList<SelectGenreByArtistResultRow> selectGenreByArtist(OptionalInt artistParam) throws SQLException {
    if (artistParam.isPresent()) {
      selectGenreByArtistStatement.setInt(1, artistParam.getAsInt());
    } else {
      selectGenreByArtistStatement.setNull(1, Types.INTEGER);
    }
    selectGenreByArtistStatement.execute();
  
    ArrayList<SelectGenreByArtistResultRow> list = new ArrayList<>();
  
    try (ResultSet resultSet = selectGenreByArtistStatement.getResultSet()) {
      while (resultSet.next()) {
        int idCol = resultSet.getInt(1);
        String nameCol = resultSet.getString(2);
        list.add(new SelectGenreByArtistResultRow(idCol, nameCol));
      }
    }
  
    return list;
  }
  
  /**
   * Run the following statement, decoding only the first result row if there are any.
   *
   * <pre>{@code
   * select id, genre.name
   * from genre
   * left join album_genre on album_genre.genre = genre.id
   * left join album_artist on album_artist.album = album_genre.album
   * where album_artist.artist = $artist
   * }</pre>
   *
   * @param artistParam Value for the "$artist" placeholder in the query
   * @return The first result row if there are any
   */
  public Optional<SelectGenreByArtistResultRow> selectGenreByArtistFirst(OptionalInt artistParam) throws SQLException {
    if (artistParam.isPresent()) {
      selectGenreByArtistStatement.setInt(1, artistParam.getAsInt());
    } else {
      selectGenreByArtistStatement.setNull(1, Types.INTEGER);
    }
    selectGenreByArtistStatement.execute();
  
    try (ResultSet resultSet = selectGenreByArtistStatement.getResultSet()) {
      if (resultSet.next()) {
        int idCol = resultSet.getInt(1);
        String nameCol = resultSet.getString(2);
        return Optional.of(new SelectGenreByArtistResultRow(idCol, nameCol));
      } else {
        return Optional.empty();
      }
    }
  }
}
