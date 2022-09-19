module PgenieArtifacts.MySpace.MusicCatalogue.Hasql.Statements where

import BasePrelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Foldl (Fold (..))
import Hasql.Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified PgenieArtifacts.MySpace.MusicCatalogue.Hasql.Model as Model

-- |
-- Integration with the following statement:
--
-- > select id, genre.name
-- > from genre
-- > left join album_genre on album_genre.genre = genre.id
-- > left join album_artist on album_artist.album = album_genre.album
-- > where album_artist.artist = $artist
--
selectGenreByArtistFolding ::
  -- | How to fold the result rows.
  Fold Model.SelectGenreByArtistResultRow rows ->
  Statement Model.SelectGenreByArtistParams rows
selectGenreByArtistFolding (Fold step init extract) =
  Statement sql encoder decoder True
  where
    sql =
      "select id, genre.name\n\
      \from genre\n\
      \left join album_genre on album_genre.genre = genre.id\n\
      \left join album_artist on album_artist.album = album_genre.album\n\
      \where album_artist.artist = $1"
    encoder =
      Model.selectGenreByArtistParamsArtist >$< Encoders.param (Encoders.nullable Encoders.int4)
    decoder =
      fmap extract . Decoders.foldlRows step init $
        Model.SelectGenreByArtistResultRow
          <$> Decoders.column (Decoders.nonNullable Decoders.int4)
          <*> Decoders.column (Decoders.nonNullable Decoders.text)

-- |
-- Integration with the following statement:
--
-- > select album.*
-- > from album
-- > left join album_artist on album_artist.album = album.id
-- > where artist = $artist
--
selectAlbumByArtistFolding ::
  -- | How to fold the result rows.
  Fold Model.SelectAlbumByArtistResultRow rows ->
  Statement Model.SelectAlbumByArtistParams rows
selectAlbumByArtistFolding (Fold step init extract) =
  Statement sql encoder decoder True
  where
    sql =
      "select album.*\n\
      \from album\n\
      \left join album_artist on album_artist.album = album.id\n\
      \where artist = $1"
    encoder =
      Model.selectAlbumByArtistParamsArtist >$< Encoders.param (Encoders.nullable Encoders.int4)
    decoder =
      fmap extract . Decoders.foldlRows step init $
        Model.SelectAlbumByArtistResultRow
          <$> Decoders.column (Decoders.nonNullable Decoders.int8)
          <*> Decoders.column (Decoders.nonNullable Decoders.text)
          <*> Decoders.column (Decoders.nullable Decoders.date)

-- |
-- Integration with the following statement:
--
-- > insert into album (name, released)
-- > values ($name, $released)
-- > returning id
--
insertAlbumFolding ::
  -- | How to fold the result rows.
  Fold Model.InsertAlbumResultRow rows ->
  Statement Model.InsertAlbumParams rows
insertAlbumFolding (Fold step init extract) =
  Statement sql encoder decoder True
  where
    sql =
      "insert into album (name, released)\n\
      \values ($1, $2)\n\
      \returning id"
    encoder =
      mconcat 
        [ Model.insertAlbumParamsName >$< Encoders.param (Encoders.nonNullable Encoders.text),
          Model.insertAlbumParamsReleased >$< Encoders.param (Encoders.nullable Encoders.date)
        ]
    decoder =
      fmap extract . Decoders.foldlRows step init $
        Model.InsertAlbumResultRow
          <$> Decoders.column (Decoders.nonNullable Decoders.int8)
