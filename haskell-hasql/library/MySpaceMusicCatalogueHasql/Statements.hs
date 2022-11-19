module MySpaceMusicCatalogueHasql.Statements where

import BasePrelude
import Control.Foldl (Fold (..))
import Hasql.Statement
import qualified Data.Vector as BoxedVector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified MySpaceMusicCatalogueHasql.Model as Model

-- |
-- Integration with the following statement:
--
-- > insert into album (name, released)
-- > values ($name, $released)
-- > returning id
--
insertAlbum :: Statement Model.InsertAlbumParams Model.InsertAlbumResultRow
insertAlbum =
  Statement sql encoder decoder True
  where
    sql =
      "insert into album (name, released)\n\
      \values ($1, $2)\n\
      \returning id"
    encoder =
      mconcat 
        [ Model.insertAlbumParamsName >$< (Encoders.param (Encoders.nonNullable Encoders.text)),
          Model.insertAlbumParamsReleased >$< (Encoders.param (Encoders.nullable Encoders.date))
        ]
    decoder =
      Decoders.singleRow $
        Model.InsertAlbumResultRow <$> (Decoders.column (Decoders.nonNullable Decoders.int8))

-- |
-- Integration with the following statement:
--
-- > select album.*
-- > from album
-- > left join album_artist on album_artist.album = album.id
-- > where artist = $artist
--
selectAlbumByArtist ::
  -- | Specification of how to fold the result rows.
  Fold Model.SelectAlbumByArtistResultRow result ->
  Statement Model.SelectAlbumByArtistParams result
selectAlbumByArtist (Fold step init extract) =
  Statement sql encoder decoder True
  where
    sql =
      "select album.*\n\
      \from album\n\
      \left join album_artist on album_artist.album = album.id\n\
      \where artist = $1"
    encoder =
      Model.selectAlbumByArtistParamsArtist >$< (Encoders.param (Encoders.nullable Encoders.int4))
    decoder =
      fmap extract . Decoders.foldlRows step init $
        Model.SelectAlbumByArtistResultRow
          <$> (Decoders.column (Decoders.nonNullable Decoders.int8))
          <*> (Decoders.column (Decoders.nonNullable Decoders.text))
          <*> (Decoders.column (Decoders.nullable Decoders.date))

-- |
-- Integration with the following statement:
--
-- > select id, genre.name
-- > from genre
-- > left join album_genre on album_genre.genre = genre.id
-- > left join album_artist on album_artist.album = album_genre.album
-- > where album_artist.artist = $artist
--
selectGenreByArtist ::
  -- | Specification of how to fold the result rows.
  Fold Model.SelectGenreByArtistResultRow result ->
  Statement Model.SelectGenreByArtistParams result
selectGenreByArtist (Fold step init extract) =
  Statement sql encoder decoder True
  where
    sql =
      "select id, genre.name\n\
      \from genre\n\
      \left join album_genre on album_genre.genre = genre.id\n\
      \left join album_artist on album_artist.album = album_genre.album\n\
      \where album_artist.artist = $1"
    encoder =
      Model.selectGenreByArtistParamsArtist >$< (Encoders.param (Encoders.nullable Encoders.int4))
    decoder =
      fmap extract . Decoders.foldlRows step init $
        Model.SelectGenreByArtistResultRow
          <$> (Decoders.column (Decoders.nonNullable Decoders.int4))
          <*> (Decoders.column (Decoders.nonNullable Decoders.text))

-- |
-- Integration with the following statement:
--
-- > update album
-- > set released = $released
-- > where id = $id
--
updateAlbumReleased :: Statement Model.UpdateAlbumReleasedParams Int
updateAlbumReleased =
  Statement sql encoder decoder True
  where
    sql =
      "update album\n\
      \set released = $1\n\
      \where id = $2"
    encoder =
      mconcat 
        [ Model.updateAlbumReleasedParamsReleased >$< (Encoders.param (Encoders.nullable Encoders.date)),
          Model.updateAlbumReleasedParamsId >$< (Encoders.param (Encoders.nullable Encoders.int8))
        ]
    decoder =
      fromIntegral <$> Decoders.rowsAffected

-- |
-- Integration with the following statement:
--
-- > update album
-- > set released = $released
-- > where id = $id
-- > returning *
--
updateAlbumReleasedReturning ::
  -- | Specification of how to fold the result rows.
  Fold Model.UpdateAlbumReleasedReturningResultRow result ->
  Statement Model.UpdateAlbumReleasedReturningParams result
updateAlbumReleasedReturning (Fold step init extract) =
  Statement sql encoder decoder True
  where
    sql =
      "update album\n\
      \set released = $1\n\
      \where id = $2\n\
      \returning *"
    encoder =
      mconcat 
        [ Model.updateAlbumReleasedReturningParamsReleased >$< (Encoders.param (Encoders.nullable Encoders.date)),
          Model.updateAlbumReleasedReturningParamsId >$< (Encoders.param (Encoders.nullable Encoders.int8))
        ]
    decoder =
      fmap extract . Decoders.foldlRows step init $
        Model.UpdateAlbumReleasedReturningResultRow
          <$> (Decoders.column (Decoders.nonNullable Decoders.int8))
          <*> (Decoders.column (Decoders.nonNullable Decoders.text))
          <*> (Decoders.column (Decoders.nullable Decoders.date))
