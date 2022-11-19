-- | Session definitions according to every statement.
module MySpaceMusicCatalogueHasql.Sessions where

import BasePrelude
import Hasql.Session
import Control.Foldl (Fold)
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Network.IP.Addr (NetAddr, IP)
import qualified MySpaceMusicCatalogueHasql.Model as Model
import qualified MySpaceMusicCatalogueHasql.Statements as Statements
import qualified Control.Foldl as Foldl
import qualified Data.Aeson as Aeson

-- | Execute the following statement producing exactly one row:
--
-- > insert into album (name, released)
-- > values ($name, $released)
-- > returning id
-- > 
insertAlbum ::
  -- | @$name@ statement placeholder value.
  Text ->
  -- | @$released@ statement placeholder value.
  Maybe Day ->
  -- | Session executing the statement and
  -- producing the only result row.
  Session Model.InsertAlbumResultRow
insertAlbum nameParam releasedParam =
  statement params Statements.insertAlbum
  where
    params =
      Model.InsertAlbumParams nameParam releasedParam

-- | Execute the following statement folding the result rows:
--
-- > select album.*
-- > from album
-- > left join album_artist on album_artist.album = album.id
-- > where artist = $artist
selectAlbumByArtist ::
  -- | @$artist@ statement placeholder value.
  Maybe Int32 ->
  -- | How to fold the rows of the result.
  --
  -- E.g., you can use 'Foldl.vec' to produce a vector,
  -- 'Foldl.list' to produce a list.
  -- You can also applicatively compose folds
  -- to do aggregates efficiently.
  Fold Model.SelectAlbumByArtistResultRow rows ->
  -- | Session executing the statement and
  -- producing the folded rows.
  Session rows
selectAlbumByArtist artistParam =
  statement params . Statements.selectAlbumByArtist
  where
    params =
      Model.SelectAlbumByArtistParams artistParam

-- | Execute the following statement folding the result rows:
--
-- > select id, genre.name
-- > from genre
-- > left join album_genre on album_genre.genre = genre.id
-- > left join album_artist on album_artist.album = album_genre.album
-- > where album_artist.artist = $artist
selectGenreByArtist ::
  -- | @$artist@ statement placeholder value.
  Maybe Int32 ->
  -- | How to fold the rows of the result.
  --
  -- E.g., you can use 'Foldl.vec' to produce a vector,
  -- 'Foldl.list' to produce a list.
  -- You can also applicatively compose folds
  -- to do aggregates efficiently.
  Fold Model.SelectGenreByArtistResultRow rows ->
  -- | Session executing the statement and
  -- producing the folded rows.
  Session rows
selectGenreByArtist artistParam =
  statement params . Statements.selectGenreByArtist
  where
    params =
      Model.SelectGenreByArtistParams artistParam

-- | Execute the following statement producing the amount of
-- rows affected by it:
--
-- > update album
-- > set released = $released
-- > where id = $id
updateAlbumReleased ::
  -- | @$released@ statement placeholder value.
  Maybe Day ->
  -- | @$id@ statement placeholder value.
  Maybe Int64 ->
  -- | Session executing the statement and
  -- producing the amount of affected rows.
  Session Int
updateAlbumReleased releasedParam idParam =
  statement params Statements.updateAlbumReleased
  where
    params =
      Model.UpdateAlbumReleasedParams releasedParam idParam

-- | Execute the following statement folding the result rows:
--
-- > update album
-- > set released = $released
-- > where id = $id
-- > returning *
-- > 
updateAlbumReleasedReturning ::
  -- | @$released@ statement placeholder value.
  Maybe Day ->
  -- | @$id@ statement placeholder value.
  Maybe Int64 ->
  -- | How to fold the rows of the result.
  --
  -- E.g., you can use 'Foldl.vec' to produce a vector,
  -- 'Foldl.list' to produce a list.
  -- You can also applicatively compose folds
  -- to do aggregates efficiently.
  Fold Model.UpdateAlbumReleasedReturningResultRow rows ->
  -- | Session executing the statement and
  -- producing the folded rows.
  Session rows
updateAlbumReleasedReturning releasedParam idParam =
  statement params . Statements.updateAlbumReleasedReturning
  where
    params =
      Model.UpdateAlbumReleasedReturningParams releasedParam idParam