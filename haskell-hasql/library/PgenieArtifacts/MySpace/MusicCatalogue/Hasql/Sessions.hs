-- | Session definitions.
module PgenieArtifacts.MySpace.MusicCatalogue.Hasql.Sessions where

import BasePrelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeZone, UTCTime)
import Hasql.Session (Session)
import qualified Control.Foldl as Foldl
import qualified Hasql.Session as Session
import qualified PgenieArtifacts.MySpace.MusicCatalogue.Hasql.Model as Model
import qualified PgenieArtifacts.MySpace.MusicCatalogue.Hasql.Statements as Statements

-- | Execute the following statement folding the result rows:
-- > insert into album (name, released)
-- > values ($name, $released)
-- > returning id
insertAlbum ::
  -- | @$name@ statement param.
  Maybe Text ->
  -- | @$released@ statement param.
  Maybe Day ->
  -- | How to fold the rows of the result.
  -- 
  -- E.g., you can use 'Foldl.vec' to produce a vector,
  -- 'Foldl.list' to produce a list.
  -- You can also applicatively compose folds efficiently
  -- to do aggregates.
  Foldl.Fold Model.InsertAlbumResultRow rows ->
  -- | Session executing the statement and
  -- producing the folded rows.
  Session rows
insertAlbum namePrm releasedPrm fold =
  Session.statement params $
    Statements.insertAlbumFolding fold
  where
    params =
      Model.InsertAlbumParams namePrm releasedPrm

-- | Execute the following statement folding the result rows:
-- > select album.*
-- > from album
-- > left join album_artist on album_artist.album = album.id
-- > where artist = $artist
selectAlbumByArtist ::
  -- | @$artist@ statement param.
  Maybe Int32 ->
  -- | How to fold the rows of the result.
  -- 
  -- E.g., you can use 'Foldl.vec' to produce a vector,
  -- 'Foldl.list' to produce a list.
  -- You can also applicatively compose folds efficiently
  -- to do aggregates.
  Foldl.Fold Model.SelectAlbumByArtistResultRow rows ->
  -- | Session executing the statement and
  -- producing the folded rows.
  Session rows
selectAlbumByArtist artistPrm fold =
  Session.statement params $
    Statements.selectAlbumByArtistFolding fold
  where
    params =
      Model.SelectAlbumByArtistParams artistPrm

-- | Execute the following statement folding the result rows:
-- > select id, genre.name
-- > from genre
-- > left join album_genre on album_genre.genre = genre.id
-- > left join album_artist on album_artist.album = album_genre.album
-- > where album_artist.artist = $artist
selectGenreByArtist ::
  -- | @$artist@ statement param.
  Maybe Int32 ->
  -- | How to fold the rows of the result.
  -- 
  -- E.g., you can use 'Foldl.vec' to produce a vector,
  -- 'Foldl.list' to produce a list.
  -- You can also applicatively compose folds efficiently
  -- to do aggregates.
  Foldl.Fold Model.SelectGenreByArtistResultRow rows ->
  -- | Session executing the statement and
  -- producing the folded rows.
  Session rows
selectGenreByArtist artistPrm fold =
  Session.statement params $
    Statements.selectGenreByArtistFolding fold
  where
    params =
      Model.SelectGenreByArtistParams artistPrm