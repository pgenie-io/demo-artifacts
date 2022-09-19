module PgenieArtifacts.MySpace.MusicCatalogue.Hasql.Model where

import BasePrelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeZone, UTCTime)

-- * Parameters

-- |
-- Parameters for the following statement:
--
-- > select id, genre.name
-- > from genre
-- > left join album_genre on album_genre.genre = genre.id
-- > left join album_artist on album_artist.album = album_genre.album
-- > where album_artist.artist = $artist
--
newtype SelectGenreByArtistParams = SelectGenreByArtistParams
  { -- | @$artist@ parameter.
    selectGenreByArtistParamsArtist :: Maybe Int32
  }

-- |
-- Parameters for the following statement:
--
-- > select album.*
-- > from album
-- > left join album_artist on album_artist.album = album.id
-- > where artist = $artist
--
newtype SelectAlbumByArtistParams = SelectAlbumByArtistParams
  { -- | @$artist@ parameter.
    selectAlbumByArtistParamsArtist :: Maybe Int32
  }

-- |
-- Parameters for the following statement:
--
-- > insert into album (name, released)
-- > values ($name, $released)
-- > returning id
--
data InsertAlbumParams = InsertAlbumParams
  { -- | @$name@ parameter.
    insertAlbumParamsName :: !Text,
    -- | @$released@ parameter.
    insertAlbumParamsReleased :: !(Maybe Day)
  }

-- * Result rows

-- |
-- Result row of the following statement:
--
-- > select id, genre.name
-- > from genre
-- > left join album_genre on album_genre.genre = genre.id
-- > left join album_artist on album_artist.album = album_genre.album
-- > where album_artist.artist = $artist
--
data SelectGenreByArtistResultRow = SelectGenreByArtistResultRow
  { -- | @"id"@ column.
    selectGenreByArtistResultRowId :: !Int32,
    -- | @"name"@ column.
    selectGenreByArtistResultRowName :: !Text
  }

-- |
-- Result row of the following statement:
--
-- > select album.*
-- > from album
-- > left join album_artist on album_artist.album = album.id
-- > where artist = $artist
--
data SelectAlbumByArtistResultRow = SelectAlbumByArtistResultRow
  { -- | @"id"@ column.
    selectAlbumByArtistResultRowId :: !Int64,
    -- | @"name"@ column.
    selectAlbumByArtistResultRowName :: !Text,
    -- | @"released"@ column.
    selectAlbumByArtistResultRowReleased :: !(Maybe Day)
  }

-- |
-- Result row of the following statement:
--
-- > insert into album (name, released)
-- > values ($name, $released)
-- > returning id
--
newtype InsertAlbumResultRow = InsertAlbumResultRow
  { -- | @"id"@ column.
    insertAlbumResultRowId :: Int64
  }


