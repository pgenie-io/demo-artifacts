module MySpaceMusicCatalogueHasql.Model where

import BasePrelude
import Data.Hashable
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Network.IP.Addr (NetAddr, IP)
import qualified Data.Aeson as Aeson

-- * Params

data InsertAlbumParams = InsertAlbumParams
  { insertAlbumParamsName :: !Text,
    insertAlbumParamsReleased :: !(Maybe (Day))
  }
  deriving (Show, Eq, Generic)

data SelectAlbumByArtistParams = SelectAlbumByArtistParams
  { selectAlbumByArtistParamsArtist :: !(Maybe (Int32))
  }
  deriving (Show, Eq, Generic)

data SelectGenreByArtistParams = SelectGenreByArtistParams
  { selectGenreByArtistParamsArtist :: !(Maybe (Int32))
  }
  deriving (Show, Eq, Generic)

data UpdateAlbumReleasedParams = UpdateAlbumReleasedParams
  { updateAlbumReleasedParamsReleased :: !(Maybe (Day)),
    updateAlbumReleasedParamsId :: !(Maybe (Int64))
  }
  deriving (Show, Eq, Generic)

data UpdateAlbumReleasedReturningParams = UpdateAlbumReleasedReturningParams
  { updateAlbumReleasedReturningParamsReleased :: !(Maybe (Day)),
    updateAlbumReleasedReturningParamsId :: !(Maybe (Int64))
  }
  deriving (Show, Eq, Generic)

-- * ResultRow

data InsertAlbumResultRow = InsertAlbumResultRow
  { insertAlbumResultRowId :: !Int64
  }
  deriving (Show, Eq, Generic)

data SelectAlbumByArtistResultRow = SelectAlbumByArtistResultRow
  { selectAlbumByArtistResultRowId :: !Int64,
    selectAlbumByArtistResultRowName :: !Text,
    selectAlbumByArtistResultRowReleased :: !(Maybe (Day))
  }
  deriving (Show, Eq, Generic)

data SelectGenreByArtistResultRow = SelectGenreByArtistResultRow
  { selectGenreByArtistResultRowId :: !Int32,
    selectGenreByArtistResultRowName :: !Text
  }
  deriving (Show, Eq, Generic)

data UpdateAlbumReleasedReturningResultRow = UpdateAlbumReleasedReturningResultRow
  { updateAlbumReleasedReturningResultRowId :: !Int64,
    updateAlbumReleasedReturningResultRowName :: !Text,
    updateAlbumReleasedReturningResultRowReleased :: !(Maybe (Day))
  }
  deriving (Show, Eq, Generic)