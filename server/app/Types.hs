{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Network.WebSockets qualified as WS

import Data.Aeson qualified as JSON
import Data.Time.Clock

type JestState = Map Text Room

data Event = Event
  { event :: Text
  , payload :: Text
  }

instance JSON.FromJSON Event where
  parseJSON = JSON.withObject "Event" $ \o -> do
    event <- o JSON..: "event"
    payload <- o JSON..: "payload"
    return Event{..}

data ParsedEvent
  = PJoin JoinRequest
  | PStart StartRequest
  | PClaim ClaimRequest
  | PUnclaim UnclaimRequest
  | PUnknown Text

data Room = Room
  { rid :: Text
  , clients :: Map Text Client
  , connected :: Set Text
  , leader :: Text
  , items :: [Item]
  , status :: Status
  , started :: Maybe UTCTime
  }
  deriving (Generic, Show)

data Item = Item
  { name :: Text
  , claim :: Maybe Claim
  }
  deriving (Show, Generic)

data Claim = Claim
  { by :: Text
  , with :: Text
  }
  deriving (Show, Generic)

data Client = Client
  { cid :: Text
  , cconn :: WS.Connection
  , cname :: Text
  }

instance Show Client where show Client{..} = show (cid, cname)

instance JSON.ToJSON Client where
  toJSON :: Client -> JSON.Value
  toJSON Client{..} =
    JSON.object
      [ "id" JSON..= cid
      , "name" JSON..= cname
      ]

data JoinRequest = JoinRequest
  { room :: Text
  , name :: Text
  }
  deriving (Generic)

data Status = Lobby | InProgress | Finished
  deriving (Show, Generic)

data StartRequest = StartRequest
  { numItems :: Int
  , includeTargets :: [Text]
  }
  deriving (Generic)

data ClaimRequest = ClaimRequest
  { item :: Text
  , target :: Text
  }
  deriving (Generic)

newtype UnclaimRequest = UnclaimRequest
  { unclaim :: Text
  }
  deriving (Generic)

instance JSON.FromJSON ClaimRequest
instance JSON.ToJSON ClaimRequest

instance JSON.FromJSON UnclaimRequest
instance JSON.ToJSON UnclaimRequest

instance JSON.FromJSON StartRequest
instance JSON.ToJSON StartRequest

instance JSON.FromJSON JoinRequest
instance JSON.ToJSON JoinRequest

instance JSON.FromJSON Status
instance JSON.ToJSON Status

instance JSON.FromJSON Claim
instance JSON.ToJSON Claim

instance JSON.ToJSON Room
instance JSON.ToJSON Item