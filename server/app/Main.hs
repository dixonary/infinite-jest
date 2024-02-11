{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Exception (finally)
import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Network.WebSockets qualified as WS

import Data.Aeson qualified as JSON

import Data.Functor (($>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)
import System.Random.Shuffle (shuffleM)
import Types

main :: IO ()
main = do
  putStrLn "Running on :12024"
  _rooms <- newTVarIO mempty
  WS.runServer "0.0.0.0" 12024 $ app _rooms

app :: TVar JestState -> WS.ServerApp
app _rooms pendingConn = do
  conn <- WS.acceptRequest pendingConn

  clientId <- WS.receiveData @Text conn
  putStrLn $ "Received client ID: " ++ show clientId

  WS.sendTextData @Text conn "join"
  PJoin (JoinRequest roomId name) <- parseEvent <$> WS.receiveData conn
  putStrLn $ "Received join request: " ++ show roomId

  let c = Client{cid = clientId, cconn = conn, cname = name}
  createRoom roomId clientId _rooms
  updateRoom roomId (addClient c) _rooms

  let disconnect = do
        updateRoom roomId (removeConnectedClient clientId) _rooms
        deleteRoomIfEmpty roomId _rooms

  flip finally disconnect $ forever $ do
    d <- WS.receiveData conn
    case parseEvent d of
      PStart StartRequest{..} -> do
        putStrLn $ "Received start game request for " <> show roomId

        now <- getCurrentTime

        -- Choose the items
        items <- do
          includedItems <-
            take numItems
              <$> shuffleM (map (Text.take 100) includeTargets)

          otherItems <-
            take (numItems - length includeTargets) <$> shuffleM randomItem

          fmap (map (`Item` Nothing)) $ shuffleM $ includedItems <> otherItems

        updateRoom roomId (startGame items now) _rooms
      PClaim cr@ClaimRequest{..} -> do
        putStrLn $ "Received claim request: " ++ show item
        updateRoom roomId (claimItem clientId cr) _rooms
      PUnclaim UnclaimRequest{..} -> do
        putStrLn $ "Received unclaim request: " ++ show unclaim
        updateRoom roomId (unclaimItem clientId unclaim) _rooms
      PJoin JoinRequest{} -> do
        putStrLn "Received join request after joining; ignoring"
      PUnknown raw -> do
        putStrLn $ "Received message: " ++ Text.unpack raw

parseEvent :: Text -> ParsedEvent
parseEvent raw =
  let
    Event{..} = fromJust $ JSON.decodeStrictText @Event raw
   in
    fromMaybe (PUnknown payload)
      $ if
        | event == "join" -> PJoin <$> JSON.decodeStrictText payload
        | event == "start" -> PStart <$> JSON.decodeStrictText payload
        | event == "claim" -> PClaim <$> JSON.decodeStrictText payload
        | event == "unclaim" -> PUnclaim <$> JSON.decodeStrictText payload
        | otherwise -> Nothing

randomItem :: [Text]
randomItem = map Text.singleton ['A' .. 'Z']

--------------------------------------------------------------------------------
-- Room creation and updating

-- Insert a new room into the room list if it doesn't exist
createRoom :: Text -> Text -> TVar JestState -> IO ()
createRoom roomId clientId _rooms = do
  didCreate <- atomically $ do
    r <- readTVar _rooms
    when (Map.notMember roomId r)
      $ writeTVar _rooms
      $ Map.insert
        roomId
        Room
          { rid = roomId
          , clients = mempty
          , connected = mempty
          , leader = clientId
          , items = mempty
          , status = Lobby
          , started = Nothing
          }
        r
    pure $ Map.notMember roomId r
  when didCreate $ putStrLn $ "Created room " ++ show roomId

-- Update some property of a room and tell all connected clients about it
updateRoom :: Text -> (Room -> Room) -> TVar JestState -> IO ()
updateRoom roomId f _rooms = do
  r' <- atomically $ do
    r <- readTVar _rooms
    let r' = Map.adjust f roomId r
    writeTVar _rooms r'
    pure r'

  -- Broadcast the update to all connected clients
  forM_ (Map.lookup roomId r') $ \room@Room{connected, clients} ->
    forM_ (Set.elems connected) $ \c ->
      forM_ (Map.lookup c clients) $ \Client{..} ->
        WS.sendTextData cconn $ JSON.encode room

-- Delete a room if all clients have disconnected
deleteRoomIfEmpty :: Text -> TVar JestState -> IO ()
deleteRoomIfEmpty roomId _rooms = do
  deleted <- atomically $ do
    r <- readTVar _rooms
    let r' = Map.delete roomId r
    if null $ connected $ r Map.! roomId
      then writeTVar _rooms r' $> True
      else pure False
  when deleted $ putStrLn $ "Deleting room " ++ show roomId

--------------------------------------------------------------------------------
-- Room modifying operations

startGame :: [Item] -> UTCTime -> Room -> Room
startGame items time room =
  room
    { status = InProgress
    , items
    , started = Just time
    }

claimItem :: Text -> ClaimRequest -> Room -> Room
claimItem clientId ClaimRequest{..} room =
  room
    { items = map claimItem' (items room)
    }
 where
  claimItem' i@Item{..}
    | name == target = i{claim = Just Claim{by = clientId, with = item}}
    | otherwise = i

unclaimItem :: Text -> Text -> Room -> Room
unclaimItem clientId target room =
  room
    { items = map unclaimItem' (items room)
    }
 where
  unclaimItem' i@(Item name (Just Claim{by}))
    | name == target && by == clientId = i{claim = Nothing}
  unclaimItem' i = i

-- Add a new client and connect them
addClient :: Client -> Room -> Room
addClient Client{..} room =
  addConnectedClient cid
    $ room{clients = Map.insert cid Client{..} (clients room)}

-- Remove a client from a room
removeClient :: Text -> Room -> Room
removeClient c r =
  r
    { clients = Map.delete c (clients r)
    , connected = Set.delete c (connected r)
    , items = map remClaim (items r)
    }
 where
  -- Remove a claim from an item if it was made by the client we're removing
  remClaim i = case claim i of
    Just claim | by claim == c -> i{claim = Nothing}
    _ -> i

-- Connect an existing client
addConnectedClient :: Text -> Room -> Room
addConnectedClient c r = r{connected = Set.insert c (connected r)}

-- Disconnect a client
removeConnectedClient :: Text -> Room -> Room
removeConnectedClient c r = r{connected = Set.delete c (connected r)}
