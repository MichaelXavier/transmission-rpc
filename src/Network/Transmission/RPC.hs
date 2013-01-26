{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Transmission.RPC where

import Network.Transmission.RPC.Types
import Network.Transmission.RPC.Utils

import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import qualified Control.Exception.Lifted as E
import Control.Failure (Failure)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.State (gets,
                                  modify)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson (FromJSON(..),
                   Value(..),
                   fromJSON,
                   Result(..),
                   ToJSON(..),
                   encode,
                   object,
                   (.=),
                   (.:),
                   withObject,
                   json)
import Data.ByteString (ByteString)
import Data.Conduit (($$+-),
                     MonadUnsafeIO,
                     MonadThrow)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Default (Default(..))
import Data.Monoid (mempty)
import qualified Data.Vector as V

import Network.HTTP.Conduit (parseUrl,
                             RequestBody(..),
                             Request(..),
                             HttpException(..),
                             withManager,
                             Manager,
                             Response(..),
                             http)
import Network.HTTP.Types (Status(..))
import Network.HTTP.Types.Header (Header,
                                  HeaderName)

torrentStart :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentStart = ffmap unUnit . makeRequest . RPCRequest TorrentStart

torrentStartNow :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentStartNow = ffmap unUnit . makeRequest . RPCRequest TorrentStartNow


torrentStop :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentStop = ffmap unUnit . makeRequest . RPCRequest TorrentStop

torrentVerify :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentVerify = ffmap unUnit . makeRequest . RPCRequest TorrentVerify

torrentReannounce :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentReannounce = ffmap unUnit . makeRequest . RPCRequest TorrentReannounce

newtype TorrentCtlOptions = TorrentCtlOptions {
  torrentCtlIds :: [TorrentId]
} deriving (Show, Eq)

instance ToJSON TorrentCtlOptions where
  toJSON (TorrentCtlOptions ids) = object ["ids" .= ids]

torrentSet :: a
torrentSet = undefined

torrentGet :: TorrentGetOptions -> TransmissionM IO (RPCResponse [Torrent])
torrentGet opts = ffmap unTorrentList $ makeRequest req
  where req = RPCRequest TorrentGet opts

newtype TorrentGetOptions = TorrentGetOptions {
  torrentGetIds :: [TorrentId]
} deriving (Show, Eq, Default)

instance ToJSON TorrentGetOptions where
  toJSON opts = object ["ids" .= torrentGetIds opts]

torrentAdd :: a
torrentAdd = undefined

torrentRemove :: TorrentRemoveOptions -> TransmissionM IO (RPCResponse ())
torrentRemove opts = ffmap unUnit $ makeRequest req
  where req = RPCRequest TorrentRemove opts

newtype TorrentRemoveOptions = TorrentRemoveOptions {
  torrentRemoveIds :: [TorrentId]
} deriving (Show, Eq, Default)

instance ToJSON TorrentRemoveOptions where
  toJSON opts = object ["ids" .= torrentRemoveIds opts]

torrentSetLocation :: TorrentSetLocationOptions -> TransmissionM IO (RPCResponse ())
torrentSetLocation opts = ffmap unUnit $ makeRequest req
  where req = RPCRequest TorrentSetLocation opts

data TorrentSetLocationOptions = TorrentSetLocationOptions {
  torrentSetLocationIds      :: [TorrentId],
  torrentSetLocationLocation :: FilePath,
  torrentSetLocationMove     :: Bool
} deriving (Show, Eq)

instance Default TorrentSetLocationOptions where
  def = TorrentSetLocationOptions mempty mempty False

instance ToJSON TorrentSetLocationOptions where
  toJSON opts = object ["ids"      .= torrentSetLocationIds opts,
                        "location" .= torrentSetLocationLocation opts,
                        "move"     .= torrentSetLocationMove opts]

sessionSet :: a
sessionSet = undefined

sessionGet :: a
sessionGet = undefined

sessionStats :: a
sessionStats = undefined

blocklistUpdate :: a
blocklistUpdate = undefined

portTest :: a
portTest = undefined

sessionClose :: a
sessionClose = undefined

queueMoveTop :: a
queueMoveTop = undefined

queueMoveUp :: a
queueMoveUp = undefined

queueMoveDown :: a
queueMoveDown = undefined

queueMoveBottom :: a
queueMoveBottom = undefined

-- helpers
--TODO: either
--TODO: move manager to TransmissionM
-- TODO: capture errors and put them in RPCError
--makeRequest :: (FromJSON a,
--                ToJSON arg,
--                MonadIO m,
--                MonadThrow m,
--                MonadUnsafeIO m,
--                MonadBaseControl IO m,
--                Failure HttpException m) =>
--     RPCRequest arg -> TransmissionM m (RPCResponse a)
makeRequest rpcReq = do
  base <- gets transmissionWebUrl
  req' <- parseUrl $ adjustPath base
  let req = prepareRequest req'
  -- TODO: send body
  E.catch (requestAndParse req)
          (retryWithSessionId req)
  where prepareRequest req = req { method = "POST",
                                   requestBody = generateBody rpcReq}

--requestAndParse
--  :: (FromJSON a,
--      MonadIO m,
--      MonadBaseControl IO m,
--      MonadUnsafeIO m,
--      MonadThrow m) =>
--     Request (ResourceT m)
--     -> m (Result a)
     --TODO: does not gauarantee uniqueness on header key
requestAndParse req = do 
  sid                 <- gets transmissionSessionId
  withManager $ \manager -> do
    Response _ _ _ body <- http (setSession sid) manager
    body $$+- sinkParser (fmap convert json)
  where setSession (Just sid) = req { requestHeaders = (sessionIdKey, sid):(requestHeaders req)}
        setSession _ = req
        convert      = resultToResponse . fromJSON


resultToResponse :: FromJSON a => Result (RPCResponse a) -> RPCResponse a
resultToResponse (Success a) = a--RPCSuccess a
resultToResponse (Error e)   = RPCError e--RPCError e

--retryWithSessionId
--  :: (FromJSON a, MonadIO m, MonadBaseControl IO m, MonadUnsafeIO m,
--      MonadThrow m) =>
--     Request
--       (ResourceT
--          (Control.Monad.Trans.State.Lazy.StateT ClientConfiguration m))
--     -> HttpException
--     -> TransmissionM m (Result a)
--retryWithSessionId :: (FromJSON b, Monad m) => Request a -> HttpException -> TransmissionM m (Result b)
--TODO: check type
retryWithSessionId req e@(StatusCodeException (Status 409 _) headers) = do
  --liftIO $ print $ extractSessionId headers
  case extractSessionId headers of
    Just sid -> modify (setSessionId sid) >> requestAndParse req
    Nothing  -> E.throw e
  where setSessionId sid config = config { transmissionSessionId = Just sid}
retryWithSessionId _ e = E.throw e
  
extractSessionId :: [Header] -> Maybe ByteString
extractSessionId = lookup sessionIdKey

sessionIdKey :: HeaderName
sessionIdKey = "X-Transmission-Session-Id"

adjustPath :: String -> String
adjustPath = (++ "/rpc")

generateBody :: ToJSON arg => RPCRequest arg -> RequestBody m
generateBody = RequestBodyLBS . encode

-- response parsing boilerplate

newtype TorrentList = TorrentList { unTorrentList :: [Torrent] }

instance FromJSON TorrentList where
  parseJSON = withObject "[Torrent]" $ \v -> TorrentList <$> v .: "torrents"

unUnit :: Unit -> ()
unUnit = const ()
