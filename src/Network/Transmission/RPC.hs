{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Transmission.RPC where

import Network.Transmission.RPC.Types
import Network.Transmission.RPC.Utils

import Control.Monad.IO.Class
import Control.Applicative ((<$>), (<*>))
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
import Data.Text (Text)
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
torrentStart = makeRequest_ . RPCRequest TorrentStart

torrentStartNow :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentStartNow = makeRequest_ . RPCRequest TorrentStartNow


torrentStop :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentStop = makeRequest_ . RPCRequest TorrentStop

torrentVerify :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentVerify = makeRequest_ . RPCRequest TorrentVerify

torrentReannounce :: TorrentCtlOptions -> TransmissionM IO (RPCResponse ())
torrentReannounce = makeRequest_ . RPCRequest TorrentReannounce

newtype TorrentCtlOptions = TorrentCtlOptions {
  torrentCtlIds :: [TorrentId]
} deriving (Show, Eq)

instance ToJSON TorrentCtlOptions where
  toJSON (TorrentCtlOptions ids) = object ["ids" .= ids]

torrentSet :: [TorrentSetOption] -> TransmissionM IO (RPCResponse ())
torrentSet = makeRequest_ . RPCRequest TorrentSet . TorrentSetOptions

data TorrentSetOption = SetBandwithPriority TorrentPriority         |
                        SetDownloadLimit KiloBytesPerSecond         |
                        SetDownloadLimited Bool                     |
                        SetFilesWanted [Text]                       |
                        SetFilesUnwanted [Text]                     |
                        SetHonorsSessionLimits Bool                 |
                        SetIds [TorrentId]                          |
                        SetLocation FilePath                        |
                        SetPeerLimit Integer                        |
                        SetPriorityHigh [Text]                      |
                        SetPriorityLow [Text]                       |
                        SetPriorityNormal [Text]                    |
                        SetQueuePosition Integer                    |
                        SetSeedIdleLimitMinutes Integer             |
                        SetSeedIdleMode InactiveMode                |
                        SetTorrentSeedRatioLimit Rational           |
                        SetSeedRatioMode RatioMode                  |
                        SetTrackerAdd [TrackerUrl]                  |
                        SetTrackerRemove [TrackerId]                |
                        SetTrackerReplace [(TrackerId, TrackerUrl)] |
                        SetUploadimit KiloBytesPerSecond            |
                        SetUploadLimited Bool deriving (Show, Eq)

torrentGet :: [TorrentId] -> TransmissionM IO (RPCResponse [Torrent])
torrentGet ids = ffmap unTorrentList $ makeRequest req
  where req = RPCRequest TorrentGet $ IdList ids

torrentAdd :: Either FilePath MetaInfo -> [TorrentAddOption] -> TransmissionM IO (RPCResponse TorrentAddedResponse)
torrentAdd toAdd opts = makeRequest req
  where req = RPCRequest TorrentAdd $ TorrentAddOptions toAdd opts

data TorrentAddOption = TorrentAddCookies Text                       |
                        TorrentAddPaused Bool                        |
                        TorrentAddPeerLimit Integer                  |
                        TorrentAddBandwidthPriority TorrentPriority  |
                        TorrentAddFilesWanted [Text]                 |
                        TorrentAddFilesUnWanted [Text]               |
                        TorrentAddPriorityHigh [Text]                |
                        TorrentAddPriorityLow [Text]                 |
                        TorrentAddPriorityNormal [Text] deriving (Show, Eq)


data TorrentAddedResponse = TorrentAddedResponse { 
  torrentAddId         :: TorrentId,
  torrentAddName       :: Text,
  torrentAddHashString :: Text
} deriving (Show, Eq)

instance FromJSON TorrentAddedResponse where
  parseJSON = withObject "TorrentAddedResponse" parseResponse
    where parseResponse v = TorrentAddedResponse <$> v .: "id"
                                                 <*> v .: "name"
                                                 <*> v .: "hashString"

torrentRemove :: [TorrentId] -> TransmissionM IO (RPCResponse ())
torrentRemove ids = makeRequest_ req
  where req = RPCRequest TorrentRemove $ IdList ids

torrentSetLocation :: TorrentSetLocationOptions -> TransmissionM IO (RPCResponse ())
torrentSetLocation opts = makeRequest_ req
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

sessionSet :: [SessionSetOption] -> TransmissionM IO (RPCResponse ())
sessionSet = makeRequest_ . RPCRequest SessionSet . SessionSetOptions

data SessionSetOption = SetAltSpeedDown KiloBytesPerSecond    |
                        SetAltSpeedEnabled Bool               |
                        SetAltSpeedTimeBegin Time             |
                        SetAltSpeedTimeEnabled Bool           |
                        SetAltSpeedTimeEnd Time               |
                        SetAltSpeedTimeDay [Day]              | -- TODO: unwrap
                        SetAltSpeedUp KiloBytesPerSecond      |
                        SetBlocklistUrl Text                  |
                        SetBlocklistEnabled Bool              |
                        SetCacheSizeMb Rational               |
                        SetDownloadDir FilePath               |
                        SetDownloadQueueSize Integer          |
                        SetDownloadQueueEnabled Bool          |
                        SetDhtEnabled Bool                    |
                        SetEncryption EncryptionPreference    |
                        SetIdleSeedingLimit Integer           |
                        SetIdleSeedingLimitEnabled Bool       |
                        SetIncompleteDir FilePath             |
                        SetIncompleteDirEnabled Bool          |
                        SetLpdEnabled Bool                    | -- local peer discovery
                        SetPeerLimitGlobal Integer            |
                        SetPeerLimitPerTorrent Integer        |
                        SetPexEnabled Bool                    |
                        SetPeerPort Integer                   | -- port
                        SetPeerPortRandomOnStart Bool         |
                        SetPortForwardingEnabled Bool         |
                        SetQueueStalledEnabled Bool           |
                        SetQueueStalledMinutes Integer        |
                        SetRenamePartialFiles Bool            |
                        SetScriptTorrentDoneFilename FilePath |
                        SetScriptTorrentDoneEnabled Bool      |
                        SetSeedRatioLimit Rational            |
                        SetSeedRatioLimited Bool              |
                        SetSeedQueueSize Integer              |
                        SetSeedQueueEnabled Bool              |
                        SetSpeedLimitDown KiloBytesPerSecond  |
                        SetSpeedLimitDownEnabled Bool         |
                        SetSpeedLimitUp KiloBytesPerSecond    |
                        SetSpeedLimitUpEnabled Bool           |
                        SetStartAddedTorrents Bool            |
                        SetTrashOriginalTorrentFiles Bool     |
                        SetUtpEnabled Bool deriving (Show, Eq)

sessionGet :: TransmissionM IO (RPCResponse TransmissionSession)
sessionGet = makeRequest $ RPCRequest SessionGet ()

sessionStats :: TransmissionM IO (RPCResponse SessionStatistics)
sessionStats = makeRequest $ RPCRequest SessionStats ()

-- why does this not take arguments?
blocklistUpdate :: TransmissionM IO (RPCResponse Integer)
blocklistUpdate = ffmap unBlocklistSize $ makeRequest req 
  where req = RPCRequest BlocklistUpdate ()

portTest :: TransmissionM IO (RPCResponse Bool)
portTest = ffmap unPortIsOpen $ makeRequest req
  where req = RPCRequest PortTest ()

sessionClose :: TransmissionM IO (RPCResponse ())
sessionClose = makeRequest_ $ RPCRequest SessionClose ()

queueMoveTop :: [TorrentId] -> TransmissionM IO (RPCResponse ())
queueMoveTop = makeRequest_ . RPCRequest QueueMoveTop . IdList

queueMoveUp :: [TorrentId] -> TransmissionM IO (RPCResponse ())
queueMoveUp = makeRequest_ . RPCRequest QueueMoveUp . IdList


queueMoveDown :: [TorrentId] -> TransmissionM IO (RPCResponse ())
queueMoveDown = makeRequest_ . RPCRequest QueueMoveDown . IdList

queueMoveBottom :: [TorrentId] -> TransmissionM IO (RPCResponse ())
queueMoveBottom = makeRequest_ . RPCRequest QueueMoveBottom . IdList

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

makeRequest_ req = ffmap unUnit $ makeRequest req

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

newtype BlocklistSize = BlocklistSize { unBlocklistSize :: Integer }

instance FromJSON BlocklistSize where
  parseJSON = withObject "BlocklistSize" $ \v -> BlocklistSize <$> v .: "blocklist-size"

newtype PortIsOpen = PortIsOpen { unPortIsOpen :: Bool }

instance FromJSON PortIsOpen where
  parseJSON = withObject "PortIsOpen" $ \v -> PortIsOpen <$> v .: "port-is-open"

newtype IdList = IdList {
  unIdList :: [TorrentId]
} deriving (Show, Eq, Default)

instance ToJSON IdList where
  toJSON opts = object ["ids" .= unIdList opts]

data TorrentAddOptions = TorrentAddOptions (Either FilePath MetaInfo) [TorrentAddOption]

instance ToJSON TorrentAddOptions where
  toJSON (TorrentAddOptions toAdd opts) = object $ toAddPair:(map toPair opts)
    where toAddPair = either ("filename" .=)  ("metainfo" .=) toAdd
          toPair (TorrentAddCookies txt)           = "cookies" .= txt
          toPair (TorrentAddPaused bool)           = "paused" .= bool
          toPair (TorrentAddPeerLimit num)         = "peer-limit" .= num
          toPair (TorrentAddBandwidthPriority pri) = "bandwidthPriority" .= pri
          toPair (TorrentAddFilesWanted fs)        = "files-wanted" .= fs
          toPair (TorrentAddFilesUnWanted fs)      = "files-unwanted" .= fs
          toPair (TorrentAddPriorityHigh fs)       = "priority-high" .= fs
          toPair (TorrentAddPriorityLow fs)        = "priority-low" .= fs
          toPair (TorrentAddPriorityNormal fs)     = "priority-normal" .= fs

newtype TorrentSetOptions = TorrentSetOptions [TorrentSetOption]

instance ToJSON TorrentSetOptions where
  toJSON (TorrentSetOptions opts) = object $ map toPair opts
    where toPair (SetBandwithPriority pri)      = "bandwidthPriority"   .= pri
          toPair (SetDownloadLimit rate)        = "downloadLimit"       .= rate
          toPair (SetDownloadLimited bool)      = "downloadLimited"     .= bool
          toPair (SetFilesWanted fs)            = "files-wanted"        .= fs
          toPair (SetFilesUnwanted fs)          = "files-unwanted"      .= fs
          toPair (SetHonorsSessionLimits bool)  = "honorsSessionLimits" .= bool
          toPair (SetIds ids)                   = "ids"                 .= ids
          toPair (SetLocation f)                = "location"            .= f
          toPair (SetPeerLimit lim)             = "peer-limit"          .= lim
          toPair (SetPriorityHigh fs)           = "priority-high"       .= fs
          toPair (SetPriorityLow fs)            = "priority-low"        .= fs
          toPair (SetPriorityNormal fs)         = "priority-normal"     .= fs
          toPair (SetQueuePosition pos)         = "queuePosition"       .= pos
          toPair (SetSeedIdleLimitMinutes mins) = "seedIdleLimit"       .= mins
          toPair (SetSeedIdleMode mode)         = "seedIdleMode"        .= mode
          toPair (SetTorrentSeedRatioLimit lim) = "seedRatioLimit"      .= lim
          toPair (SetSeedRatioMode mode)        = "seedRatioMode"       .= mode
          toPair (SetTrackerAdd urls)           = "trackerAdd"          .= urls
          toPair (SetTrackerRemove ids)         = "trackerRemove"       .= ids
          toPair (SetTrackerReplace pairs)      = "trackerReplace"      .= pairs
          toPair (SetUploadimit lim)            = "uploadLimit"         .= lim
          toPair (SetUploadLimited bool)        = "uploadLimited"       .= bool

newtype SessionSetOptions = SessionSetOptions [SessionSetOption]

instance ToJSON SessionSetOptions where
  toJSON (SessionSetOptions opts) = object $ map toPair opts
    where toPair (SetAltSpeedDown rate)              = "alt-speed-down"               .= rate
          toPair (SetAltSpeedEnabled bool)           = "alt-speed-enabled"            .= bool
          toPair (SetAltSpeedTimeBegin time)         = "alt-speed-time-begin"         .= time
          toPair (SetAltSpeedTimeEnabled bool)       = "alt-speed-time-enabled"       .= bool
          toPair (SetAltSpeedTimeEnd time)           = "alt-speed-time-end"           .= time
          toPair (SetAltSpeedTimeDay days)           = "alt-speed-time-day"           .= days
          toPair (SetAltSpeedUp rate)                = "alt-speed-up"                 .= rate
          toPair (SetBlocklistUrl text)              = "blocklist-url"                .= text
          toPair (SetBlocklistEnabled bool)          = "blocklist-enabled"            .= bool
          toPair (SetCacheSizeMb size)               = "cache-size-mb"                .= size
          toPair (SetDownloadDir f)                  = "download-dir"                 .= f
          toPair (SetDownloadQueueSize sz)           = "download-queue-size"          .= sz
          toPair (SetDownloadQueueEnabled bool)      = "download-queue-enabled"       .= bool
          toPair (SetDhtEnabled bool)                = "dht-enabled"                  .= bool
          toPair (SetEncryption pref)                = "encryption"                   .= pref
          toPair (SetIdleSeedingLimit lim)           = "idle-seeding-limit"           .= lim
          toPair (SetIdleSeedingLimitEnabled bool)   = "idle-seeding-limit-enabled"   .= bool
          toPair (SetIncompleteDir f)                = "incomplete-dir"               .= f
          toPair (SetIncompleteDirEnabled bool)      = "incomplete-dir-enabled"       .= bool
          toPair (SetLpdEnabled bool)                = "lpd-enabled"                  .= bool
          toPair (SetPeerLimitGlobal lim)            = "peer-limit-global"            .= lim
          toPair (SetPeerLimitPerTorrent lim)        = "peer-limit-per-torrent"       .= lim
          toPair (SetPexEnabled bool)                = "pex-enabled"                  .= bool
          toPair (SetPeerPort p)                     = "peer-port"                    .= p
          toPair (SetPeerPortRandomOnStart bool)     = "peer-port-random-on-start"    .= bool
          toPair (SetPortForwardingEnabled bool)     = "port-forwarding-enabled"      .= bool
          toPair (SetQueueStalledEnabled bool)       = "queue-stalled-enabled"        .= bool
          toPair (SetQueueStalledMinutes mins)       = "queue-stalled-minutes"        .= mins
          toPair (SetRenamePartialFiles bool)        = "rename-partial-files"         .= bool
          toPair (SetScriptTorrentDoneFilename f)    = "script-torrent-done-filename" .= f
          toPair (SetScriptTorrentDoneEnabled bool)  = "script-torrent-done-enabled"  .= bool
          toPair (SetSeedRatioLimit lim)             = "seedRatioLimit"               .= lim
          toPair (SetSeedRatioLimited bool)          = "seedRatioLimited"             .= bool
          toPair (SetSeedQueueSize sz)               = "seed-queue-size"              .= sz
          toPair (SetSeedQueueEnabled bool)          = "seed-queue-enabled"           .= bool
          toPair (SetSpeedLimitDown kbps)            = "speed-limit-down"             .= kbps
          toPair (SetSpeedLimitDownEnabled bool)     = "speed-limit-down-enabled"     .= bool
          toPair (SetSpeedLimitUp kbps)              = "speed-limit-up"               .= kbps
          toPair (SetSpeedLimitUpEnabled bool)       = "speed-limit-up-enabled"       .= bool
          toPair (SetStartAddedTorrents bool)        = "start-added-torrents"         .= bool
          toPair (SetTrashOriginalTorrentFiles bool) = "trash-original-torrent-files" .= bool
          toPair (SetUtpEnabled bool)                = "utp-enabled"                  .= bool

unUnit :: Unit -> ()
unUnit = const ()   
