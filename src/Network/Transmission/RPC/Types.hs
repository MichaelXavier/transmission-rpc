{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Transmission.RPC.Types (RPCRequest(..),
                                       RPCMethod(..),
                                       RPCResponse(..),
                                       ClientConfiguration(..),
                                       Torrent(..),
                                       TorrentId(..),
                                       Unit(..),
                                       TransmissionM) where

import Control.Applicative ((<$>),
                            (<|>),
                            (*>),
                            pure,
                            Applicative(..))
import Control.Monad.Trans.State (StateT)
import Data.Aeson (FromJSON(..),
                   ToJSON(..),
                   Value(..),
                   (.=),
                   (.:),
                   withObject,
                   withNumber,
                   withText,
                   object)
import qualified Data.Attoparsec.Number as N
import Data.ByteString (ByteString)
import Data.Default
import Data.Text (Text)

data RPCRequest arguments = RPCRequest RPCMethod arguments deriving (Show, Eq)

instance ToJSON arg => ToJSON (RPCRequest arg) where
  toJSON (RPCRequest rpcMethod arg) = object ["arguments" .= arg,
                                              "method" .= rpcMethod]

data RPCResponse a = RPCSuccess a | RPCError String deriving (Show, Eq)

data Unit = Unit deriving (Show, Eq)

instance FromJSON Unit where
  parseJSON _ = pure Unit

instance FromJSON a => FromJSON (RPCResponse a) where
  parseJSON (Object v) = RPCSuccess <$> (v .: "arguments") <|>
                         showError v
  parseJSON v          = showError v

instance Functor RPCResponse where
  fmap _ (RPCError e)   = RPCError e
  fmap f (RPCSuccess x) = RPCSuccess (f x)

instance Applicative RPCResponse where
  pure = RPCSuccess
  RPCError e <*> _  = RPCError e
  RPCSuccess f <*> x  = fmap f x

data Torrent = Torrent {
  torrentActivityDate            :: Integer,
  torrentAddedDate               :: Integer,
  torrentBandwidthPriority       :: Integer,
  torrentComment                 :: Text,
  torrentCorruptEver             :: Integer,
  torrentCreator                 :: Text,
  torrentDateCreated             :: Integer,
  torrentDesiredAvailable        :: Integer,
  torrentDoneDate                :: Integer,
  torrentDownloadDir             :: Text,
  torrentDownloadedEver          :: Integer,
  torrentDownloadLimit           :: Integer,
  torrentDownloadLimited         :: Bool,
  torrentError                   :: Integer,
  torrentErrorString             :: Text,
  torrentEta                     :: Integer,
  torrentEtaIdle                 :: Integer,
  torrentFiles                   :: [TorrentFile],
  torrentFileStats               :: [FileStat],
  torrentHashString              :: Text,
  torrentHaveUnchecked           :: Integer,
  torrentHaveValid               :: Integer,
  torrentHonorsSessionLimits     :: Bool,
  torrentId                      :: Integer,
  torrentIsFinished              :: Bool,
  torrentIsPrivate               :: Bool,
  torrentIsStalled               :: Bool,
  torrentLeftUntilDone           :: Integer,
  torrentMagnetLink              :: Integer,
  torrentManualAnnounceTime      :: Integer,
  torrentMaxConnectedPeers       :: Integer,
  torrentMetadataPercentComplete :: Rational,
  torrentName                    :: Text,
  torrentPeerLimit               :: Integer,
  torrentPeers                   :: [Peer],
  torrentPeersConnected          :: Integer,
  torrentPeersFrom               :: PeersFrom,
  torrentPeersGettingFromUs      :: Integer,
  torrentPeersSendingToUs        :: Integer,
  torrentPercentDone             :: Rational,
  torrentPieces                  :: ByteString,
  torrentPieceCount              :: Integer,
  torrentPieceSize               :: Integer,
  -- maybe? what is a tr_info.filecount
  torrentPriorities              :: [Integer],
  torrentQueuePosition           :: Integer,
  torrentRateDownload            :: TransferRate,
  torrentRateUpload              :: TransferRate,
  torrentRecheckProgress         :: Rational,
  torrentSecondsDownloading      :: Integer,
  torrentSecondsSeeding          :: Integer,
  torrentSeedIdleLimit           :: Integer,
  torrentSeedIdleMode            :: Integer,
  torrentSeedRatioLimit          :: Rational,
  torrentSeedRatioMode           :: Integer,
  torrentSizeWhenDone            :: Integer,
  torrentStartDate               :: Integer,
  torrentStatus                  :: Integer,
  torrentTrackers                :: [Tracker],
  torrentTrackerStats            :: [TrackerStat],
  torrentTotalSize               :: Integer,
  torrentTorrentFile             :: Text,
  torrentUploadedEver            :: Integer,
  torrentUploadLimit             :: Integer,
  torrentUploadLimited           :: Bool,
  torrentUploadRatio             :: Rational,
  -- maybe? what is a tr_info.filecount
  torrentWanted                  :: [Integer],
  torrentWebseeds                :: [Webseed],
  torrentWebseedsSendingToUs     :: Integer
} deriving (Show, Eq)

instance FromJSON Torrent where
  parseJSON = withObject "Torrent" $ \v ->
    Torrent <$> v .: "activityDate"
            <*> v .: "addedDate"
            <*> v .: "bandwidthPriority"
            <*> v .: "comment"
            <*> v .: "corruptEver"
            <*> v .: "creator"
            <*> v .: "dateCreated"
            <*> v .: "desiredAvailable"
            <*> v .: "doneDate"
            <*> v .: "downloadDir"
            <*> v .: "downloadedEver"
            <*> v .: "downloadLimit"
            <*> v .: "downloadLimited"
            <*> v .: "error"
            <*> v .: "errorString"
            <*> v .: "eta"
            <*> v .: "etaIdle"
            <*> v .: "files"
            <*> v .: "fileStats"
            <*> v .: "hashString"
            <*> v .: "haveUnchecked"
            <*> v .: "haveValid"
            <*> v .: "honorsSessionLimits"
            <*> v .: "id"
            <*> v .: "isFinished"
            <*> v .: "isPrivate"
            <*> v .: "isStalled"
            <*> v .: "leftUntilDone"
            <*> v .: "magnetLink"
            <*> v .: "manualAnnounceTime"
            <*> v .: "maxConnectedPeers"
            <*> v .: "metadataPercentComplete"
            <*> v .: "name"
            <*> v .: "peer-limit"
            <*> v .: "peers"
            <*> v .: "peersConnected"
            <*> v .: "peersFrom"
            <*> v .: "peersGettingFromUs"
            <*> v .: "peersSendingToUs"
            <*> v .: "percentDone"
            <*> v .: "pieces"
            <*> v .: "pieceCount"
            <*> v .: "pieceSize"
            <*> v .: "priorities"
            <*> v .: "queuePosition"
            <*> v .: "rateDownload"
            <*> v .: "rateUpload (B/s)"
            <*> v .: "recheckProgress"
            <*> v .: "secondsDownloading"
            <*> v .: "secondsSeeding"
            <*> v .: "seedIdleLimit"
            <*> v .: "seedIdleMode"
            <*> v .: "seedRatioLimit"
            <*> v .: "seedRatioMode"
            <*> v .: "sizeWhenDone"
            <*> v .: "startDate"
            <*> v .: "status"
            <*> v .: "trackers"
            <*> v .: "trackerStats"
            <*> v .: "totalSize"
            <*> v .: "torrentFile"
            <*> v .: "uploadedEver"
            <*> v .: "uploadLimit"
            <*> v .: "uploadLimited"
            <*> v .: "uploadRatio"
            <*> v .: "wanted"
            <*> v .: "webseeds"
            <*> v .: "webseedsSendingToUs"

data TransferRate = BytesPerSecond Integer deriving (Show, Eq)

instance FromJSON TransferRate where
  parseJSON = withNumber "TransterRate (B/s)" parseNumber
    where parseNumber (N.I int) = BytesPerSecond <$> pure int
          parseNumber _         = fail "Double not supported"

data RPCMethod = TorrentStart       |
                 TorrentStartNow    |
                 TorrentStop        |
                 TorrentVerify      |
                 TorrentReannounce  |
                 TorrentSet         |
                 TorrentGet         |
                 TorrentAdd         |
                 TorrentRemove      |
                 TorrentSetLocation |
                 SessionSet         |
                 SessionGet         |
                 SessionStats       |
                 BlocklistUpdate    |
                 PortTest           |
                 SessionClose       |
                 QueueMoveTop       |
                 QueueMoveUp        |
                 QueueMoveDown deriving (Show, Eq)

instance ToJSON RPCMethod where
  toJSON TorrentStart       = "torrent-start"
  toJSON TorrentStartNow    = "torrent-start-now"
  toJSON TorrentStop        = "torrent-stop"
  toJSON TorrentVerify      = "torrent-verify"
  toJSON TorrentReannounce  = "torrent-reannounce"
  toJSON TorrentSet         = "torrent-set"
  toJSON TorrentGet         = "torrent-get"
  toJSON TorrentAdd         = "torrent-add"
  toJSON TorrentRemove      = "torrent-remove"
  toJSON TorrentSetLocation = "torrent-set-location"
  toJSON SessionSet         = "session-set"
  toJSON SessionGet         = "session-get"
  toJSON SessionStats       = "session-stats"
  toJSON BlocklistUpdate    = "blocklist-update"
  toJSON PortTest           = "port-test"
  toJSON SessionClose       = "session-close"
  toJSON QueueMoveTop       = "queue-move-top"
  toJSON QueueMoveUp        = "queue-move-up"
  toJSON QueueMoveDown      = "queue-move-down"

data ClientConfiguration = ClientConfiguration {
  transmissionWebUrl :: String,
  transmissionSessionId :: Maybe ByteString
} deriving (Show, Eq)

instance Default ClientConfiguration where
  def = ClientConfiguration { transmissionWebUrl = "http://localhost:9091/transmission",
                              transmissionSessionId = Nothing }

type TransmissionM m a = StateT ClientConfiguration m a

data TorrentId = TorrentIdNumber Text |
                 TorrentSHA1 Text     |
                 RecentlyActive deriving (Show, Eq)

instance ToJSON TorrentId where
  toJSON (TorrentIdNumber txt) = String txt
  toJSON (TorrentSHA1 txt)     = String txt
  toJSON RecentlyActive        = String "recently-active"

newtype TorrentFile = TorrentFile { torrentFile :: Text } deriving (Show, Eq, FromJSON)

data FileStat = FileStat {
  fileStatBytesCompleted :: Integer,
  fileStatWanted         :: Bool,
  fileStatPriority       :: TorrentPriority
} deriving (Show, Eq)

instance FromJSON FileStat where
  parseJSON = withObject "FileStat" $ \v ->
    FileStat <$> v .: "bytesCompleted"
             <*> v .: "wanted"
             <*> v .: "priority"

data TorrentPriority = HighPriority | NormalPriority | LowPriority deriving (Show, Eq)

instance FromJSON TorrentPriority where
  parseJSON = withNumber "TorrentPriority" parsePriority
    where parsePriority (N.D _)  = fail "Double not supported"
          parsePriority (N.I (-1)) = pure LowPriority
          parsePriority (N.I 0)  = pure NormalPriority
          parsePriority (N.I 1)  = pure HighPriority
          parsePriority _        = fail "Priority must be -1, 0, or 1"

data Peer = Peer {
  peerAddress            :: Text, --todo: IP address type?
  peerClientName         :: Text,
  peerClientIsChoked     :: Bool,
  peerClientIsInterested :: Bool,
  peerClientFlagStr      :: Text,
  peerIsDownloadingFrom  :: Bool,
  peerIsEncrypted        :: Bool,
  peerIsIncoming         :: Bool,
  peerIsUploadingTo      :: Bool,
  peerIsUTP              :: Bool,
  peerIsChoked           :: Bool,
  peerIsInterested       :: Bool,
  peerPort               :: Integer, -- TODO: port
  peerProgress           :: Rational,
  peerRateToClient       :: TransferRate,
  peerRateToPeer         :: TransferRate
} deriving (Show, Eq)

instance FromJSON Peer where
  parseJSON = withObject "Peer" $ \v ->
    Peer <$> v .: "address"
         <*> v .: "clientName"
         <*> v .: "clientIsChoked"
         <*> v .: "clientIsInterested"
         <*> v .: "clientFlagStr"
         <*> v .: "isDownloadingFrom"
         <*> v .: "isEncrypted"
         <*> v .: "isIncoming"
         <*> v .: "isUploadingTo"
         <*> v .: "isUTP"
         <*> v .: "isChoked"
         <*> v .: "isInterested"
         <*> v .: "port"
         <*> v .: "progress"
         <*> v .: "rateToClient"
         <*> v .: "rateToPeer"

data PeersFrom = PeersFrom {
  peersFromCache    :: Integer,
  peersFromDht      :: Integer,
  peersFromIncoming :: Integer,
  peersFromLpd      :: Integer,
  peersFromLtep     :: Integer,
  peersFromPex      :: Integer,
  peersFromTracker  :: Integer
} deriving (Show, Eq)

instance FromJSON PeersFrom where
  parseJSON = withObject "PeersFrom" $ \v ->
    PeersFrom <$> v .: "fromCache"
              <*> v .: "fromDht"
              <*> v .: "fromIncoming"
              <*> v .: "fromLpd"
              <*> v .: "fromLtep"
              <*> v .: "fromPex"
              <*> v .: "fromTracker"

data Tracker = Tracker {
  trackerAnnounce :: String,
  trackerId       :: Integer,
  trackerScrape   :: String,
  trackerTier     :: Integer
} deriving (Show, Eq)

instance FromJSON Tracker where
  parseJSON = withObject "Tracker" $ \v ->
    Tracker <$> v.: "trackerAnnounce"
            <*> v.: "trackerId"
            <*> v.: "trackerScrape"
            <*> v.: "trackerTier"

data TrackerStat = TrackerStat {
  trackerStatAnnounce              :: Text,
  trackerStatAnnounceState         :: TrackerState,
  trackerStatDownloadCount         :: Integer,
  trackerStatHasAnnounced          :: Bool,
  trackerStatHasScraped            :: Bool,
  trackerStatHost                  :: Text,
  trackerStatId                    :: Integer,
  trackerStatIsBackup              :: Bool,
  trackerStatLastAnnouncePeerCount :: Integer,
  trackerStatLastAnnounceResult    :: Text,
  trackerStatLastAnnounceStartTime :: Integer,
  trackerStatLastAnnounceSucceeded :: Bool,
  trackerStatLastAnnounceTime      :: Integer,
  trackerStatLastAnnounceTimedOut  :: Bool,
  trackerStatLastScrapeResult      :: String,
  trackerStatLastScrapeStartTime   :: Integer,
  trackerStatLastScrapeSucceeded   :: Bool,
  trackerStatLastScrapeTime        :: Integer,
  trackerStatLastScrapeTimedOut    :: Bool,
  trackerStatLeecherCount          :: Integer,
  trackerStatNextAnnounceTime      :: Integer,
  trackerStatNextScrapeTime        :: Integer,
  trackerStatScrape                :: Text, -- url
  trackerStatScrapeState           :: TrackerState,
  trackerStatSeederCount           :: Integer,
  trackerStatTier                  :: Integer
} deriving (Show, Eq)

instance FromJSON TrackerStat where
  parseJSON = withObject "TrackerStat" $ \v ->
    TrackerStat <$> v .: "announce"
                <*> v .: "announceState"
                <*> v .: "downloadCount"
                <*> v .: "hasAnnounced"
                <*> v .: "hasScraped"
                <*> v .: "host"
                <*> v .: "id"
                <*> v .: "isBackup"
                <*> v .: "lastAnnouncePeerCount"
                <*> v .: "lastAnnounceResult"
                <*> v .: "lastAnnounceStartTime"
                <*> v .: "lastAnnounceSucceeded"
                <*> v .: "lastAnnounceTime"
                <*> v .: "lastAnnounceTimedOut"
                <*> v .: "lastScrapeResult"
                <*> v .: "lastScrapeStartTime"
                <*> v .: "lastScrapeSucceeded"
                <*> v .: "lastScrapeTime"
                <*> v .: "lastScrapeTimedOut"
                <*> v .: "leecherCount"
                <*> v .: "nextAnnounceTime"
                <*> v .: "nextScrapeTime"
                <*> v .: "scrape"
                <*> v .: "scrapeState"
                <*> v .: "seederCount"
                <*> v .: "tier"

data TrackerState = TrackerInactive |
                    TrackerWaiting  |
                    TrackerQueued   |
                    TrackerActive deriving (Show, Eq)

instance FromJSON TrackerState where
  parseJSON = withNumber "TrackerState" parseState
    where parseState (N.D _)  = fail "Double not supported"
          parseState (N.I 0)  = pure TrackerInactive
          parseState (N.I 1)  = pure TrackerWaiting
          parseState (N.I 2)  = pure TrackerQueued
          parseState (N.I 3)  = pure TrackerActive
          parseState _        = fail "Known tracker states are 0-3"

newtype Webseed = Webseed { webseed :: Text } deriving (Show, Eq, FromJSON)

-- helpers
showError v = pure $ RPCError $ "expected Object with arguments key but got " ++ show v
