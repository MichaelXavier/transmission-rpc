{-# LANGUAGE OverloadedStrings #-}
module Network.Transmission.RPC.Types (RPCRequest(..),
                                       RPCMethod(..),
                                       RPCResponse(..),
                                       ClientConfiguration(..),
                                       TorrentGetOptions(..),
                                       Torrent(..),
                                       TransmissionM) where

import Control.Applicative ((<$>),
                            (<|>),
                            pure)
import Control.Monad.Trans.State (StateT)
import Data.Aeson (FromJSON(..),
                   ToJSON(..),
                   Value(..),
                   (.=),
                   (.:),
                   object)
import Data.ByteString (ByteString)
import Data.Default
import Data.Text (Text)

data RPCRequest arguments = RPCRequest RPCMethod arguments deriving (Show, Eq)

instance ToJSON arg => ToJSON (RPCRequest arg) where
  toJSON (RPCRequest rpcMethod arg) = object ["arguments" .= arg,
                                              "method" .= rpcMethod]

data RPCResponse a = RPCSuccess a | RPCError String deriving (Show, Eq)

instance FromJSON a => FromJSON (RPCResponse a) where
  parseJSON (Object v) = RPCSuccess <$> (v .: "arguments")-- <|>
                         --showError v
  --parseJSON v          = showError v

showError v = pure $ RPCError $ "expected Object with arguments key but got " ++ show v
  
data Torrent = Torrent deriving (Show, Eq)

instance FromJSON Torrent where
  parseJSON v = error $ show v

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
                 TorrentSha1 Text     |
                 RecentlyActive deriving (Show, Eq)

data TorrentField = ActivityDateF            |
                    AddedDateF               |
                    BandwidthPriorityF       |
                    CommentF                 |
                    CorruptEverF             |
                    CreatorF                 |
                    DateCreatedF             |
                    DesiredAvailableF        |
                    DoneDateF                |
                    DownloadDirF             |
                    DownloadedEverF          |
                    DownloadLimitF           |
                    DownloadLimitedF         |
                    ErrorF                   |
                    ErrorStringF             |
                    EtaF                     |
                    EtaIdleF                 |
                    FilesF                   |
                    FileStatsF               |
                    HashStringF              |
                    HaveUncheckedF           |
                    HaveValidF               |
                    HonorsSessionLimitsF     |
                    IdF                      |
                    IsFinishedF              |
                    IsPrivateF               |
                    IsStalledF               |
                    LeftUntilDoneF           |
                    MagnetLinkF              |
                    ManualAnnounceTimeF      |
                    MaxConnectedPeersF       |
                    MetadataPercentCompleteF |
                    NameF                    |
                    PeerF                    |
                    PeersF                   |
                    PeersConnectedF          |
                    PeersFromF               |
                    PeersGettingFromUsF      |
                    PeersSendingToUsF        |
                    PercentDoneF             |
                    PiecesF                  |
                    PieceCountF              |
                    PieceSizeF               |
                    PrioritiesF              |
                    QueuePositionF           |
                    RateDownloadF            |
                    RateUploadF              |
                    RecheckProgressF         |
                    SecondsDownloadingF      |
                    SecondsSeedingF          |
                    SeedIdleLimitF           |
                    SeedIdleModeF            |
                    SeedRatioLimitF          |
                    SeedRatioModeF           |
                    SizeWhenDoneF            |
                    StartDateF               |
                    StatusF                  |
                    TrackersF                |
                    TrackerStatsF            |
                    TotalSizeF               |
                    TorrentFileF             |
                    UploadedEverF            |
                    UploadLimitF             |
                    UploadLimitedF           |
                    UploadRatioF             |
                    WantedF                  |
                    WebseedsF                |
                    WebseedsSendingToUsF deriving (Show, Eq)

data TorrentGetOptions = TorrentGetOptions { torrentGetIds :: [TorrentId],
                                             torrentGetFields :: [TorrentField] } deriving (Show, Eq)

instance Default TorrentGetOptions where
  def = TorrentGetOptions { torrentGetIds    = [],
                            torrentGetFields = []}

--TODO
instance ToJSON TorrentGetOptions where
  toJSON _ = object []
  
