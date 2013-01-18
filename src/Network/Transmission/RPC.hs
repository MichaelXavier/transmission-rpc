{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Transmission.RPC where

import Control.Failure (Failure)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (asks)
import Data.Aeson (FromJSON(..),
                   fromJSON,
                   Result(..),
                   ToJSON(..),
                   encode,
                   json)
import Data.Conduit (($$+-),
                     MonadUnsafeIO,
                     MonadThrow)
import Data.Conduit.Attoparsec (sinkParser)
import Network.Transmission.RPC.Types

import Network.HTTP.Conduit (parseUrl,
                             RequestBody(..),
                             Request(..),
                             HttpException,
                             withManager,
                             Response(..),
                             http)

torrentStart :: a
torrentStart = undefined

torrentStartNow :: a
torrentStartNow = undefined

torrentStop :: a
torrentStop = undefined

torrentVerify :: a
torrentVerify = undefined

torrentReannounce :: a
torrentReannounce = undefined

torrentSet :: a
torrentSet = undefined

--TODO: handle result properly
--TODO: decide return type
torrentGet :: TorrentGetOptions -> TransmissionM IO Bool
torrentGet opts = do Success result <- makeRequest $ RPCRequest TorrentGet opts
                     return result
                     

torrentAdd :: a
torrentAdd = undefined

torrentRemove :: a
torrentRemove = undefined

torrentSetLocation :: a
torrentSetLocation = undefined

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
makeRequest :: (FromJSON a,
                ToJSON arg,
                MonadIO m,
                MonadThrow m,
                MonadUnsafeIO m,
                MonadBaseControl IO m,
                Failure HttpException m) =>
     RPCRequest arg -> TransmissionM m (Result a)
makeRequest rpcReq = do
  base <- asks transmissionWebUrl
  req  <- parseUrl $ adjustPath base
  -- TODO: send body
  withManager $ \manager -> do
    Response _ _ _ body <- http (prepareRequest req) manager
    body $$+- sinkParser (fmap fromJSON json)
  where prepareRequest req = req { method = "POST",
                                   requestBody = generateBody rpcReq}

adjustPath :: String -> String
adjustPath = (++ "/rpc")

generateBody :: ToJSON arg => RPCRequest arg -> RequestBody m
generateBody = RequestBodyLBS . encode
