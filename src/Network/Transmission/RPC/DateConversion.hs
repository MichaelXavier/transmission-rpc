module Network.Transmission.RPC.DateConversion (Day(..),
                                                Days(..),
                                                bitfieldToDays) where

import Control.Applicative ((<$>),
                            pure)
import Data.Aeson (FromJSON(..),
                   ToJSON(..),
                   Value(Number),
                   withNumber)
import Data.Bits
import Data.Ix
import qualified Data.Attoparsec.Number as N

bitfieldToDays :: Bits a => a -> [Day]
bitfieldToDays bitField = [ day | (day, on)<- zip days bitsOn, on == True]
  where days = range (Sunday, Saturday)
        bitsOn = map (testBit bitField) [0..6]

data Day = Sunday    |
           Monday    |
           Tuesday   |
           Wednesday |
           Thursday  |
           Friday    |
           Saturday deriving (Show, Eq, Ord, Ix)

newtype Days = Days { days :: [Day] } deriving (Show, Eq)

instance FromJSON Days where
  parseJSON = withNumber "Day" parseDays
    where parseDays (N.I n) = Days <$> pure (bitfieldToDays n)
          parseDays _       = fail "Integer bitfield expected"

instance ToJSON Day where
  toJSON Sunday    = Number $ N.I 0
  toJSON Monday    = Number $ N.I 1
  toJSON Tuesday   = Number $ N.I 2
  toJSON Wednesday = Number $ N.I 3
  toJSON Thursday  = Number $ N.I 4
  toJSON Friday    = Number $ N.I 5
  toJSON Saturday  = Number $ N.I 6
