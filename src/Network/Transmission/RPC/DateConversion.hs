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
           Saturday deriving (Show, Eq, Ord, Ix, Enum)

newtype Days = Days { days :: [Day] } deriving (Show, Eq)

instance FromJSON Days where
  parseJSON = withNumber "Day" parseDays
    where parseDays (N.I n) = Days <$> pure (bitfieldToDays n)
          parseDays _       = fail "Integer bitfield expected"

instance ToJSON Day where
  toJSON = Number . N.I . fromIntegral . fromEnum
