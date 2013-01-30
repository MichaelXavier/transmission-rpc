module Network.Transmission.RPC.DateConversionSpec (spec) where

import Test.Hspec

import Network.Transmission.RPC.DateConversion

spec :: Spec
spec = describe "bitfieldToDays" $ do
         it "parses Sunday" $
           bitfieldToDays' 1 `shouldBe` [Sunday]
         it "parses Monday" $
           bitfieldToDays' 2 `shouldBe` [Monday]
         it "parses Tuesday" $
           bitfieldToDays' 4 `shouldBe` [Tuesday]
         it "parses Wednesday" $
           bitfieldToDays' 8 `shouldBe` [Wednesday]
         it "parses Thursday" $
           bitfieldToDays' 16 `shouldBe` [Thursday]
         it "parses Friday" $
           bitfieldToDays' 32 `shouldBe` [Friday]
         it "parses Saturday" $
           bitfieldToDays' 64 `shouldBe` [Saturday]
         it "parses weekdays" $
           bitfieldToDays' 62 `shouldBe` [Monday, Tuesday, Wednesday, Thursday, Friday]
         it "parses weekends" $
           bitfieldToDays' 65 `shouldBe` [Sunday, Saturday]
  where bitfieldToDays' :: Integer -> [Day]
        bitfieldToDays' = bitfieldToDays
