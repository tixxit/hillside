module Data.VPTreeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.VPTree

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
    it "is idempotent" $ property $
      \str -> strip str == strip (strip str)
