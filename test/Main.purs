module Test.Main where

import Data.ArrayBuffer.Builder

import Control.Monad.Writer.Trans (tell)
import Data.ArrayBuffer.Builder.Internal (cons, encodeInt8, execBuilder, length, singleton, singleton_, (<>>))
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.UInt as UInt
import Effect (Effect)
import Prelude (Unit, bind, pure, discard, map, negate, ($), (<$>), (=<<), (<>))
import Test.Assert (assertEqual')

asBytes :: ArrayBuffer -> Effect (Array Int)
asBytes x = do
  x' :: Uint8Array <- AT.whole x
  map UInt.toInt <$> AT.toArray x'

putTest :: String -> Array Int -> PutM Effect Unit -> Effect Unit
putTest label expected put = do
  actual <- asBytes =<< execPut put
  assertEqual' label {actual,expected}

buildTest :: String -> Array Int -> Effect Builder -> Effect Unit
buildTest label expected bldr = do
  actual <- asBytes =<< execBuilder =<< bldr
  assertEqual' label {actual,expected}

main :: Effect Unit
main = do
  putTest "Test 0" [6,7,8] $ do
    putInt8 6
    putInt8 7
    putInt8 8

  putTest "Test 1" [255,254] $ do
    putInt16be (-2)

  putTest "Test 3" [3,0,0,0] $ do
    putInt32le 3

  buildTest "Test 4" [1,2,3,4] $ do
    b1 <- encodeInt8 1
    b2 <- encodeInt8 2
    b3 <- encodeInt8 3
    b4 <- encodeInt8 4
    pure $ singleton b1 <> singleton b2 <> singleton b3 <> singleton b4

  buildTest "Test 5" [1,2,3,4] $ do
    b1 <- encodeInt8 1
    b2 <- encodeInt8 2
    b3 <- encodeInt8 3
    b4 <- encodeInt8 4
    pure $ singleton b1 <>> singleton b2 <>> singleton b3 <>> singleton b4

  buildTest "Test 6" [1,2,3,4] $ do
    b1 <- encodeInt8 1
    b2 <- encodeInt8 2
    b3 <- encodeInt8 3
    b4 <- encodeInt8 4
    pure $ singleton b1 <> (singleton b2 <> singleton b3) <> singleton b4

  buildTest "Test 6" [1,2,3,4,5,6,7,8] $ do
    b1 <- encodeInt8 1
    b2 <- encodeInt8 2
    b3 <- encodeInt8 3
    b4 <- encodeInt8 4
    b5 <- encodeInt8 5
    b6 <- encodeInt8 6
    b7 <- encodeInt8 7
    b8 <- encodeInt8 8
    pure $ singleton b1 <> (singleton b2 <> singleton b3) <> singleton b4
         <> singleton b5 <> (singleton b6 <> singleton b7) <> singleton b8

  buildTest "Test 7" [1,2,3,4,5,6,7,8] $ do
    b1 <- encodeInt8 1
    b2 <- encodeInt8 2
    b3 <- encodeInt8 3
    b4 <- encodeInt8 4
    b5 <- encodeInt8 5
    b6 <- encodeInt8 6
    b7 <- encodeInt8 7
    b8 <- encodeInt8 8
    pure $ ((singleton b1 <> singleton b2) <> (singleton b3 <> singleton b4))
         <> ((singleton b5 <> singleton b6) <> (singleton b7 <> singleton b8))

  buildTest "Test 8" [1,2,3,4] $ do
    b1 <- encodeInt8 1
    b2 <- encodeInt8 2
    b3 <- encodeInt8 3
    b4 <- encodeInt8 4
    pure $ (cons b1 (singleton b2)) <> (cons b3 (singleton b4))

  putTest "Test 9" [1,2,2,3,4] $ do
    x <- subBuilder $ do
      putInt8 3
      putInt8 4
    putInt8 1
    putInt8 2
    putInt8 $ length x
    tell x

  buildTest "Test 10" [1,2,3,4,5,6,7,8] $ do
    b1 <- DV.whole <$> encodeInt8 1
    b2 <- DV.whole <$> encodeInt8 2
    b3 <- DV.whole <$> encodeInt8 3
    b4 <- DV.whole <$> encodeInt8 4
    b5 <- DV.whole <$> encodeInt8 5
    b6 <- DV.whole <$> encodeInt8 6
    b7 <- DV.whole <$> encodeInt8 7
    b8 <- DV.whole <$> encodeInt8 8
    pure $ ((singleton_ b1 <> singleton_ b2) <> (singleton_ b3 <> singleton_ b4))
         <> ((singleton_ b5 <> singleton_ b6) <> (singleton_ b7 <> singleton_ b8))

