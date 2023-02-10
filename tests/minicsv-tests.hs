module Main (main) where

import MiniCSV
import Data.Foldable (toList)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Csv as Csv
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

main :: IO ()
main = defaultMain $ testGroup "minicsv"
    [ testProperty "roundtrip" $ \table ->
        csvDecodeTable (csvEncodeTable table) === cleanup table

    -- cassava decode
    , testProperty "roundtrip3" $ \table ->
        csvDecodeTable (LT.unpack (LTE.decodeUtf8 (Csv.encode table))) === cleanup table

    -- cassava encode
    , testProperty "roundtrip3" $ \table ->
        fmap toList (Csv.decode Csv.NoHeader (LTE.encodeUtf8 (LT.pack (csvEncodeTable table)))) === Right (cleanup table)
    ]

cleanup :: [[String]] -> [[String]]
cleanup = filter (not . (\row -> null row || row == [""]))
