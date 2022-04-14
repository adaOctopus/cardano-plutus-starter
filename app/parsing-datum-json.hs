{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson as Json ( encode )
import Data.ByteString.Lazy qualified as LB
import System.Environment ( getArgs )
import Prelude
import Data.String (fromString)
import Ledger          ( TokenName, CurrencySymbol, PubKeyHash, ValidatorHash, POSIXTime, PaymentPubKeyHash )
import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx


data AuctionDatumStruct = AuctionDatumStruct {
      aNFTowner   :: !PaymentPubKeyHash
    , aDeadline :: !POSIXTime
    , aMinBid   :: !Integer
    , aCurrency :: !CurrencySymbol
    , aToken    :: !TokenName
}

-- Constructs the JSON file for the datum, used as input to --tx-in-datum-file in cardano-cli
main :: IO ()
main = do
  [nftowner',deadline',minbid',nftsym',nftname'] <- getArgs
  let nftowner = fromString nftowner'
      deadline = read deadline' :: POSIXTime
      minbid   = read minbid' :: Integer
      nftsym   = fromString nftsym'
      nftname  = fromString nftname'
      datumstructure   =  AuctionDatumStruct nftowner deadline minbid nftsym nftname
  writeData ("datum-nft.json") datumstructure
  putStrLn "Done"

-- Specifying a filepath and writing the data to the JSON file
-- This is needed to attach the datum in its necessary format
writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

-- Parsing the data to json
-- In order to attach the needed datum as specified in Surehive task
-- The highest bid, the bidder etc
toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
