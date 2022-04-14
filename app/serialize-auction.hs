import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS

import           SurehiveAuction (auctionScriptShortBs, auctionScript)


-- Executable for serializing the Surehive auction script
-- the last two parameters used, are the functions exported from the actual surehive auction script 
main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptnum = if nargs > 0 then read (args!!0) else 42
  let scriptname = if nargs > 1 then args!!1 else  "result.plutus"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusScript scriptnum scriptname auctionScript auctionScriptShortBs


-- actual function the write the plutus script to a file in the disk
writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  -- optional function to calculate fees based on byte budget
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
