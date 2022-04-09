{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Locker where


import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)


data TheDatum = TheDatum {

    addressOwner :: PaymentPubKeyHash,
    secret :: Integer
}
PlutusTx.unstableMakeIsData ''TheDatum

newtype TheRedeemer = TheRedeemer {

    secretPass :: Integer
}
PlutusTx.unstableMakeIsData ''TheRedeemer

mkValidator :: TheDatum -> TheRedeemer -> ScriptContext -> Bool
mkValidator dt rd ctx = traceIfFalse "Not correct signature!" signedByOwner &&
                        traceIfFalse "Oops wrong secret phrase." passThePhrase
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy info $ unPaymentPubKeyHash $ addressOwner dt

    passThePhrase :: Bool
    passThePhrase = secretPass rd == secret dt

data Locking
instance Scripts.ValidatorTypes Locking where
    type instance DatumType Locking = TheDatum
    type instance RedeemerType Locking = TheRedeemer

typedValidator :: Scripts.TypedValidator Locking
typedValidator = Scripts.mkTypedValidator @Locking
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TheDatum @TheRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
