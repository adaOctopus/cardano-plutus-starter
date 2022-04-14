{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Week06.Trace
    ( testToken
    ) where

import           Control.Monad              hiding (fmap)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO)
import           Wallet.Emulator.Wallet

import           SurehiveAuction.hs

testToken :: IO ()
testToken = runEmulatorTraceIO auctionSimulation

auctionSimulation :: EmulatorTrace ()
tokenTrace = do
    let w1 = knownWallet 1
    let starting = {
                    adeadline = "Given SLot time",
                    aminBid   = 10_000_000,
                    aCurren   = "66",
                    aNFTname  = "T"
                      }
    h1 <- activateContractWallet w1 startEndpoint
    h2 <- activateContractWallet w2
    h3 <- activateContractWallet w3
    callEndpoint @"start" h1 starting
    void $ Emulator.waitNSlots 1

    callEndpoint @"bid" h2 BidParams {
                bidOwner = mockWalletAddress $ knownWallet 2,
                bid      = 15_000_00   
                }
   void $ 

