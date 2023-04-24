{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NamedFieldPuns #-}


module Contract where

import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import Plutus.V1.Ledger.Value (assetClassValueOf, Value, valueOf, AssetClass (AssetClass))
import Plutus.V2.Ledger.Api
    ( FromData(fromBuiltinData),
      adaSymbol,
      adaToken,
      CurrencySymbol,
      TokenName,
      mkValidatorScript,
      unValidatorScript,
      PubKeyHash (PubKeyHash),
      Validator,
      ScriptContext(scriptContextTxInfo, ScriptContext),
      TxInfo, Address (Address), UnsafeFromData (unsafeFromBuiltinData) ) 

import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), unPaymentPubKeyHash, toPubKeyHash, scriptHashAddress)
import Plutus.V2.Ledger.Contexts ( valuePaidTo, txSignedBy, findOwnInput, TxOut(..), TxInfo(..), txInInfoResolved, ownCurrencySymbol, ownHash)
import GHC.Show (Show)
import GHC.Generics
import Ledger.Ada as Ada
import PlutusTx


data SplitData =
    SplitData
        { recipient1 :: PaymentPubKeyHash 
        , recipient2 :: PaymentPubKeyHash 
        }
    deriving (Show, Generic)

PlutusTx.makeIsDataIndexed 'SplitData [('SplitData, 0)]
PlutusTx.makeLift ''SplitData

{-# INLINABLE mkValidator #-}
mkValidator :: SplitData -> () -> () -> ScriptContext -> Bool
mkValidator sd _ _ ctx =  
                            let 
                            info :: TxInfo
                            info = scriptContextTxInfo ctx  

                            split1 = valueOf (valuePaidTo info (unPaymentPubKeyHash $ recipient1 sd)) Ada.adaSymbol Ada.adaToken

                            split2 = valueOf (valuePaidTo info (unPaymentPubKeyHash $ recipient2 sd)) Ada.adaSymbol Ada.adaToken

                            inputs =  map txInInfoResolved $ txInfoInputs info
                            
                            scriptValue = foldr (\x acc -> 
                                    if txOutAddress x == scriptHashAddress (ownHash ctx) then acc + valueOf (txOutValue x) Ada.adaSymbol Ada.adaToken else acc ) 0 inputs

                            checkSplit :: Bool
                            checkSplit = split1 == split2 && scriptValue >= split1 + split2                
                        in   
                            traceIfFalse "Not Splitted" checkSplit 


{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator ::  SplitData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator  sd _ _ c = check $ mkValidator sd () () (unsafeFromBuiltinData c)

validator':: SplitData -> Validator
validator' sd = mkValidatorScript $ $$(compile [|| mkWrappedValidator ||]) `applyCode` liftCode sd

validator :: Validator
validator = validator' $ SplitData (PaymentPubKeyHash "7a1513f53986fe8f52ca8addd48a390edb6cdf417d7b722c226e9588") (PaymentPubKeyHash "7e36a4c8da9c6ffaa842d69432c3008b76dd7a338111c55abe958dcc") 
-- 7a1513f53986fe8f52ca8addd48a390edb6cdf417d7b722c226e9588 = addr_test1qpap2yl48xr0ar6je29dm4y28y8dkmxlg97hku3vyfhftzprkwpajej8f9xuarfzz4uhgez0j6xd8jqf8xwg2lz2mewqvurvgd
-- 7e36a4c8da9c6ffaa842d69432c3008b76dd7a338111c55abe958dcc = addr_test1qplrdfxgm2wxl74ggttfgvkrqz9hdht6xwq3r326h62cmn989k5m8uprgt299v2080aga7uqqkutycf97stwtskk40nskerus4
-- Contract Address : addr_test1wp34y3rwkf5wgfc32wcwguvlr3wzr9fhj5hw6sk0mul9k5q4pr369
