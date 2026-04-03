-- | ACTUS-specific matching logic.
--
-- This module provides specialized matchers for ACTUS contract types
-- that require more complex pattern detection.
module L4.ACTUS.Matching.ACTUS (
  -- * ACTUS Type Detection
  detectACTUSType,
  isLikelyFXOutright,
  isLikelySwap,
  isLikelyOption,
  isLikelyLoan,
  isLikelyBond,
  isLikelyFuture,

  -- * FIBO Class Mapping
  inferFIBOClass,
  actusToFIBOMapping,
) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.FeatureExtractor
import L4.ACTUS.Ontology.Types (URI, mkURI)

-- | Detect the most likely ACTUS type from features.
--
-- This uses heuristic analysis when the rule-based approach
-- is inconclusive.
detectACTUSType :: L4Features -> Maybe Text
detectACTUSType features
  | isLikelyFXOutright features = Just "FXOUT"
  | isLikelySwap features = Just "SWAPS"
  | isLikelyOption features = Just "OPTNS"
  | isLikelyLoan features = Just "PAM"
  | isLikelyBond features = Just "PAM"
  | isLikelyFuture features = Just "FUTUR"
  | otherwise = Nothing

-- | Check if features indicate an FX outright transaction.
isLikelyFXOutright :: L4Features -> Bool
isLikelyFXOutright features =
  let hasCurrency = any isCurrencyIndicator features.domainIndicators
      hasFX = any isFXIndicator features.domainIndicators
      hasExchange = any hasExchangePattern features.typePatterns
   in hasCurrency && (hasFX || hasExchange)
 where
  isCurrencyIndicator (CurrencyInd _) = True
  isCurrencyIndicator _ = False

  isFXIndicator (FXTransactionInd _) = True
  isFXIndicator _ = False

  hasExchangePattern p =
    let n = T.toLower p.patternName
     in "exchange" `T.isInfixOf` n || "fx" `T.isInfixOf` n

-- | Check if features indicate a swap.
isLikelySwap :: L4Features -> Bool
isLikelySwap features =
  let hasSwapInd = any isSwapIndicator features.domainIndicators
      hasSwapPattern = any hasSwapTypePattern features.typePatterns
   in hasSwapInd || hasSwapPattern
 where
  isSwapIndicator (SwapInd _) = True
  isSwapIndicator _ = False

  hasSwapTypePattern p =
    "swap" `T.isInfixOf` T.toLower p.patternName

-- | Check if features indicate an option.
isLikelyOption :: L4Features -> Bool
isLikelyOption features =
  let hasOptionInd = any isOptionIndicator features.domainIndicators
      hasOptionPattern = any hasOptionTypePattern features.typePatterns
   in hasOptionInd || hasOptionPattern
 where
  isOptionIndicator (OptionInd _) = True
  isOptionIndicator _ = False

  hasOptionTypePattern p =
    "option" `T.isInfixOf` T.toLower p.patternName

-- | Check if features indicate a loan.
isLikelyLoan :: L4Features -> Bool
isLikelyLoan features =
  let hasLoanInd = any isLoanIndicator features.domainIndicators
      hasLoanPattern = any hasLoanTypePattern features.typePatterns
   in hasLoanInd || hasLoanPattern
 where
  isLoanIndicator (LoanInd _) = True
  isLoanIndicator _ = False

  hasLoanTypePattern p =
    let n = T.toLower p.patternName
     in "loan" `T.isInfixOf` n || "mortgage" `T.isInfixOf` n

-- | Check if features indicate a bond.
isLikelyBond :: L4Features -> Bool
isLikelyBond features =
  let hasBondPattern = any hasBondTypePattern features.typePatterns
   in hasBondPattern
 where
  hasBondTypePattern p =
    let n = T.toLower p.patternName
     in "bond" `T.isInfixOf` n || "coupon" `T.isInfixOf` n

-- | Check if features indicate a future.
isLikelyFuture :: L4Features -> Bool
isLikelyFuture features =
  let hasFuturePattern = any hasFutureTypePattern features.typePatterns
   in hasFuturePattern
 where
  hasFutureTypePattern p =
    "future" `T.isInfixOf` T.toLower p.patternName

-- | Infer the FIBO class from an ACTUS type code.
inferFIBOClass :: Text -> Maybe URI
inferFIBOClass actusType =
  Map.lookup actusType actusToFIBOMapping

-- | Mapping from ACTUS type codes to FIBO class URIs.
--
-- Based on the ACTUS Controlled Vocabulary's refersTo predicates.
actusToFIBOMapping :: Map.Map Text URI
actusToFIBOMapping =
  Map.fromList
    [ ("FXOUT", mkURI "https://spec.edmcouncil.org/fibo/ontology/DER/DerivativesContracts/CurrencyContracts/ForeignExchangeForwardAgreement")
    , ("SWAPS", mkURI "https://spec.edmcouncil.org/fibo/ontology/DER/DerivativesContracts/Swaps/Swap")
    , ("OPTNS", mkURI "https://spec.edmcouncil.org/fibo/ontology/DER/DerivativesContracts/Options/Option")
    , ("PAM", mkURI "https://spec.edmcouncil.org/fibo/ontology/SEC/Debt/DebtInstruments/DebtInstrument")
    , ("ANN", mkURI "https://spec.edmcouncil.org/fibo/ontology/SEC/Debt/DebtInstruments/DebtInstrument")
    , ("LAM", mkURI "https://spec.edmcouncil.org/fibo/ontology/SEC/Debt/DebtInstruments/DebtInstrument")
    , ("NAM", mkURI "https://spec.edmcouncil.org/fibo/ontology/SEC/Debt/DebtInstruments/DebtInstrument")
    , ("FUTUR", mkURI "https://spec.edmcouncil.org/fibo/ontology/FBC/FinancialInstruments/FinancialInstruments/Future")
    , ("STK", mkURI "https://spec.edmcouncil.org/fibo/ontology/SEC/Equities/EquityInstruments/Share")
    , ("CDSWP", mkURI "https://spec.edmcouncil.org/fibo/ontology/DER/CreditDerivatives/CreditDefaultSwaps/CreditDefaultSwap")
    , ("TRSWP", mkURI "https://spec.edmcouncil.org/fibo/ontology/DER/DerivativesContracts/Swaps/TotalReturnSwap")
    , ("CEG", mkURI "https://spec.edmcouncil.org/fibo/ontology/FBC/DebtAndEquities/Debt/CreditEnhancementAgreement")
    , ("CEC", mkURI "https://spec.edmcouncil.org/fibo/ontology/FBC/ProductsAndServices/ClientsAndAccounts/CollateralAccount")
    , ("MasterAgreement", mkURI "https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/MasterAgreement")
    ]
