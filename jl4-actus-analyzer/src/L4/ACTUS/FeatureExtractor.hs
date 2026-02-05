{-# LANGUAGE StrictData #-}

-- | Extract semantic features from L4 AST for ACTUS classification.
--
-- This module traverses the L4 AST and identifies patterns that indicate
-- specific contract types. Features include:
--
-- * Type declarations (Currency, FXTransaction, etc.)
-- * Deontic rules (MUST, MAY, SHANT patterns)
-- * State transitions (HENCE, LEST clauses)
-- * Temporal constraints (BEFORE, WITHIN)
-- * Party structures (bilateral, multilateral)
module L4.ACTUS.FeatureExtractor (
  -- * Feature Types
  L4Features (..),
  DomainIndicator (..),
  CurrencyIndicatorData (..),
  FXTransactionIndicatorData (..),
  SwapIndicatorData (..),
  OptionIndicatorData (..),
  LoanIndicatorData (..),
  MasterAgreementIndicatorData (..),
  SettlementIndicatorData (..),
  CreditSupportIndicatorData (..),
  DeonticPattern (..),
  StateTransition (..),
  TemporalConstraint (..),
  PartyStructure (..),
  TypePattern (..),
  TypePatternKind (..),

  -- * Re-exported from L4.Syntax
  DeonticModal (..),

  -- * Feature Extraction
  extractFeatures,
  extractFeaturesFromModule,

  -- * Individual Extractors
  extractDomainIndicators,
  extractDeonticPatterns,
  extractStateTransitions,
  extractTemporalConstraints,
  extractPartyStructure,
  extractTypePatterns,
) where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import L4.ACTUS.Ontology.Types (SourceLocation (..))
import L4.Syntax
import Optics (foldMapOf, gplate)

-- | All extracted features from an L4 module.
data L4Features = L4Features
  { domainIndicators :: [DomainIndicator]
  -- ^ Domain-specific type and naming patterns
  , deonticPatterns :: [DeonticPattern]
  -- ^ MUST/MAY/SHANT obligations
  , stateTransitions :: [StateTransition]
  -- ^ HENCE/LEST state machine patterns
  , temporalConstraints :: [TemporalConstraint]
  -- ^ BEFORE/WITHIN temporal logic
  , partyStructure :: PartyStructure
  -- ^ Bilateral vs multilateral
  , typePatterns :: [TypePattern]
  -- ^ Structural patterns in type declarations
  , sourceFile :: FilePath
  -- ^ Original source file
  }
  deriving stock (Eq, Show)

-- | A domain indicator suggests a specific financial domain.
--
-- Note: We use separate data types for each indicator to avoid partial
-- record field warnings with sum types.
data DomainIndicator
  = CurrencyInd CurrencyIndicatorData
  | FXTransactionInd FXTransactionIndicatorData
  | SwapInd SwapIndicatorData
  | OptionInd OptionIndicatorData
  | LoanInd LoanIndicatorData
  | MasterAgreementInd MasterAgreementIndicatorData
  | SettlementInd SettlementIndicatorData
  | CreditSupportInd CreditSupportIndicatorData
  deriving stock (Eq, Show)

-- | Currency indicator data.
data CurrencyIndicatorData = CurrencyIndicatorData
  { ciCurrencyNames :: Set Text
  -- ^ Currency codes found (USD, EUR, etc.)
  , ciLocation :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | FX transaction indicator data.
data FXTransactionIndicatorData = FXTransactionIndicatorData
  { fxHasTwoCurrencies :: Bool
  -- ^ Transaction has bought/sold currencies
  , fxHasValueDate :: Bool
  -- ^ Has value date field
  , fxLocation :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | Swap indicator data.
data SwapIndicatorData = SwapIndicatorData
  { swHasLegs :: Bool
  -- ^ Has leg/payment structures
  , swHasNotional :: Bool
  -- ^ Has notional amount
  , swHasPaymentSchedule :: Bool
  -- ^ Has periodic payments
  , swLocation :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | Option indicator data.
data OptionIndicatorData = OptionIndicatorData
  { optHasStrike :: Bool
  -- ^ Has strike price
  , optHasExpiry :: Bool
  -- ^ Has expiration date
  , optHasUnderlying :: Bool
  -- ^ References underlying asset
  , optLocation :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | Loan indicator data.
data LoanIndicatorData = LoanIndicatorData
  { lnHasPrincipal :: Bool
  , lnHasInterestRate :: Bool
  , lnHasAmortization :: Bool
  , lnLocation :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | Master agreement indicator data.
data MasterAgreementIndicatorData = MasterAgreementIndicatorData
  { maHasSchedule :: Bool
  -- ^ Has schedule/part structure
  , maHasMultipleTransactionTypes :: Bool
  -- ^ Governs multiple transaction types
  , maHasCloseoutNetting :: Bool
  -- ^ Has close-out netting provisions
  , maLocation :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | Settlement indicator data.
data SettlementIndicatorData = SettlementIndicatorData
  { stHasNetting :: Bool
  -- ^ Has netting provisions
  , stHasDelivery :: Bool
  -- ^ Has delivery obligations
  , stLocation :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | Credit support indicator data.
data CreditSupportIndicatorData = CreditSupportIndicatorData
  { csHasGuaranty :: Bool
  , csHasCollateral :: Bool
  , csHasMargin :: Bool
  , csLocation :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | A deontic pattern represents a legal obligation/permission.
data DeonticPattern = DeonticPattern
  { modal :: DeonticModal
  -- ^ MUST, MAY, or SHANT
  , partyRole :: Maybe Text
  -- ^ The party with the obligation (if identifiable)
  , actionType :: Maybe Text
  -- ^ The type of action (e.g., "deliver", "pay")
  , hasConsequence :: Bool
  -- ^ Has HENCE/LEST consequence
  , location :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | A state transition from regulative rules.
data StateTransition = StateTransition
  { fromState :: Maybe Text
  -- ^ State before transition (if identifiable)
  , toState :: Maybe Text
  -- ^ State after transition
  , trigger :: Maybe Text
  -- ^ What triggers the transition
  , isBreach :: Bool
  -- ^ Transition leads to breach
  , location :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | A temporal constraint on obligations.
data TemporalConstraint = TemporalConstraint
  { constraintType :: Text
  -- ^ BEFORE, WITHIN, ON, etc.
  , duration :: Maybe Text
  -- ^ Duration if specified (e.g., "2 days")
  , location :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

-- | Party structure of the contract.
data PartyStructure
  = Bilateral
  | Multilateral Int -- number of parties
  | Unknown
  deriving stock (Eq, Show)

-- | Structural pattern in a type declaration.
data TypePattern = TypePattern
  { patternName :: Text
  -- ^ Name of the type
  , patternKind :: TypePatternKind
  -- ^ What kind of pattern
  , relevantFields :: [Text]
  -- ^ Relevant field names
  , location :: Maybe SourceLocation
  }
  deriving stock (Eq, Show)

data TypePatternKind
  = CurrencyEnumType -- ^ Enum of currency codes
  | AmountRecordType -- ^ Record with value + currency
  | TransactionRecordType -- ^ Transaction with parties, amounts, dates
  | PartyRecordType -- ^ Party/counterparty definition
  | ObligationRecordType -- ^ Obligation/duty definition
  | DateRecordType -- ^ Date with special meaning (value date, settlement date)
  | SettlementRecordType -- ^ Settlement/netting related
  | EventRecordType -- ^ Event/trigger definition
  | GenericRecordType -- ^ Other record type
  | GenericEnumType -- ^ Other enum type
  deriving stock (Eq, Show)

-- | Extract all features from a resolved L4 module.
extractFeatures :: FilePath -> Module Resolved -> L4Features
extractFeatures path mod' =
  L4Features
    { domainIndicators = extractDomainIndicators mod'
    , deonticPatterns = extractDeonticPatterns mod'
    , stateTransitions = extractStateTransitions mod'
    , temporalConstraints = extractTemporalConstraints mod'
    , partyStructure = extractPartyStructure mod'
    , typePatterns = extractTypePatterns mod'
    , sourceFile = path
    }

-- | Extract features from a module (convenience function).
extractFeaturesFromModule :: Module Resolved -> L4Features
extractFeaturesFromModule = extractFeatures ""

-- | Extract domain indicators from type declarations and naming patterns.
extractDomainIndicators :: Module Resolved -> [DomainIndicator]
extractDomainIndicators mod' =
  catMaybes $
    map detectIndicator (collectDeclares mod')
 where
  detectIndicator :: Declare Resolved -> Maybe DomainIndicator
  detectIndicator (MkDeclare _ _ (MkAppForm _ name _ _) typeDecl) =
    let declName = nameText name
     in case typeDecl of
          EnumDecl _ cons ->
            detectEnumIndicator declName cons
          RecordDecl _ _ fields ->
            detectRecordIndicator declName fields
          SynonymDecl _ _ -> Nothing

  detectEnumIndicator :: Text -> [ConDecl Resolved] -> Maybe DomainIndicator
  detectEnumIndicator name cons
    | isCurrencyName name || hasCurrencyConstructors cons =
        Just $
          CurrencyInd
            CurrencyIndicatorData
              { ciCurrencyNames = Set.fromList $ map conName cons
              , ciLocation = Nothing
              }
    | otherwise = Nothing

  detectRecordIndicator :: Text -> [TypedName Resolved] -> Maybe DomainIndicator
  detectRecordIndicator name fields
    | isFXTransactionName name =
        Just $
          FXTransactionInd
            FXTransactionIndicatorData
              { fxHasTwoCurrencies = hasTwoCurrencyFields fields
              , fxHasValueDate = hasFieldNamed ["valueDate", "value_date", "settlement_date", "settlementDate"] fields
              , fxLocation = Nothing
              }
    | isSwapName name =
        Just $
          SwapInd
            SwapIndicatorData
              { swHasLegs = hasFieldNamed ["legs", "payLeg", "receiveLeg", "leg1", "leg2"] fields
              , swHasNotional = hasFieldNamed ["notional", "notionalAmount", "principal"] fields
              , swHasPaymentSchedule = hasFieldNamed ["schedule", "payments", "paymentSchedule"] fields
              , swLocation = Nothing
              }
    | isOptionName name =
        Just $
          OptionInd
            OptionIndicatorData
              { optHasStrike = hasFieldNamed ["strike", "strikePrice", "exercisePrice"] fields
              , optHasExpiry = hasFieldNamed ["expiry", "expiration", "expirationDate", "maturity"] fields
              , optHasUnderlying = hasFieldNamed ["underlying", "underlyingAsset"] fields
              , optLocation = Nothing
              }
    | isLoanName name =
        Just $
          LoanInd
            LoanIndicatorData
              { lnHasPrincipal = hasFieldNamed ["principal", "principalAmount", "loanAmount"] fields
              , lnHasInterestRate = hasFieldNamed ["interestRate", "rate", "couponRate"] fields
              , lnHasAmortization = hasFieldNamed ["amortization", "repayment", "repaymentSchedule"] fields
              , lnLocation = Nothing
              }
    | isMasterAgreementName name =
        Just $
          MasterAgreementInd
            MasterAgreementIndicatorData
              { maHasSchedule = hasFieldNamed ["schedule", "partI", "partII"] fields
              , maHasMultipleTransactionTypes = False -- Would need deeper analysis
              , maHasCloseoutNetting = hasFieldNamed ["closeout", "netting", "closeoutNetting"] fields
              , maLocation = Nothing
              }
    | isSettlementName name =
        Just $
          SettlementInd
            SettlementIndicatorData
              { stHasNetting = hasFieldNamed ["netting", "netAmount", "netObligation"] fields
              , stHasDelivery = hasFieldNamed ["delivery", "settlement", "settlementAmount"] fields
              , stLocation = Nothing
              }
    | isCreditSupportName name =
        Just $
          CreditSupportInd
            CreditSupportIndicatorData
              { csHasGuaranty = hasFieldNamed ["guarantor", "guaranty", "guarantee"] fields
              , csHasCollateral = hasFieldNamed ["collateral", "margin", "creditSupport"] fields
              , csHasMargin = hasFieldNamed ["margin", "marginCall", "variationMargin"] fields
              , csLocation = Nothing
              }
    | otherwise = Nothing

-- | Extract deontic patterns from regulative rules.
extractDeonticPatterns :: Module Resolved -> [DeonticPattern]
extractDeonticPatterns mod' =
  mapMaybe extractFromDeonton (collectDeontons mod')
 where
  extractFromDeonton :: Deonton Resolved -> Maybe DeonticPattern
  extractFromDeonton d =
    Just
      DeonticPattern
        { modal = d.action.modal
        , partyRole = extractPartyRole d.party
        , actionType = extractActionType d.action.action
        , hasConsequence = d.hence /= Nothing || d.lest /= Nothing
        , location = Nothing
        }

  extractPartyRole :: Expr Resolved -> Maybe Text
  extractPartyRole = \case
    App _ name [] -> Just $ nameText name
    Proj _ _ name -> Just $ nameText name
    _ -> Nothing

  extractActionType :: Pattern Resolved -> Maybe Text
  extractActionType = \case
    PatApp _ name _ -> Just $ nameText name
    PatVar _ name -> Just $ nameText name
    _ -> Nothing

-- | Extract state transitions from HENCE/LEST clauses.
extractStateTransitions :: Module Resolved -> [StateTransition]
extractStateTransitions mod' =
  mapMaybe extractTransition (collectDeontons mod')
 where
  extractTransition :: Deonton Resolved -> Maybe StateTransition
  extractTransition d
    | d.hence == Nothing && d.lest == Nothing = Nothing
    | otherwise =
        Just
          StateTransition
            { fromState = Nothing -- Would need flow analysis
            , toState = extractState d.hence
            , trigger = extractActionType d.action.action
            , isBreach = isBreach' d.lest
            , location = Nothing
            }

  extractState :: Maybe (Expr Resolved) -> Maybe Text
  extractState = \case
    Just (App _ name []) -> Just $ nameText name
    Just (Regulative _ _) -> Just "regulative"
    _ -> Nothing

  extractActionType :: Pattern Resolved -> Maybe Text
  extractActionType = \case
    PatApp _ name _ -> Just $ nameText name
    PatVar _ name -> Just $ nameText name
    _ -> Nothing

  isBreach' :: Maybe (Expr Resolved) -> Bool
  isBreach' = \case
    Just (Breach _ _ _) -> True
    _ -> False

-- | Extract temporal constraints.
extractTemporalConstraints :: Module Resolved -> [TemporalConstraint]
extractTemporalConstraints mod' =
  mapMaybe extractFromDeonton (collectDeontons mod')
 where
  extractFromDeonton :: Deonton Resolved -> Maybe TemporalConstraint
  extractFromDeonton d =
    case d.due of
      Nothing -> Nothing
      Just dueExpr ->
        Just
          TemporalConstraint
            { constraintType = "BEFORE"
            , duration = extractDuration dueExpr
            , location = Nothing
            }

  extractDuration :: Expr Resolved -> Maybe Text
  extractDuration = \case
    Lit _ (NumericLit _ n) -> Just $ T.pack $ show n
    App _ name [] -> Just $ nameText name
    _ -> Nothing

-- | Analyze party structure.
extractPartyStructure :: Module Resolved -> PartyStructure
extractPartyStructure mod' =
  let partyTypes = filter isPartyType (collectDeclares mod')
      partyCount = length partyTypes
      fieldParties = countPartyFields mod'
   in case max partyCount fieldParties of
        0 -> Unknown
        1 -> Unknown -- Could be bilateral with one generic party type
        2 -> Bilateral
        n -> Multilateral n
 where
  isPartyType (MkDeclare _ _ (MkAppForm _ name _ _) _) =
    let n = T.toLower (nameText name)
     in "party" `T.isInfixOf` n || "counterpart" `T.isInfixOf` n

  countPartyFields _ =
    2 -- Default assumption for IFEMA-style bilateral

-- | Extract type patterns from declarations.
extractTypePatterns :: Module Resolved -> [TypePattern]
extractTypePatterns mod' =
  map classifyDeclare (collectDeclares mod')
 where
  classifyDeclare :: Declare Resolved -> TypePattern
  classifyDeclare (MkDeclare _ _ (MkAppForm _ name _ _) typeDecl) =
    let declName = nameText name
     in case typeDecl of
          EnumDecl _ cons ->
            TypePattern
              { patternName = declName
              , patternKind = classifyEnumType declName cons
              , relevantFields = map conName cons
              , location = Nothing
              }
          RecordDecl _ _ fields ->
            TypePattern
              { patternName = declName
              , patternKind = classifyRecordType declName fields
              , relevantFields = map fieldName fields
              , location = Nothing
              }
          SynonymDecl _ _ ->
            TypePattern
              { patternName = declName
              , patternKind = GenericRecordType
              , relevantFields = []
              , location = Nothing
              }

  classifyEnumType name cons
    | isCurrencyName name || hasCurrencyConstructors cons = CurrencyEnumType
    | otherwise = GenericEnumType

  classifyRecordType name _fields
    | isFXTransactionName name = TransactionRecordType
    | isPartyName name = PartyRecordType
    | isObligationName name = ObligationRecordType
    | isAmountName name = AmountRecordType
    | isDateName name = DateRecordType
    | isSettlementName name = SettlementRecordType
    | isEventName name = EventRecordType
    | otherwise = GenericRecordType

-- | Collect all Declare nodes from a module.
collectDeclares :: Module Resolved -> [Declare Resolved]
collectDeclares = foldMapOf (gplate @(Declare Resolved)) (: [])

-- | Collect all Deonton nodes from a module.
collectDeontons :: Module Resolved -> [Deonton Resolved]
collectDeontons = foldMapOf (gplate @(Deonton Resolved)) (: [])

-- Helper functions for name detection

nameText :: Resolved -> Text
nameText = rawNameToText . rawName . getActual

conName :: ConDecl Resolved -> Text
conName (MkConDecl _ name _) = nameText name

fieldName :: TypedName Resolved -> Text
fieldName (MkTypedName _ name _) = nameText name

isCurrencyName :: Text -> Bool
isCurrencyName name =
  let lower = T.toLower name
   in lower == "currency" || "currency" `T.isSuffixOf` lower

hasCurrencyConstructors :: [ConDecl Resolved] -> Bool
hasCurrencyConstructors cons =
  let names = Set.fromList $ map (T.toUpper . conName) cons
      currencies = Set.fromList ["USD", "EUR", "GBP", "JPY", "CHF", "CAD", "AUD"]
   in not $ Set.null $ Set.intersection names currencies

isFXTransactionName :: Text -> Bool
isFXTransactionName name =
  let lower = T.toLower name
   in "fxtransaction" `T.isInfixOf` lower
        || "fxoutright" `T.isInfixOf` lower
        || "forexchange" `T.isInfixOf` lower
        || "foreignexchange" `T.isInfixOf` lower

isSwapName :: Text -> Bool
isSwapName name =
  let lower = T.toLower name
   in "swap" `T.isInfixOf` lower

isOptionName :: Text -> Bool
isOptionName name =
  let lower = T.toLower name
   in "option" `T.isInfixOf` lower

isLoanName :: Text -> Bool
isLoanName name =
  let lower = T.toLower name
   in any (`T.isInfixOf` lower)
        [ "loan"
        , "mortgage"
        , "note"          -- promissory note
        , "debt"
        , "borrower"
        , "lender"
        , "creditor"
        , "debtor"
        , "repayment"
        , "installment"
        , "amortiz"       -- amortization/amortize
        , "penalty"       -- late payment penalty
        , "payment"       -- payment types
        ]

isMasterAgreementName :: Text -> Bool
isMasterAgreementName name =
  let lower = T.toLower name
   in "master" `T.isInfixOf` lower || "agreement" `T.isInfixOf` lower

isSettlementName :: Text -> Bool
isSettlementName name =
  let lower = T.toLower name
   in "settlement" `T.isInfixOf` lower || "netting" `T.isInfixOf` lower

isCreditSupportName :: Text -> Bool
isCreditSupportName name =
  let lower = T.toLower name
   in "credit" `T.isInfixOf` lower
        || "collateral" `T.isInfixOf` lower
        || "support" `T.isInfixOf` lower
        || "guaranty" `T.isInfixOf` lower
        || "guarantee" `T.isInfixOf` lower

isPartyName :: Text -> Bool
isPartyName name =
  let lower = T.toLower name
   in "party" `T.isInfixOf` lower || "counterpart" `T.isInfixOf` lower

isObligationName :: Text -> Bool
isObligationName name =
  let lower = T.toLower name
   in "obligation" `T.isInfixOf` lower || "duty" `T.isInfixOf` lower

isAmountName :: Text -> Bool
isAmountName name =
  let lower = T.toLower name
   in lower == "amount" || "amount" `T.isSuffixOf` lower

isDateName :: Text -> Bool
isDateName name =
  let lower = T.toLower name
   in "date" `T.isSuffixOf` lower || "valuedate" `T.isInfixOf` lower

isEventName :: Text -> Bool
isEventName name =
  let lower = T.toLower name
   in "event" `T.isInfixOf` lower || "trigger" `T.isInfixOf` lower

hasTwoCurrencyFields :: [TypedName Resolved] -> Bool
hasTwoCurrencyFields fields =
  let names = map (T.toLower . fieldName) fields
      currencyFields = filter ("currency" `T.isInfixOf`) names
   in length currencyFields >= 2

hasFieldNamed :: [Text] -> [TypedName Resolved] -> Bool
hasFieldNamed patterns fields =
  let names = map (T.toLower . fieldName) fields
   in any (\p -> any (T.toLower p `T.isInfixOf`) names) patterns
