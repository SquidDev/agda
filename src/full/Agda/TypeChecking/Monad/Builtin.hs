
module Agda.TypeChecking.Monad.Builtin
  ( module Agda.TypeChecking.Monad.Builtin
  , module Agda.Syntax.Builtin  -- The names are defined here.
  ) where

import qualified Control.Monad.Fail as Fail

import Control.Monad                ( liftM2, void )
import Control.Monad.Except
import Control.Monad.IO.Class       ( MonadIO(..) )
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe
import Control.Monad.Writer

import Data.Function ( on )
import qualified Data.Map as Map
import Data.Set (Set)

import Agda.Syntax.Common
import Agda.Syntax.Position
import Agda.Syntax.Literal
import Agda.Syntax.Builtin
import Agda.Syntax.Internal as I
import Agda.Interaction.Options.Base (PragmaOptions(..))
import Agda.TypeChecking.Monad.Base
-- import Agda.TypeChecking.Functions  -- LEADS TO IMPORT CYCLE
import Agda.TypeChecking.Substitute

import Agda.Utils.Functor
import Agda.Utils.Lens
import Agda.Utils.ListT
import Agda.Utils.Monad
import Agda.Utils.Maybe
import Agda.Utils.Singleton
import Agda.Utils.Tuple

import Agda.Utils.Impossible

class ( Functor m
      , Applicative m
      , Fail.MonadFail m
      ) => HasBuiltins m where
  getBuiltinThing :: SomeBuiltin -> m (Maybe (Builtin PrimFun))

  default getBuiltinThing :: (MonadTrans t, HasBuiltins n, t n ~ m) => SomeBuiltin -> m (Maybe (Builtin PrimFun))
  getBuiltinThing = lift . getBuiltinThing

instance HasBuiltins m => HasBuiltins (ExceptT e m)
instance HasBuiltins m => HasBuiltins (IdentityT m)
instance HasBuiltins m => HasBuiltins (ListT m)
instance HasBuiltins m => HasBuiltins (MaybeT m)
instance HasBuiltins m => HasBuiltins (ReaderT e m)
instance HasBuiltins m => HasBuiltins (StateT s m)
instance (HasBuiltins m, Monoid w) => HasBuiltins (WriterT w m)

deriving instance HasBuiltins m => HasBuiltins (BlockT m)

instance MonadIO m => HasBuiltins (TCMT m) where
  getBuiltinThing b =
    liftM2 (unionMaybeWith unionBuiltin)
      (Map.lookup b <$> useTC stLocalBuiltins)
      (Map.lookup b <$> useTC stImportedBuiltins)


-- | The trivial implementation of 'HasBuiltins', using a constant 'TCState'.
--
-- This may be used instead of 'TCMT'/'ReduceM' where builtins must be accessed
-- in a pure context.
newtype BuiltinAccess a = BuiltinAccess { unBuiltinAccess :: TCState -> a }
  deriving (Functor, Applicative, Monad)

instance Fail.MonadFail BuiltinAccess where
  fail msg = BuiltinAccess $ \_ -> error msg

instance HasBuiltins BuiltinAccess where
  getBuiltinThing b = BuiltinAccess $ \state ->
    unionMaybeWith unionBuiltin
      (Map.lookup b $ state ^. stLocalBuiltins)
      (Map.lookup b $ state ^. stImportedBuiltins)

-- | Run a 'BuiltinAccess' monad.
runBuiltinAccess :: TCState -> BuiltinAccess a -> a
runBuiltinAccess s m = unBuiltinAccess m s


-- If Agda is changed so that the type of a literal can belong to an
-- inductive family (with at least one index), then the implementation
-- of split' in Agda.TypeChecking.Coverage should be changed.

litType
  :: (HasBuiltins m, MonadError TCErr m, MonadTCEnv m, ReadTCState m)
  => Literal -> m Type
litType = \case
  LitNat n    -> do
    _ <- primZero
    when (n > 0) $ void $ primSuc
    el <$> primNat
  LitWord64 _ -> el <$> primWord64
  LitFloat _  -> el <$> primFloat
  LitChar _   -> el <$> primChar
  LitString _ -> el <$> primString
  LitQName _  -> el <$> primQName
  LitMeta _ _ -> el <$> primAgdaMeta
  where
    el t = El (mkType 0) t

setBuiltinThings :: BuiltinThings PrimFun -> TCM ()
setBuiltinThings b = stLocalBuiltins `setTCLens` b

bindBuiltinName :: BuiltinId -> Term -> TCM ()
bindBuiltinName b x = do
  builtin <- getBuiltinThing b'
  case builtin of
    Just (Builtin y) -> typeError $ DuplicateBuiltinBinding b y x
    Just Prim{}      -> typeError $ __IMPOSSIBLE__
    Just BuiltinRewriteRelations{} -> __IMPOSSIBLE__
    Nothing          -> stLocalBuiltins `modifyTCLens` Map.insert b' (Builtin x)
  where b' = BuiltinName b

bindPrimitive :: PrimitiveId -> PrimFun -> TCM ()
bindPrimitive b pf = do
  builtin <- getBuiltinThing b'
  case builtin of
    Just (Builtin _) -> typeError $ NoSuchPrimitiveFunction (getBuiltinId b)
    Just (Prim x)    -> typeError $ (DuplicatePrimitiveBinding b `on` primFunName) x pf
    Just BuiltinRewriteRelations{} -> __IMPOSSIBLE__
    Nothing          -> stLocalBuiltins `modifyTCLens` Map.insert b' (Prim pf)
  where b' = PrimitiveName b

-- | Add one (more) relation symbol to the rewrite relations.
bindBuiltinRewriteRelation :: QName -> TCM ()
bindBuiltinRewriteRelation x =
  stLocalBuiltins `modifyTCLens`
    Map.insertWith unionBuiltin (BuiltinName BuiltinRewrite) (BuiltinRewriteRelations $ singleton x)

-- | Get the currently registered rewrite relation symbols.
getBuiltinRewriteRelations :: HasBuiltins m => m (Maybe (Set QName))
getBuiltinRewriteRelations = fmap rels <$> getBuiltinThing (BuiltinName BuiltinRewrite)
  where
  rels = \case
    BuiltinRewriteRelations xs -> xs
    Prim{}    -> __IMPOSSIBLE__
    Builtin{} -> __IMPOSSIBLE__

getBuiltin :: (HasBuiltins m, MonadTCError m)
           => BuiltinId -> m Term
getBuiltin x =
  fromMaybeM (typeError $ NoBindingForBuiltin x) $ getBuiltin' x

getBuiltin' :: HasBuiltins m => BuiltinId -> m (Maybe Term)
getBuiltin' x = (getBuiltin =<<) <$> getBuiltinThing (BuiltinName x) where
  getBuiltin BuiltinRewriteRelations{} = __IMPOSSIBLE__
  getBuiltin (Builtin t)               = Just $ killRange t
  getBuiltin _                         = Nothing

getPrimitive' :: HasBuiltins m => PrimitiveId -> m (Maybe PrimFun)
getPrimitive' x = (getPrim =<<) <$> getBuiltinThing (PrimitiveName x)
  where
    getPrim (Prim pf) = return pf
    getPrim BuiltinRewriteRelations{} = __IMPOSSIBLE__
    getPrim _         = Nothing

getPrimitive :: (HasBuiltins m, MonadError TCErr m, MonadTCEnv m, ReadTCState m)
             => PrimitiveId -> m PrimFun
getPrimitive x =
  fromMaybeM (typeError . NoSuchPrimitiveFunction $ getBuiltinId x) $ getPrimitive' x

getPrimitiveTerm :: (HasBuiltins m, MonadError TCErr m, MonadTCEnv m, ReadTCState m)
                 => PrimitiveId -> m Term
getPrimitiveTerm x = (`Def` []) . primFunName <$> getPrimitive x

getPrimitiveTerm' :: HasBuiltins m => PrimitiveId -> m (Maybe Term)
getPrimitiveTerm' x = fmap (`Def` []) <$> getPrimitiveName' x

getTerm' :: (HasBuiltins m, IsBuiltin a) => a -> m (Maybe Term)
getTerm' = go . someBuiltin where
  go (BuiltinName x)   = getBuiltin' x
  go (PrimitiveName x) = getPrimitiveTerm' x

getName' :: (HasBuiltins m, IsBuiltin a) => a -> m (Maybe QName)
getName' = go . someBuiltin where
  go (BuiltinName x)   = getBuiltinName' x
  go (PrimitiveName x) = getPrimitiveName' x

-- | @getTerm use name@ looks up @name@ as a primitive or builtin, and
-- throws an error otherwise.
-- The @use@ argument describes how the name is used for the sake of
-- the error message.
getTerm :: (HasBuiltins m, IsBuiltin a) => String -> a -> m Term
getTerm use name = flip fromMaybeM (getTerm' name) $
  return $! throwImpossible (ImpMissingDefinitions [getBuiltinId name] use)


-- | Rewrite a literal to constructor form if possible.
constructorForm :: HasBuiltins m => Term -> m Term
constructorForm v = do
  let pZero = fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinZero
      pSuc  = fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinSuc
  constructorForm' pZero pSuc v

constructorForm' :: Applicative m => m Term -> m Term -> Term -> m Term
constructorForm' pZero pSuc v =
  case v of
    Lit (LitNat n)
      | n == 0    -> pZero
      | n > 0     -> (`apply1` Lit (LitNat $ n - 1)) <$> pSuc
      | otherwise -> pure v
    _ -> pure v

---------------------------------------------------------------------------
-- * The names of built-in things
---------------------------------------------------------------------------

primInteger, primIntegerPos, primIntegerNegSuc,
    primFloat, primChar, primString, primUnit, primUnitUnit, primBool, primTrue, primFalse,
    primSigma,
    primList, primNil, primCons, primIO, primNat, primSuc, primZero, primMaybe, primNothing, primJust,
    primPath, primPathP, primIntervalUniv, primInterval, primIZero, primIOne, primPartial, primPartialP,
    primIMin, primIMax, primINeg,
    primIsOne, primItIsOne, primIsOne1, primIsOne2, primIsOneEmpty,
    primSub, primSubIn, primSubOut,
    primTrans, primHComp,
    primId, primConId, primIdElim,
    primEquiv, primEquivFun, primEquivProof,
    primTranspProof,
    primGlue, prim_glue, prim_unglue,
    prim_glueU, prim_unglueU,
    primFaceForall,
    primNatPlus, primNatMinus, primNatTimes, primNatDivSucAux, primNatModSucAux,
    primNatEquality, primNatLess,
    -- Machine words
    primWord64,
    primSizeUniv, primSize, primSizeLt, primSizeSuc, primSizeInf, primSizeMax,
    primInf, primSharp, primFlat,
    primEquality, primRefl,
    primLevel, primLevelZero, primLevelSuc, primLevelMax,
    primLockUniv,
    primLevelUniv,
    primSet, primProp, primSetOmega, primStrictSet, primSSetOmega,
    primFromNat, primFromNeg, primFromString,
    -- builtins for reflection:
    primQName, primArgInfo, primArgArgInfo, primArg, primArgArg, primAbs, primAbsAbs, primAgdaTerm, primAgdaTermVar,
    primAgdaTermLam, primAgdaTermExtLam, primAgdaTermDef, primAgdaTermCon, primAgdaTermPi,
    primAgdaTermSort, primAgdaTermLit, primAgdaTermUnsupported, primAgdaTermMeta,
    primAgdaErrorPart, primAgdaErrorPartString, primAgdaErrorPartTerm, primAgdaErrorPartPatt, primAgdaErrorPartName,
    primHiding, primHidden, primInstance, primVisible,
    primRelevance, primRelevant, primIrrelevant,
    primQuantity, primQuantity0, primQuantityω,
    primModality, primModalityConstructor,
    primAssoc, primAssocLeft, primAssocRight, primAssocNon,
    primPrecedence, primPrecRelated, primPrecUnrelated,
    primFixity, primFixityFixity,
    primAgdaLiteral, primAgdaLitNat, primAgdaLitWord64, primAgdaLitFloat, primAgdaLitString, primAgdaLitChar, primAgdaLitQName, primAgdaLitMeta,
    primAgdaSort, primAgdaSortSet, primAgdaSortLit, primAgdaSortProp, primAgdaSortPropLit, primAgdaSortInf, primAgdaSortUnsupported,
    primAgdaDefinition, primAgdaDefinitionFunDef, primAgdaDefinitionDataDef, primAgdaDefinitionRecordDef,
    primAgdaDefinitionPostulate, primAgdaDefinitionPrimitive, primAgdaDefinitionDataConstructor,
    primAgdaClause, primAgdaClauseClause, primAgdaClauseAbsurd,
    primAgdaPattern, primAgdaPatCon, primAgdaPatVar, primAgdaPatDot,
    primAgdaPatLit, primAgdaPatProj,
    primAgdaPatAbsurd,
    primAgdaMeta,
    primAgdaTCM, primAgdaTCMReturn, primAgdaTCMBind, primAgdaTCMUnify,
    primAgdaTCMTypeError, primAgdaTCMInferType, primAgdaTCMCheckType,
    primAgdaTCMNormalise, primAgdaTCMReduce,
    primAgdaTCMCatchError, primAgdaTCMGetContext, primAgdaTCMExtendContext, primAgdaTCMInContext,
    primAgdaTCMFreshName, primAgdaTCMDeclareDef, primAgdaTCMDeclarePostulate, primAgdaTCMDeclareData, primAgdaTCMDefineData, primAgdaTCMDefineFun,
    primAgdaTCMGetType, primAgdaTCMGetDefinition,
    primAgdaTCMQuoteTerm, primAgdaTCMUnquoteTerm, primAgdaTCMQuoteOmegaTerm,
    primAgdaTCMBlockOnMeta, primAgdaTCMCommit, primAgdaTCMIsMacro,
    primAgdaTCMFormatErrorParts, primAgdaTCMDebugPrint,
    primAgdaTCMWithNormalisation, primAgdaTCMWithReconstructed,
    primAgdaTCMWithExpandLast, primAgdaTCMWithReduceDefs,
    primAgdaTCMAskNormalisation, primAgdaTCMAskReconstructed,
    primAgdaTCMAskExpandLast, primAgdaTCMAskReduceDefs,
    primAgdaTCMNoConstraints,
    primAgdaTCMRunSpeculative,
    primAgdaTCMExec,
    primAgdaTCMGetInstances,
    primAgdaTCMPragmaForeign,
    primAgdaTCMPragmaCompile
    :: (HasBuiltins m, MonadError TCErr m, MonadTCEnv m, ReadTCState m) => m Term

primInteger                           = getBuiltin BuiltinInteger
primIntegerPos                        = getBuiltin BuiltinIntegerPos
primIntegerNegSuc                     = getBuiltin BuiltinIntegerNegSuc
primFloat                             = getBuiltin BuiltinFloat
primChar                              = getBuiltin BuiltinChar
primString                            = getBuiltin BuiltinString
primBool                              = getBuiltin BuiltinBool
primSigma                             = getBuiltin BuiltinSigma
primUnit                              = getBuiltin BuiltinUnit
primUnitUnit                          = getBuiltin BuiltinUnitUnit
primTrue                              = getBuiltin BuiltinTrue
primFalse                             = getBuiltin BuiltinFalse
primList                              = getBuiltin BuiltinList
primNil                               = getBuiltin BuiltinNil
primCons                              = getBuiltin BuiltinCons
primMaybe                             = getBuiltin BuiltinMaybe
primNothing                           = getBuiltin BuiltinNothing
primJust                              = getBuiltin BuiltinJust
primIO                                = getBuiltin BuiltinIO
primId                                = getBuiltin BuiltinId
primConId                             = getPrimitiveTerm PrimConId
primIdElim                            = getPrimitiveTerm PrimIdElim
primPath                              = getBuiltin BuiltinPath
primPathP                             = getBuiltin BuiltinPathP
primIntervalUniv                      = getBuiltin BuiltinIntervalUniv
primInterval                          = getBuiltin BuiltinInterval
primIZero                             = getBuiltin BuiltinIZero
primIOne                              = getBuiltin BuiltinIOne
primIMin                              = getPrimitiveTerm PrimIMin
primIMax                              = getPrimitiveTerm PrimIMax
primINeg                              = getPrimitiveTerm PrimINeg
primPartial                           = getPrimitiveTerm PrimPartial
primPartialP                          = getPrimitiveTerm PrimPartialP
primIsOne                             = getBuiltin BuiltinIsOne
primItIsOne                           = getBuiltin BuiltinItIsOne
primTrans                             = getPrimitiveTerm PrimTrans
primHComp                             = getPrimitiveTerm PrimHComp
primEquiv                             = getBuiltin BuiltinEquiv
primEquivFun                          = getBuiltin BuiltinEquivFun
primEquivProof                        = getBuiltin BuiltinEquivProof
primTranspProof                       = getBuiltin BuiltinTranspProof
prim_glueU                            = getPrimitiveTerm Prim_glueU
prim_unglueU                          = getPrimitiveTerm Prim_unglueU
primGlue                              = getPrimitiveTerm PrimGlue
prim_glue                             = getPrimitiveTerm Prim_glue
prim_unglue                           = getPrimitiveTerm Prim_unglue
primFaceForall                        = getPrimitiveTerm PrimFaceForall
primIsOne1                            = getBuiltin BuiltinIsOne1
primIsOne2                            = getBuiltin BuiltinIsOne2
primIsOneEmpty                        = getBuiltin BuiltinIsOneEmpty
primSub                               = getBuiltin BuiltinSub
primSubIn                             = getBuiltin BuiltinSubIn
primSubOut                            = getPrimitiveTerm PrimSubOut
primNat                               = getBuiltin BuiltinNat
primSuc                               = getBuiltin BuiltinSuc
primZero                              = getBuiltin BuiltinZero
primNatPlus                           = getBuiltin BuiltinNatPlus
primNatMinus                          = getBuiltin BuiltinNatMinus
primNatTimes                          = getBuiltin BuiltinNatTimes
primNatDivSucAux                      = getBuiltin BuiltinNatDivSucAux
primNatModSucAux                      = getBuiltin BuiltinNatModSucAux
primNatEquality                       = getBuiltin BuiltinNatEquals
primNatLess                           = getBuiltin BuiltinNatLess
primWord64                            = getBuiltin BuiltinWord64
primSizeUniv                          = getBuiltin BuiltinSizeUniv
primSize                              = getBuiltin BuiltinSize
primSizeLt                            = getBuiltin BuiltinSizeLt
primSizeSuc                           = getBuiltin BuiltinSizeSuc
primSizeInf                           = getBuiltin BuiltinSizeInf
primSizeMax                           = getBuiltin BuiltinSizeMax
primInf                               = getBuiltin BuiltinInf
primSharp                             = getBuiltin BuiltinSharp
primFlat                              = getBuiltin BuiltinFlat
primEquality                          = getBuiltin BuiltinEquality
primRefl                              = getBuiltin BuiltinRefl
primLevel                             = getBuiltin BuiltinLevel
primLevelZero                         = getBuiltin BuiltinLevelZero
primLevelSuc                          = getBuiltin BuiltinLevelSuc
primLevelMax                          = getBuiltin BuiltinLevelMax
primSet                               = getBuiltin BuiltinSet
primProp                              = getBuiltin BuiltinProp
primSetOmega                          = getBuiltin BuiltinSetOmega
primLockUniv                          = getPrimitiveTerm PrimLockUniv
primLevelUniv                         = getBuiltin BuiltinLevelUniv
primSSetOmega                         = getBuiltin BuiltinSSetOmega
primStrictSet                         = getBuiltin BuiltinStrictSet
primFromNat                           = getBuiltin BuiltinFromNat
primFromNeg                           = getBuiltin BuiltinFromNeg
primFromString                        = getBuiltin BuiltinFromString
primQName                             = getBuiltin BuiltinQName
primArg                               = getBuiltin BuiltinArg
primArgArg                            = getBuiltin BuiltinArgArg
primAbs                               = getBuiltin BuiltinAbs
primAbsAbs                            = getBuiltin BuiltinAbsAbs
primAgdaSort                          = getBuiltin BuiltinAgdaSort
primHiding                            = getBuiltin BuiltinHiding
primHidden                            = getBuiltin BuiltinHidden
primInstance                          = getBuiltin BuiltinInstance
primVisible                           = getBuiltin BuiltinVisible
primRelevance                         = getBuiltin BuiltinRelevance
primRelevant                          = getBuiltin BuiltinRelevant
primIrrelevant                        = getBuiltin BuiltinIrrelevant
primQuantity                          = getBuiltin BuiltinQuantity
primQuantity0                         = getBuiltin BuiltinQuantity0
primQuantityω                         = getBuiltin BuiltinQuantityω
primModality                          = getBuiltin BuiltinModality
primModalityConstructor               = getBuiltin BuiltinModalityConstructor
primAssoc                             = getBuiltin BuiltinAssoc
primAssocLeft                         = getBuiltin BuiltinAssocLeft
primAssocRight                        = getBuiltin BuiltinAssocRight
primAssocNon                          = getBuiltin BuiltinAssocNon
primPrecedence                        = getBuiltin BuiltinPrecedence
primPrecRelated                       = getBuiltin BuiltinPrecRelated
primPrecUnrelated                     = getBuiltin BuiltinPrecUnrelated
primFixity                            = getBuiltin BuiltinFixity
primFixityFixity                      = getBuiltin BuiltinFixityFixity
primArgInfo                           = getBuiltin BuiltinArgInfo
primArgArgInfo                        = getBuiltin BuiltinArgArgInfo
primAgdaSortSet                       = getBuiltin BuiltinAgdaSortSet
primAgdaSortLit                       = getBuiltin BuiltinAgdaSortLit
primAgdaSortProp                      = getBuiltin BuiltinAgdaSortProp
primAgdaSortPropLit                   = getBuiltin BuiltinAgdaSortPropLit
primAgdaSortInf                       = getBuiltin BuiltinAgdaSortInf
primAgdaSortUnsupported               = getBuiltin BuiltinAgdaSortUnsupported
primAgdaTerm                          = getBuiltin BuiltinAgdaTerm
primAgdaTermVar                       = getBuiltin BuiltinAgdaTermVar
primAgdaTermLam                       = getBuiltin BuiltinAgdaTermLam
primAgdaTermExtLam                    = getBuiltin BuiltinAgdaTermExtLam
primAgdaTermDef                       = getBuiltin BuiltinAgdaTermDef
primAgdaTermCon                       = getBuiltin BuiltinAgdaTermCon
primAgdaTermPi                        = getBuiltin BuiltinAgdaTermPi
primAgdaTermSort                      = getBuiltin BuiltinAgdaTermSort
primAgdaTermLit                       = getBuiltin BuiltinAgdaTermLit
primAgdaTermUnsupported               = getBuiltin BuiltinAgdaTermUnsupported
primAgdaTermMeta                      = getBuiltin BuiltinAgdaTermMeta
primAgdaErrorPart                     = getBuiltin BuiltinAgdaErrorPart
primAgdaErrorPartString               = getBuiltin BuiltinAgdaErrorPartString
primAgdaErrorPartTerm                 = getBuiltin BuiltinAgdaErrorPartTerm
primAgdaErrorPartPatt                 = getBuiltin BuiltinAgdaErrorPartPatt
primAgdaErrorPartName                 = getBuiltin BuiltinAgdaErrorPartName
primAgdaLiteral                       = getBuiltin BuiltinAgdaLiteral
primAgdaLitNat                        = getBuiltin BuiltinAgdaLitNat
primAgdaLitWord64                     = getBuiltin BuiltinAgdaLitWord64
primAgdaLitFloat                      = getBuiltin BuiltinAgdaLitFloat
primAgdaLitChar                       = getBuiltin BuiltinAgdaLitChar
primAgdaLitString                     = getBuiltin BuiltinAgdaLitString
primAgdaLitQName                      = getBuiltin BuiltinAgdaLitQName
primAgdaLitMeta                       = getBuiltin BuiltinAgdaLitMeta
primAgdaPattern                       = getBuiltin BuiltinAgdaPattern
primAgdaPatCon                        = getBuiltin BuiltinAgdaPatCon
primAgdaPatVar                        = getBuiltin BuiltinAgdaPatVar
primAgdaPatDot                        = getBuiltin BuiltinAgdaPatDot
primAgdaPatLit                        = getBuiltin BuiltinAgdaPatLit
primAgdaPatProj                       = getBuiltin BuiltinAgdaPatProj
primAgdaPatAbsurd                     = getBuiltin BuiltinAgdaPatAbsurd
primAgdaClause                        = getBuiltin BuiltinAgdaClause
primAgdaClauseClause                  = getBuiltin BuiltinAgdaClauseClause
primAgdaClauseAbsurd                  = getBuiltin BuiltinAgdaClauseAbsurd
primAgdaDefinitionFunDef              = getBuiltin BuiltinAgdaDefinitionFunDef
primAgdaDefinitionDataDef             = getBuiltin BuiltinAgdaDefinitionDataDef
primAgdaDefinitionRecordDef           = getBuiltin BuiltinAgdaDefinitionRecordDef
primAgdaDefinitionDataConstructor     = getBuiltin BuiltinAgdaDefinitionDataConstructor
primAgdaDefinitionPostulate           = getBuiltin BuiltinAgdaDefinitionPostulate
primAgdaDefinitionPrimitive           = getBuiltin BuiltinAgdaDefinitionPrimitive
primAgdaDefinition                    = getBuiltin BuiltinAgdaDefinition
primAgdaMeta                          = getBuiltin BuiltinAgdaMeta
primAgdaTCM                           = getBuiltin BuiltinAgdaTCM
primAgdaTCMReturn                     = getBuiltin BuiltinAgdaTCMReturn
primAgdaTCMBind                       = getBuiltin BuiltinAgdaTCMBind
primAgdaTCMUnify                      = getBuiltin BuiltinAgdaTCMUnify
primAgdaTCMTypeError                  = getBuiltin BuiltinAgdaTCMTypeError
primAgdaTCMInferType                  = getBuiltin BuiltinAgdaTCMInferType
primAgdaTCMCheckType                  = getBuiltin BuiltinAgdaTCMCheckType
primAgdaTCMNormalise                  = getBuiltin BuiltinAgdaTCMNormalise
primAgdaTCMReduce                     = getBuiltin BuiltinAgdaTCMReduce
primAgdaTCMCatchError                 = getBuiltin BuiltinAgdaTCMCatchError
primAgdaTCMGetContext                 = getBuiltin BuiltinAgdaTCMGetContext
primAgdaTCMExtendContext              = getBuiltin BuiltinAgdaTCMExtendContext
primAgdaTCMInContext                  = getBuiltin BuiltinAgdaTCMInContext
primAgdaTCMFreshName                  = getBuiltin BuiltinAgdaTCMFreshName
primAgdaTCMDeclareDef                 = getBuiltin BuiltinAgdaTCMDeclareDef
primAgdaTCMDeclarePostulate           = getBuiltin BuiltinAgdaTCMDeclarePostulate
primAgdaTCMDeclareData                = getBuiltin BuiltinAgdaTCMDeclareData
primAgdaTCMDefineData                 = getBuiltin BuiltinAgdaTCMDefineData
primAgdaTCMDefineFun                  = getBuiltin BuiltinAgdaTCMDefineFun
primAgdaTCMGetType                    = getBuiltin BuiltinAgdaTCMGetType
primAgdaTCMGetDefinition              = getBuiltin BuiltinAgdaTCMGetDefinition
primAgdaTCMQuoteTerm                  = getBuiltin BuiltinAgdaTCMQuoteTerm
primAgdaTCMQuoteOmegaTerm             = getBuiltin BuiltinAgdaTCMQuoteOmegaTerm
primAgdaTCMUnquoteTerm                = getBuiltin BuiltinAgdaTCMUnquoteTerm
primAgdaTCMBlockOnMeta                = getBuiltin BuiltinAgdaTCMBlockOnMeta
primAgdaTCMCommit                     = getBuiltin BuiltinAgdaTCMCommit
primAgdaTCMIsMacro                    = getBuiltin BuiltinAgdaTCMIsMacro
primAgdaTCMWithNormalisation          = getBuiltin BuiltinAgdaTCMWithNormalisation
primAgdaTCMWithReconstructed          = getBuiltin BuiltinAgdaTCMWithReconstructed
primAgdaTCMWithExpandLast             = getBuiltin BuiltinAgdaTCMWithExpandLast
primAgdaTCMWithReduceDefs             = getBuiltin BuiltinAgdaTCMWithReduceDefs
primAgdaTCMAskNormalisation           = getBuiltin BuiltinAgdaTCMAskNormalisation
primAgdaTCMAskReconstructed           = getBuiltin BuiltinAgdaTCMAskReconstructed
primAgdaTCMAskExpandLast              = getBuiltin BuiltinAgdaTCMAskExpandLast
primAgdaTCMAskReduceDefs              = getBuiltin BuiltinAgdaTCMAskReduceDefs
primAgdaTCMFormatErrorParts           = getBuiltin BuiltinAgdaTCMFormatErrorParts
primAgdaTCMDebugPrint                 = getBuiltin BuiltinAgdaTCMDebugPrint
primAgdaTCMNoConstraints              = getBuiltin BuiltinAgdaTCMNoConstraints
primAgdaTCMRunSpeculative             = getBuiltin BuiltinAgdaTCMRunSpeculative
primAgdaTCMExec                       = getBuiltin BuiltinAgdaTCMExec
primAgdaTCMGetInstances               = getBuiltin BuiltinAgdaTCMGetInstances
primAgdaTCMPragmaForeign              = getBuiltin BuiltinAgdaTCMPragmaForeign
primAgdaTCMPragmaCompile              = getBuiltin BuiltinAgdaTCMPragmaCompile

-- | The coinductive primitives.

data CoinductionKit = CoinductionKit
  { nameOfInf   :: QName
  , nameOfSharp :: QName
  , nameOfFlat  :: QName
  }

-- | Tries to build a 'CoinductionKit'.

coinductionKit' :: TCM CoinductionKit
coinductionKit' = do
  Def inf   _ <- primInf
  Def sharp _ <- primSharp
  Def flat  _ <- primFlat
  return $ CoinductionKit
    { nameOfInf   = inf
    , nameOfSharp = sharp
    , nameOfFlat  = flat
    }

coinductionKit :: TCM (Maybe CoinductionKit)
coinductionKit = tryMaybe coinductionKit'

-- | Sort primitives.

data SortKit = SortKit
  { nameOfSet      :: QName
  , nameOfProp     :: QName
  , nameOfSSet     :: QName
  , nameOfSetOmega :: IsFibrant -> QName
  }

-- | Compute a 'SortKit' in an environment that supports failures.
--
-- When 'optLoadPrimitives' is set to 'False', 'sortKit' is a fallible operation,
-- so for the uses of 'sortKit' in fallible contexts (e.g. 'TCM'),
-- we report a type error rather than exploding.
sortKit :: (HasBuiltins m, MonadTCError m, HasOptions m) => m SortKit
sortKit = do
  Def set      _  <- getBuiltin BuiltinSet
  Def prop     _  <- getBuiltin BuiltinProp
  Def setomega _  <- getBuiltin BuiltinSetOmega
  Def sset     _  <- getBuiltin BuiltinStrictSet
  Def ssetomega _ <- getBuiltin BuiltinSSetOmega
  return $ SortKit
    { nameOfSet      = set
    , nameOfProp     = prop
    , nameOfSSet     = sset
    , nameOfSetOmega = \case
        IsFibrant -> setomega
        IsStrict  -> ssetomega
    }

-- | Compute a 'SortKit' in contexts that do not support failure (e.g.
-- 'Reify'). This should only be used when we are sure that the
-- primitive sorts have been bound, i.e. because it is "after" type
-- checking.
infallibleSortKit :: HasBuiltins m => m SortKit
infallibleSortKit = do
  Def set      _  <- fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinSet
  Def prop     _  <- fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinProp
  Def setomega _  <- fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinSetOmega
  Def sset     _  <- fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinStrictSet
  Def ssetomega _ <- fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinSSetOmega
  return $ SortKit
    { nameOfSet      = set
    , nameOfProp     = prop
    , nameOfSSet     = sset
    , nameOfSetOmega = \case
        IsFibrant -> setomega
        IsStrict  -> ssetomega
    }

------------------------------------------------------------------------
-- * Path equality
------------------------------------------------------------------------

getPrimName :: Term -> QName
getPrimName ty = do
  let lamV (Lam i b)  = mapFst (getHiding i :) $ lamV (unAbs b)
      lamV (Pi _ b)   = lamV (unEl $ unAbs b)
      lamV v          = ([], v)
  case lamV ty of
            (_, Def path _) -> path
            (_, Con nm _ _)   -> conName nm
            (_, Var 0 [Proj _ l]) -> l
            (_, t)          -> __IMPOSSIBLE__

getBuiltinName' :: HasBuiltins m => BuiltinId -> m (Maybe QName)
getBuiltinName' n = fmap getPrimName <$> getBuiltin' n

getPrimitiveName' :: HasBuiltins m => PrimitiveId -> m (Maybe QName)
getPrimitiveName' n = fmap primFunName <$> getPrimitive' n

isPrimitive :: HasBuiltins m => PrimitiveId -> QName -> m Bool
isPrimitive n q = (Just q ==) <$> getPrimitiveName' n

intervalSort :: Sort
intervalSort = IntervalUniv

intervalView' :: HasBuiltins m => m (Term -> IntervalView)
intervalView' = do
  iz <- getBuiltinName' BuiltinIZero
  io <- getBuiltinName' BuiltinIOne
  imax <- getPrimitiveName' PrimIMax
  imin <- getPrimitiveName' PrimIMin
  ineg <- getPrimitiveName' PrimINeg
  return $ \ t ->
    case t of
      Def q es ->
        case es of
          [Apply x,Apply y] | Just q == imin -> IMin x y
          [Apply x,Apply y] | Just q == imax -> IMax x y
          [Apply x]         | Just q == ineg -> INeg x
          _                 -> OTerm t
      Con q _ [] | Just (conName q) == iz -> IZero
                 | Just (conName q) == io -> IOne
      _ -> OTerm t

intervalView :: HasBuiltins m => Term -> m IntervalView
intervalView t = do
  f <- intervalView'
  return (f t)

intervalUnview :: HasBuiltins m => IntervalView -> m Term
intervalUnview t = do
  f <- intervalUnview'
  return (f t)

intervalUnview' :: HasBuiltins m => m (IntervalView -> Term)
intervalUnview' = do
  iz <- fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinIZero -- should it be a type error instead?
  io <- fromMaybe __IMPOSSIBLE__ <$> getBuiltin' BuiltinIOne
  imin <- (`Def` []) . fromMaybe __IMPOSSIBLE__ <$> getPrimitiveName' PrimIMin
  imax <- (`Def` []) . fromMaybe __IMPOSSIBLE__ <$> getPrimitiveName' PrimIMax
  ineg <- (`Def` []) . fromMaybe __IMPOSSIBLE__ <$> getPrimitiveName' PrimINeg
  return $ \ v -> case v of
             IZero -> iz
             IOne  -> io
             IMin x y -> apply imin [x,y]
             IMax x y -> apply imax [x,y]
             INeg x   -> apply ineg [x]
             OTerm t -> t

------------------------------------------------------------------------
-- * Path equality
------------------------------------------------------------------------

-- | Check whether the type is actually an path (lhs ≡ rhs)
--   and extract lhs, rhs, and their type.
--
--   Precondition: type is reduced.

pathView :: HasBuiltins m => Type -> m PathView
pathView t0 = do
  view <- pathView'
  return $ view t0

pathView' :: HasBuiltins m => m (Type -> PathView)
pathView' = do
 mpath  <- getBuiltinName' BuiltinPath
 mpathp <- getBuiltinName' BuiltinPathP
 return $ \ t0@(El s t) ->
  case t of
    Def path' [ Apply level , Apply typ , Apply lhs , Apply rhs ]
      | Just path' == mpath, Just path <- mpathp -> PathType s path level (lam_i <$> typ) lhs rhs
      where lam_i = Lam defaultArgInfo . NoAbs "_"
    Def path' [ Apply level , Apply typ , Apply lhs , Apply rhs ]
      | Just path' == mpathp, Just path <- mpathp -> PathType s path level typ lhs rhs
    _ -> OType t0

-- | Non dependent Path
idViewAsPath :: HasBuiltins m => Type -> m PathView
idViewAsPath t0@(El s t) = do
  mid <- fmap getPrimName <$> getBuiltin' BuiltinId
  mpath <- fmap getPrimName <$> getBuiltin' BuiltinPath
  case mid of
   Just path | isJust mpath -> case t of
    Def path' [ Apply level , Apply typ , Apply lhs , Apply rhs ]
      | path' == path -> return $ PathType s (fromJust mpath) level typ lhs rhs
    _ -> return $ OType t0
   _ -> return $ OType t0

boldPathView :: Type -> PathView
boldPathView t0@(El s t) = do
  case t of
    Def path' [ Apply level , Apply typ , Apply lhs , Apply rhs ]
      -> PathType s path' level typ lhs rhs
    _ -> OType t0

-- | Revert the 'PathView'.
--
--   Postcondition: type is reduced.

pathUnview :: PathView -> Type
pathUnview (OType t) = t
pathUnview (PathType s path l t lhs rhs) =
  El s $ Def path $ map Apply [l, t, lhs, rhs]

------------------------------------------------------------------------
-- * Swan's Id Equality
------------------------------------------------------------------------

-- f x (< phi , p > : Id A x _) = Just (phi,p)
conidView' :: HasBuiltins m => m (Term -> Term -> Maybe (Arg Term,Arg Term))
conidView' = do
  mn <- sequence <$> mapM getName' [someBuiltin BuiltinReflId, someBuiltin PrimConId]
  mio <- getTerm' BuiltinIOne
  let fallback = return $ \ _ _ -> Nothing
  caseMaybe mn fallback $ \ [refl,conid] ->
   caseMaybe mio fallback $ \ io -> return $ \ x t ->
    case t of
      Con h _ [] | conName h == refl -> Just (defaultArg io,defaultArg (Lam defaultArgInfo $ NoAbs "_" $ x))
      Def d es | Just [l,a,x,y,phi,p] <- allApplyElims es, d == conid -> Just (phi, p)
      _ -> Nothing

------------------------------------------------------------------------
-- * Builtin equality
------------------------------------------------------------------------

-- | Get the name of the equality type.
primEqualityName :: TCM QName
-- primEqualityName = getDef =<< primEquality  -- LEADS TO IMPORT CYCLE
primEqualityName = do
  eq <- primEquality
  -- Andreas, 2014-05-17 moved this here from TC.Rules.Def
  -- Don't know why up to 2 hidden lambdas need to be stripped,
  -- but I left the code in place.
  -- Maybe it was intended that equality could be declared
  -- in three different ways:
  -- 1. universe and type polymorphic
  -- 2. type polymorphic only
  -- 3. monomorphic.
  let lamV (Lam i b)  = mapFst (getHiding i :) $ lamV (unAbs b)
      lamV v          = ([], v)
  return $ case lamV eq of
    (_, Def equality _) -> equality
    _                   -> __IMPOSSIBLE__

-- | Check whether the type is actually an equality (lhs ≡ rhs)
--   and extract lhs, rhs, and their type.
--
--   Precondition: type is reduced.

equalityView :: Type -> TCM EqualityView
equalityView t0@(El s t) = do
  equality <- primEqualityName
  case t of
    Def equality' es | equality' == equality -> do
      let vs = fromMaybe __IMPOSSIBLE__ $ allApplyElims es
      let n = length vs
      unless (n >= 3) __IMPOSSIBLE__
      let (pars, [ typ , lhs, rhs ]) = splitAt (n-3) vs
      return $ EqualityType s equality pars typ lhs rhs
    _ -> return $ OtherType t0

-- | Revert the 'EqualityView'.
--
--   Postcondition: type is reduced.

class EqualityUnview a where
  equalityUnview :: a -> Type

instance EqualityUnview EqualityView where
  equalityUnview = \case
    OtherType t -> t
    IdiomType t -> t
    EqualityViewType eqt -> equalityUnview eqt

instance EqualityUnview EqualityTypeData where
  equalityUnview (EqualityTypeData s equality l t lhs rhs) =
    El s $ Def equality $ map Apply (l ++ [t, lhs, rhs])

-- | Primitives with typechecking constrants.
constrainedPrims :: [PrimitiveId]
constrainedPrims =
  [ PrimConId
  , PrimPOr
  , PrimComp
  , PrimHComp
  , PrimTrans
  , Prim_glue
  , Prim_glueU
  ]

getNameOfConstrained :: HasBuiltins m => PrimitiveId -> m (Maybe QName)
getNameOfConstrained s = do
  unless (s `elem` constrainedPrims) __IMPOSSIBLE__
  getName' s
