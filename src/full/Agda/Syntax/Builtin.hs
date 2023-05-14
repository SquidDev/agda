-- | This module defines the names of all builtin and primitives used in Agda.
--
-- See "Agda.TypeChecking.Monad.Builtin"
module Agda.Syntax.Builtin where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import qualified Data.Map as M
import Data.Hashable

import Agda.Syntax.Position

import Agda.Utils.List
import Agda.Utils.Pretty

-- | Either a 'BuiltinId' or 'PrimitiveId', used for some lookups.
data SomeBuiltin
  = BuiltinName !BuiltinId
  | PrimitiveName !PrimitiveId
  deriving (Show, Eq, Ord, Generic)

instance Hashable SomeBuiltin
instance NFData SomeBuiltin

-- | The class of types which can be converted to 'SomeBuiltin'.
class IsBuiltin a where
  -- | Convert this value to a builtin.
  someBuiltin :: a -> SomeBuiltin

  -- | Get the identifier for this builtin, generally used for error messages.
  getBuiltinId :: a -> String

instance IsBuiltin SomeBuiltin where
  someBuiltin = id

  getBuiltinId (BuiltinName x) = getBuiltinId x
  getBuiltinId (PrimitiveName x) = getBuiltinId x

-- * Builtins

-- | A builtin name, defined by the @BUILTIN@ pragma.
data BuiltinId
  = BuiltinNat
  | BuiltinSuc
  | BuiltinZero
  | BuiltinNatPlus
  | BuiltinNatMinus
  | BuiltinNatTimes
  | BuiltinNatDivSucAux
  | BuiltinNatModSucAux
  | BuiltinNatEquals
  | BuiltinNatLess
  | BuiltinWord64
  | BuiltinInteger
  | BuiltinIntegerPos
  | BuiltinIntegerNegSuc
  | BuiltinFloat
  | BuiltinChar
  | BuiltinString
  | BuiltinUnit
  | BuiltinUnitUnit
  | BuiltinSigma
  | BuiltinSigmaCon
  | BuiltinBool
  | BuiltinTrue
  | BuiltinFalse
  | BuiltinList
  | BuiltinNil
  | BuiltinCons
  | BuiltinMaybe
  | BuiltinNothing
  | BuiltinJust
  | BuiltinIO
  | BuiltinId
  | BuiltinReflId
  | BuiltinPath
  | BuiltinPathP
  | BuiltinIntervalUniv
  | BuiltinInterval
  | BuiltinIZero
  | BuiltinIOne
  | BuiltinPartial
  | BuiltinPartialP
  | BuiltinIsOne
  | BuiltinItIsOne
  | BuiltinEquiv
  | BuiltinEquivFun
  | BuiltinEquivProof
  | BuiltinTranspProof
  | BuiltinIsOne1
  | BuiltinIsOne2
  | BuiltinIsOneEmpty
  | BuiltinSub
  | BuiltinSubIn
  | BuiltinSizeUniv
  | BuiltinSize
  | BuiltinSizeLt
  | BuiltinSizeSuc
  | BuiltinSizeInf
  | BuiltinSizeMax
  | BuiltinInf
  | BuiltinSharp
  | BuiltinFlat
  | BuiltinEquality
  | BuiltinRefl
  | BuiltinRewrite
  | BuiltinLevelMax
  | BuiltinLevel
  | BuiltinLevelZero
  | BuiltinLevelSuc
  | BuiltinSet
  | BuiltinProp
  | BuiltinSetOmega
  | BuiltinLevelUniv
  | BuiltinSSetOmega
  | BuiltinStrictSet
  | BuiltinFromNat
  | BuiltinFromNeg
  | BuiltinFromString
  | BuiltinQName
  | BuiltinAgdaSort
  | BuiltinAgdaSortSet
  | BuiltinAgdaSortLit
  | BuiltinAgdaSortProp
  | BuiltinAgdaSortPropLit
  | BuiltinAgdaSortInf
  | BuiltinAgdaSortUnsupported
  | BuiltinHiding
  | BuiltinHidden
  | BuiltinInstance
  | BuiltinVisible
  | BuiltinRelevance
  | BuiltinRelevant
  | BuiltinIrrelevant
  | BuiltinQuantity
  | BuiltinQuantity0
  | BuiltinQuantityω
  | BuiltinModality
  | BuiltinModalityConstructor
  | BuiltinAssoc
  | BuiltinAssocLeft
  | BuiltinAssocRight
  | BuiltinAssocNon
  | BuiltinPrecedence
  | BuiltinPrecRelated
  | BuiltinPrecUnrelated
  | BuiltinFixity
  | BuiltinFixityFixity
  | BuiltinArg
  | BuiltinArgInfo
  | BuiltinArgArgInfo
  | BuiltinArgArg
  | BuiltinAbs
  | BuiltinAbsAbs
  | BuiltinAgdaTerm
  | BuiltinAgdaTermVar
  | BuiltinAgdaTermLam
  | BuiltinAgdaTermExtLam
  | BuiltinAgdaTermDef
  | BuiltinAgdaTermCon
  | BuiltinAgdaTermPi
  | BuiltinAgdaTermSort
  | BuiltinAgdaTermLit
  | BuiltinAgdaTermUnsupported
  | BuiltinAgdaTermMeta
  | BuiltinAgdaErrorPart
  | BuiltinAgdaErrorPartString
  | BuiltinAgdaErrorPartTerm
  | BuiltinAgdaErrorPartPatt
  | BuiltinAgdaErrorPartName
  | BuiltinAgdaLiteral
  | BuiltinAgdaLitNat
  | BuiltinAgdaLitWord64
  | BuiltinAgdaLitFloat
  | BuiltinAgdaLitChar
  | BuiltinAgdaLitString
  | BuiltinAgdaLitQName
  | BuiltinAgdaLitMeta
  | BuiltinAgdaClause
  | BuiltinAgdaClauseClause
  | BuiltinAgdaClauseAbsurd
  | BuiltinAgdaPattern
  | BuiltinAgdaPatVar
  | BuiltinAgdaPatCon
  | BuiltinAgdaPatDot
  | BuiltinAgdaPatLit
  | BuiltinAgdaPatProj
  | BuiltinAgdaPatAbsurd
  | BuiltinAgdaDefinitionFunDef
  | BuiltinAgdaDefinitionDataDef
  | BuiltinAgdaDefinitionRecordDef
  | BuiltinAgdaDefinitionDataConstructor
  | BuiltinAgdaDefinitionPostulate
  | BuiltinAgdaDefinitionPrimitive
  | BuiltinAgdaDefinition
  | BuiltinAgdaMeta
  | BuiltinAgdaTCM
  | BuiltinAgdaTCMReturn
  | BuiltinAgdaTCMBind
  | BuiltinAgdaTCMUnify
  | BuiltinAgdaTCMTypeError
  | BuiltinAgdaTCMInferType
  | BuiltinAgdaTCMCheckType
  | BuiltinAgdaTCMNormalise
  | BuiltinAgdaTCMReduce
  | BuiltinAgdaTCMCatchError
  | BuiltinAgdaTCMGetContext
  | BuiltinAgdaTCMExtendContext
  | BuiltinAgdaTCMInContext
  | BuiltinAgdaTCMFreshName
  | BuiltinAgdaTCMDeclareDef
  | BuiltinAgdaTCMDeclarePostulate
  | BuiltinAgdaTCMDeclareData
  | BuiltinAgdaTCMDefineData
  | BuiltinAgdaTCMDefineFun
  | BuiltinAgdaTCMGetType
  | BuiltinAgdaTCMGetDefinition
  | BuiltinAgdaTCMBlockOnMeta
  | BuiltinAgdaTCMCommit
  | BuiltinAgdaTCMQuoteTerm
  | BuiltinAgdaTCMUnquoteTerm
  | BuiltinAgdaTCMQuoteOmegaTerm
  | BuiltinAgdaTCMIsMacro
  | BuiltinAgdaTCMWithNormalisation
  | BuiltinAgdaTCMWithReconstructed
  | BuiltinAgdaTCMWithExpandLast
  | BuiltinAgdaTCMWithReduceDefs
  | BuiltinAgdaTCMAskNormalisation
  | BuiltinAgdaTCMAskReconstructed
  | BuiltinAgdaTCMAskExpandLast
  | BuiltinAgdaTCMAskReduceDefs
  | BuiltinAgdaTCMFormatErrorParts
  | BuiltinAgdaTCMDebugPrint
  | BuiltinAgdaTCMNoConstraints
  | BuiltinAgdaTCMRunSpeculative
  | BuiltinAgdaTCMExec
  | BuiltinAgdaTCMGetInstances
  | BuiltinAgdaTCMPragmaForeign
  | BuiltinAgdaTCMPragmaCompile
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance NFData BuiltinId

instance Hashable BuiltinId where
  s `hashWithSalt` b = s `hashWithSalt` fromEnum b

instance KillRange BuiltinId where
  killRange = id

instance Pretty BuiltinId where
  pretty = text . getBuiltinId

instance IsBuiltin BuiltinId where
  someBuiltin = BuiltinName

  getBuiltinId = \case
    BuiltinNat                               -> "NATURAL"
    BuiltinSuc                               -> "SUC"
    BuiltinZero                              -> "ZERO"
    BuiltinNatPlus                           -> "NATPLUS"
    BuiltinNatMinus                          -> "NATMINUS"
    BuiltinNatTimes                          -> "NATTIMES"
    BuiltinNatDivSucAux                      -> "NATDIVSUCAUX"
    BuiltinNatModSucAux                      -> "NATMODSUCAUX"
    BuiltinNatEquals                         -> "NATEQUALS"
    BuiltinNatLess                           -> "NATLESS"
    BuiltinWord64                            -> "WORD64"
    BuiltinInteger                           -> "INTEGER"
    BuiltinIntegerPos                        -> "INTEGERPOS"
    BuiltinIntegerNegSuc                     -> "INTEGERNEGSUC"
    BuiltinFloat                             -> "FLOAT"
    BuiltinChar                              -> "CHAR"
    BuiltinString                            -> "STRING"
    BuiltinUnit                              -> "UNIT"
    BuiltinUnitUnit                          -> "UNITUNIT"
    BuiltinSigma                             -> "SIGMA"
    BuiltinSigmaCon                          -> "SIGMACON"
    BuiltinBool                              -> "BOOL"
    BuiltinTrue                              -> "TRUE"
    BuiltinFalse                             -> "FALSE"
    BuiltinList                              -> "LIST"
    BuiltinNil                               -> "NIL"
    BuiltinCons                              -> "CONS"
    BuiltinMaybe                             -> "MAYBE"
    BuiltinNothing                           -> "NOTHING"
    BuiltinJust                              -> "JUST"
    BuiltinIO                                -> "IO"
    BuiltinId                                -> "ID"
    BuiltinReflId                            -> "REFLID"
    BuiltinPath                              -> "PATH"
    BuiltinPathP                             -> "PATHP"
    BuiltinIntervalUniv                      -> "CUBEINTERVALUNIV"
    BuiltinInterval                          -> "INTERVAL"
    BuiltinIZero                             -> "IZERO"
    BuiltinIOne                              -> "IONE"
    BuiltinPartial                           -> "PARTIAL"
    BuiltinPartialP                          -> "PARTIALP"
    BuiltinIsOne                             -> "ISONE"
    BuiltinItIsOne                           -> "ITISONE"
    BuiltinEquiv                             -> "EQUIV"
    BuiltinEquivFun                          -> "EQUIVFUN"
    BuiltinEquivProof                        -> "EQUIVPROOF"
    BuiltinTranspProof                       -> "TRANSPPROOF"
    BuiltinIsOne1                            -> "ISONE1"
    BuiltinIsOne2                            -> "ISONE2"
    BuiltinIsOneEmpty                        -> "ISONEEMPTY"
    BuiltinSub                               -> "SUB"
    BuiltinSubIn                             -> "SUBIN"
    BuiltinSizeUniv                          -> "SIZEUNIV"
    BuiltinSize                              -> "SIZE"
    BuiltinSizeLt                            -> "SIZELT"
    BuiltinSizeSuc                           -> "SIZESUC"
    BuiltinSizeInf                           -> "SIZEINF"
    BuiltinSizeMax                           -> "SIZEMAX"
    BuiltinInf                               -> "INFINITY"
    BuiltinSharp                             -> "SHARP"
    BuiltinFlat                              -> "FLAT"
    BuiltinEquality                          -> "EQUALITY"
    BuiltinRefl                              -> "REFL"
    BuiltinRewrite                           -> "REWRITE"
    BuiltinLevelMax                          -> "LEVELMAX"
    BuiltinLevel                             -> "LEVEL"
    BuiltinLevelZero                         -> "LEVELZERO"
    BuiltinLevelSuc                          -> "LEVELSUC"
    BuiltinSet                               -> "TYPE"
    BuiltinProp                              -> "PROP"
    BuiltinSetOmega                          -> "SETOMEGA"
    BuiltinLevelUniv                         -> "LEVELUNIV"
    BuiltinSSetOmega                         -> "STRICTSETOMEGA"
    BuiltinStrictSet                         -> "STRICTSET"
    BuiltinFromNat                           -> "FROMNAT"
    BuiltinFromNeg                           -> "FROMNEG"
    BuiltinFromString                        -> "FROMSTRING"
    BuiltinQName                             -> "QNAME"
    BuiltinAgdaSort                          -> "AGDASORT"
    BuiltinAgdaSortSet                       -> "AGDASORTSET"
    BuiltinAgdaSortLit                       -> "AGDASORTLIT"
    BuiltinAgdaSortProp                      -> "AGDASORTPROP"
    BuiltinAgdaSortPropLit                   -> "AGDASORTPROPLIT"
    BuiltinAgdaSortInf                       -> "AGDASORTINF"
    BuiltinAgdaSortUnsupported               -> "AGDASORTUNSUPPORTED"
    BuiltinHiding                            -> "HIDING"
    BuiltinHidden                            -> "HIDDEN"
    BuiltinInstance                          -> "INSTANCE"
    BuiltinVisible                           -> "VISIBLE"
    BuiltinRelevance                         -> "RELEVANCE"
    BuiltinRelevant                          -> "RELEVANT"
    BuiltinIrrelevant                        -> "IRRELEVANT"
    BuiltinQuantity                          -> "QUANTITY"
    BuiltinQuantity0                         -> "QUANTITY-0"
    BuiltinQuantityω                         -> "QUANTITY-ω"
    BuiltinModality                          -> "MODALITY"
    BuiltinModalityConstructor               -> "MODALITY-CONSTRUCTOR"
    BuiltinAssoc                             -> "ASSOC"
    BuiltinAssocLeft                         -> "ASSOCLEFT"
    BuiltinAssocRight                        -> "ASSOCRIGHT"
    BuiltinAssocNon                          -> "ASSOCNON"
    BuiltinPrecedence                        -> "PRECEDENCE"
    BuiltinPrecRelated                       -> "PRECRELATED"
    BuiltinPrecUnrelated                     -> "PRECUNRELATED"
    BuiltinFixity                            -> "FIXITY"
    BuiltinFixityFixity                      -> "FIXITYFIXITY"
    BuiltinArg                               -> "ARG"
    BuiltinArgInfo                           -> "ARGINFO"
    BuiltinArgArgInfo                        -> "ARGARGINFO"
    BuiltinArgArg                            -> "ARGARG"
    BuiltinAbs                               -> "ABS"
    BuiltinAbsAbs                            -> "ABSABS"
    BuiltinAgdaTerm                          -> "AGDATERM"
    BuiltinAgdaTermVar                       -> "AGDATERMVAR"
    BuiltinAgdaTermLam                       -> "AGDATERMLAM"
    BuiltinAgdaTermExtLam                    -> "AGDATERMEXTLAM"
    BuiltinAgdaTermDef                       -> "AGDATERMDEF"
    BuiltinAgdaTermCon                       -> "AGDATERMCON"
    BuiltinAgdaTermPi                        -> "AGDATERMPI"
    BuiltinAgdaTermSort                      -> "AGDATERMSORT"
    BuiltinAgdaTermLit                       -> "AGDATERMLIT"
    BuiltinAgdaTermUnsupported               -> "AGDATERMUNSUPPORTED"
    BuiltinAgdaTermMeta                      -> "AGDATERMMETA"
    BuiltinAgdaErrorPart                     -> "AGDAERRORPART"
    BuiltinAgdaErrorPartString               -> "AGDAERRORPARTSTRING"
    BuiltinAgdaErrorPartTerm                 -> "AGDAERRORPARTTERM"
    BuiltinAgdaErrorPartPatt                 -> "AGDAERRORPARTPATT"
    BuiltinAgdaErrorPartName                 -> "AGDAERRORPARTNAME"
    BuiltinAgdaLiteral                       -> "AGDALITERAL"
    BuiltinAgdaLitNat                        -> "AGDALITNAT"
    BuiltinAgdaLitWord64                     -> "AGDALITWORD64"
    BuiltinAgdaLitFloat                      -> "AGDALITFLOAT"
    BuiltinAgdaLitChar                       -> "AGDALITCHAR"
    BuiltinAgdaLitString                     -> "AGDALITSTRING"
    BuiltinAgdaLitQName                      -> "AGDALITQNAME"
    BuiltinAgdaLitMeta                       -> "AGDALITMETA"
    BuiltinAgdaClause                        -> "AGDACLAUSE"
    BuiltinAgdaClauseClause                  -> "AGDACLAUSECLAUSE"
    BuiltinAgdaClauseAbsurd                  -> "AGDACLAUSEABSURD"
    BuiltinAgdaPattern                       -> "AGDAPATTERN"
    BuiltinAgdaPatVar                        -> "AGDAPATVAR"
    BuiltinAgdaPatCon                        -> "AGDAPATCON"
    BuiltinAgdaPatDot                        -> "AGDAPATDOT"
    BuiltinAgdaPatLit                        -> "AGDAPATLIT"
    BuiltinAgdaPatProj                       -> "AGDAPATPROJ"
    BuiltinAgdaPatAbsurd                     -> "AGDAPATABSURD"
    BuiltinAgdaDefinitionFunDef              -> "AGDADEFINITIONFUNDEF"
    BuiltinAgdaDefinitionDataDef             -> "AGDADEFINITIONDATADEF"
    BuiltinAgdaDefinitionRecordDef           -> "AGDADEFINITIONRECORDDEF"
    BuiltinAgdaDefinitionDataConstructor     -> "AGDADEFINITIONDATACONSTRUCTOR"
    BuiltinAgdaDefinitionPostulate           -> "AGDADEFINITIONPOSTULATE"
    BuiltinAgdaDefinitionPrimitive           -> "AGDADEFINITIONPRIMITIVE"
    BuiltinAgdaDefinition                    -> "AGDADEFINITION"
    BuiltinAgdaMeta                          -> "AGDAMETA"
    BuiltinAgdaTCM                           -> "AGDATCM"
    BuiltinAgdaTCMReturn                     -> "AGDATCMRETURN"
    BuiltinAgdaTCMBind                       -> "AGDATCMBIND"
    BuiltinAgdaTCMUnify                      -> "AGDATCMUNIFY"
    BuiltinAgdaTCMTypeError                  -> "AGDATCMTYPEERROR"
    BuiltinAgdaTCMInferType                  -> "AGDATCMINFERTYPE"
    BuiltinAgdaTCMCheckType                  -> "AGDATCMCHECKTYPE"
    BuiltinAgdaTCMNormalise                  -> "AGDATCMNORMALISE"
    BuiltinAgdaTCMReduce                     -> "AGDATCMREDUCE"
    BuiltinAgdaTCMCatchError                 -> "AGDATCMCATCHERROR"
    BuiltinAgdaTCMGetContext                 -> "AGDATCMGETCONTEXT"
    BuiltinAgdaTCMExtendContext              -> "AGDATCMEXTENDCONTEXT"
    BuiltinAgdaTCMInContext                  -> "AGDATCMINCONTEXT"
    BuiltinAgdaTCMFreshName                  -> "AGDATCMFRESHNAME"
    BuiltinAgdaTCMDeclareDef                 -> "AGDATCMDECLAREDEF"
    BuiltinAgdaTCMDeclarePostulate           -> "AGDATCMDECLAREPOSTULATE"
    BuiltinAgdaTCMDeclareData                -> "AGDATCMDECLAREDATA"
    BuiltinAgdaTCMDefineData                 -> "AGDATCMDEFINEDATA"
    BuiltinAgdaTCMDefineFun                  -> "AGDATCMDEFINEFUN"
    BuiltinAgdaTCMGetType                    -> "AGDATCMGETTYPE"
    BuiltinAgdaTCMGetDefinition              -> "AGDATCMGETDEFINITION"
    BuiltinAgdaTCMBlockOnMeta                -> "AGDATCMBLOCKONMETA"
    BuiltinAgdaTCMCommit                     -> "AGDATCMCOMMIT"
    BuiltinAgdaTCMQuoteTerm                  -> "AGDATCMQUOTETERM"
    BuiltinAgdaTCMUnquoteTerm                -> "AGDATCMUNQUOTETERM"
    BuiltinAgdaTCMQuoteOmegaTerm             -> "AGDATCMQUOTEOMEGATERM"
    BuiltinAgdaTCMIsMacro                    -> "AGDATCMISMACRO"
    BuiltinAgdaTCMWithNormalisation          -> "AGDATCMWITHNORMALISATION"
    BuiltinAgdaTCMWithReconstructed          -> "AGDATCMWITHRECONSTRUCTED"
    BuiltinAgdaTCMWithExpandLast             -> "AGDATCMWITHEXPANDLAST"
    BuiltinAgdaTCMWithReduceDefs             -> "AGDATCMWITHREDUCEDEFS"
    BuiltinAgdaTCMAskNormalisation           -> "AGDATCMASKNORMALISATION"
    BuiltinAgdaTCMAskReconstructed           -> "AGDATCMASKRECONSTRUCTED"
    BuiltinAgdaTCMAskExpandLast              -> "AGDATCMASKEXPANDLAST"
    BuiltinAgdaTCMAskReduceDefs              -> "AGDATCMASKREDUCEDEFS"
    BuiltinAgdaTCMFormatErrorParts           -> "AGDATCMFORMATERRORPARTS"
    BuiltinAgdaTCMDebugPrint                 -> "AGDATCMDEBUGPRINT"
    BuiltinAgdaTCMNoConstraints              -> "AGDATCMNOCONSTRAINTS"
    BuiltinAgdaTCMRunSpeculative             -> "AGDATCMRUNSPECULATIVE"
    BuiltinAgdaTCMExec                       -> "AGDATCMEXEC"
    BuiltinAgdaTCMGetInstances               -> "AGDATCMGETINSTANCES"
    BuiltinAgdaTCMPragmaForeign              -> "AGDATCMPRAGMAFOREIGN"
    BuiltinAgdaTCMPragmaCompile              -> "AGDATCMPRAGMACOMPILE"


-- | Builtins that come without a definition in Agda syntax.
--   These are giving names to Agda internal concepts which
--   cannot be assigned an Agda type.
--
--   An example would be a user-defined name for @Set@.
--
--     {-# BUILTIN TYPE Type #-}
--
--   The type of @Type@ would be @Type : Level → Setω@
--   which is not valid Agda.
isBuiltinNoDef :: BuiltinId -> Bool
isBuiltinNoDef = hasElem builtinsNoDef

builtinsNoDef :: [BuiltinId]
builtinsNoDef =
  sizeBuiltins ++
  [ BuiltinIntervalUniv
  , BuiltinId
  , BuiltinReflId
  , BuiltinInterval
  , BuiltinPartial
  , BuiltinPartialP
  , BuiltinIsOne
  , BuiltinSub
  , BuiltinIZero
  , BuiltinIOne
  , BuiltinSet
  , BuiltinProp
  , BuiltinLevelUniv
  , BuiltinSetOmega
  , BuiltinStrictSet
  , BuiltinSSetOmega
  ]

sizeBuiltins :: [BuiltinId]
sizeBuiltins =
  [ BuiltinSizeUniv
  , BuiltinSize
  , BuiltinSizeLt
  , BuiltinSizeSuc
  , BuiltinSizeInf
  , BuiltinSizeMax
  ]

-- | Lookup a builtin by the string used in the @BUILTIN@ pragma.
builtinById :: String -> Maybe BuiltinId
builtinById = flip M.lookup m where
  m = M.fromList [(getBuiltinId x, x) | x <- [(minBound :: BuiltinId)..]]

-- * Primitives

-- | A primitive name, defined by the @primitive@ block.
data PrimitiveId
  -- Cubical
  = PrimConId
  | PrimIdElim
  | PrimIMin
  | PrimIMax
  | PrimINeg
  | PrimPartial
  | PrimPartialP
  | PrimSubOut
  | PrimGlue
  | Prim_glue
  | Prim_unglue
  | Prim_glueU
  | Prim_unglueU
  | PrimFaceForall
  | PrimComp
  | PrimPOr
  | PrimTrans
  | PrimDepIMin
  | PrimIdFace
  | PrimIdPath
  | PrimHComp
  --  Integer
  | PrimShowInteger
  -- Natural
  | PrimNatPlus
  | PrimNatMinus
  | PrimNatTimes
  | PrimNatDivSucAux
  | PrimNatModSucAux
  | PrimNatEquality
  | PrimNatLess
  | PrimShowNat
  -- Word64
  | PrimWord64FromNat
  | PrimWord64ToNat
  | PrimWord64ToNatInjective
  -- Level
  | PrimLevelZero
  | PrimLevelSuc
  | PrimLevelMax
  -- Float
  | PrimFloatEquality
  | PrimFloatInequality
  | PrimFloatLess
  | PrimFloatIsInfinite
  | PrimFloatIsNaN
  | PrimFloatIsNegativeZero
  | PrimFloatIsSafeInteger
  | PrimFloatToWord64
  | PrimFloatToWord64Injective
  | PrimNatToFloat
  | PrimIntToFloat
  | PrimFloatRound
  | PrimFloatFloor
  | PrimFloatCeiling
  | PrimFloatToRatio
  | PrimRatioToFloat
  | PrimFloatDecode
  | PrimFloatEncode
  | PrimShowFloat
  | PrimFloatPlus
  | PrimFloatMinus
  | PrimFloatTimes
  | PrimFloatNegate
  | PrimFloatDiv
  | PrimFloatPow
  | PrimFloatSqrt
  | PrimFloatExp
  | PrimFloatLog
  | PrimFloatSin
  | PrimFloatCos
  | PrimFloatTan
  | PrimFloatASin
  | PrimFloatACos
  | PrimFloatATan
  | PrimFloatATan2
  | PrimFloatSinh
  | PrimFloatCosh
  | PrimFloatTanh
  | PrimFloatASinh
  | PrimFloatACosh
  | PrimFloatATanh
  -- Character
  | PrimCharEquality
  | PrimIsLower
  | PrimIsDigit
  | PrimIsAlpha
  | PrimIsSpace
  | PrimIsAscii
  | PrimIsLatin1
  | PrimIsPrint
  | PrimIsHexDigit
  | PrimToUpper
  | PrimToLower
  | PrimCharToNat
  | PrimCharToNatInjective
  | PrimNatToChar
  | PrimShowChar
  -- String
  | PrimStringToList
  | PrimStringToListInjective
  | PrimStringFromList
  | PrimStringFromListInjective
  | PrimStringAppend
  | PrimStringEquality
  | PrimShowString
  | PrimStringUncons
  -- "Other stuff"
  | PrimErase
  | PrimEraseEquality
  | PrimForce
  | PrimForceLemma
  | PrimQNameEquality
  | PrimQNameLess
  | PrimShowQName
  | PrimQNameFixity
  | PrimQNameToWord64s
  | PrimQNameToWord64sInjective
  | PrimMetaEquality
  | PrimMetaLess
  | PrimShowMeta
  | PrimMetaToNat
  | PrimMetaToNatInjective
  | PrimLockUniv
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance NFData PrimitiveId

instance Hashable PrimitiveId where
  s `hashWithSalt` b = s `hashWithSalt` fromEnum b

instance KillRange PrimitiveId where
  killRange = id

instance Pretty PrimitiveId where
  pretty = text . getBuiltinId

instance IsBuiltin PrimitiveId where
  someBuiltin = PrimitiveName

  getBuiltinId = \case
    -- Cubical
    PrimConId                             -> "primConId"
    PrimIdElim                            -> "primIdElim"
    PrimIMin                              -> "primIMin"
    PrimIMax                              -> "primIMax"
    PrimINeg                              -> "primINeg"
    PrimPartial                           -> "primPartial"
    PrimPartialP                          -> "primPartialP"
    PrimSubOut                            -> "primSubOut"
    PrimGlue                              -> "primGlue"
    Prim_glue                             -> "prim^glue"
    Prim_unglue                           -> "prim^unglue"
    Prim_glueU                            -> "prim^glueU"
    Prim_unglueU                          -> "prim^unglueU"
    PrimFaceForall                        -> "primFaceForall"
    PrimComp                              -> "primComp"
    PrimPOr                               -> "primPOr"
    PrimTrans                             -> "primTransp"
    PrimDepIMin                           -> "primDepIMin"
    PrimIdFace                            -> "primIdFace"
    PrimIdPath                            -> "primIdPath"
    PrimHComp                             -> "primHComp"
    --  Integer
    PrimShowInteger                       -> "primShowInteger"
    -- Natural
    PrimNatPlus                           -> "primNatPlus"
    PrimNatMinus                          -> "primNatMinus"
    PrimNatTimes                          -> "primNatTimes"
    PrimNatDivSucAux                      -> "primNatDivSucAux"
    PrimNatModSucAux                      -> "primNatModSucAux"
    PrimNatEquality                       -> "primNatEquality"
    PrimNatLess                           -> "primNatLess"
    PrimShowNat                           -> "primShowNat"
    -- Word64
    PrimWord64FromNat                     -> "primWord64FromNat"
    PrimWord64ToNat                       -> "primWord64ToNat"
    PrimWord64ToNatInjective              -> "primWord64ToNatInjective"
    -- Level
    PrimLevelZero                         -> "primLevelZero"
    PrimLevelSuc                          -> "primLevelSuc"
    PrimLevelMax                          -> "primLevelMax"
    -- Float
    PrimFloatEquality                     -> "primFloatEquality"
    PrimFloatInequality                   -> "primFloatInequality"
    PrimFloatLess                         -> "primFloatLess"
    PrimFloatIsInfinite                   -> "primFloatIsInfinite"
    PrimFloatIsNaN                        -> "primFloatIsNaN"
    PrimFloatIsNegativeZero               -> "primFloatIsNegativeZero"
    PrimFloatIsSafeInteger                -> "primFloatIsSafeInteger"
    PrimFloatToWord64                     -> "primFloatToWord64"
    PrimFloatToWord64Injective            -> "primFloatToWord64Injective"
    PrimNatToFloat                        -> "primNatToFloat"
    PrimIntToFloat                        -> "primIntToFloat"
    PrimFloatRound                        -> "primFloatRound"
    PrimFloatFloor                        -> "primFloatFloor"
    PrimFloatCeiling                      -> "primFloatCeiling"
    PrimFloatToRatio                      -> "primFloatToRatio"
    PrimRatioToFloat                      -> "primRatioToFloat"
    PrimFloatDecode                       -> "primFloatDecode"
    PrimFloatEncode                       -> "primFloatEncode"
    PrimShowFloat                         -> "primShowFloat"
    PrimFloatPlus                         -> "primFloatPlus"
    PrimFloatMinus                        -> "primFloatMinus"
    PrimFloatTimes                        -> "primFloatTimes"
    PrimFloatNegate                       -> "primFloatNegate"
    PrimFloatDiv                          -> "primFloatDiv"
    PrimFloatPow                          -> "primFloatPow"
    PrimFloatSqrt                         -> "primFloatSqrt"
    PrimFloatExp                          -> "primFloatExp"
    PrimFloatLog                          -> "primFloatLog"
    PrimFloatSin                          -> "primFloatSin"
    PrimFloatCos                          -> "primFloatCos"
    PrimFloatTan                          -> "primFloatTan"
    PrimFloatASin                         -> "primFloatASin"
    PrimFloatACos                         -> "primFloatACos"
    PrimFloatATan                         -> "primFloatATan"
    PrimFloatATan2                        -> "primFloatATan2"
    PrimFloatSinh                         -> "primFloatSinh"
    PrimFloatCosh                         -> "primFloatCosh"
    PrimFloatTanh                         -> "primFloatTanh"
    PrimFloatASinh                        -> "primFloatASinh"
    PrimFloatACosh                        -> "primFloatACosh"
    PrimFloatATanh                        -> "primFloatATanh"
    -- Character
    PrimCharEquality                      -> "primCharEquality"
    PrimIsLower                           -> "primIsLower"
    PrimIsDigit                           -> "primIsDigit"
    PrimIsAlpha                           -> "primIsAlpha"
    PrimIsSpace                           -> "primIsSpace"
    PrimIsAscii                           -> "primIsAscii"
    PrimIsLatin1                          -> "primIsLatin1"
    PrimIsPrint                           -> "primIsPrint"
    PrimIsHexDigit                        -> "primIsHexDigit"
    PrimToUpper                           -> "primToUpper"
    PrimToLower                           -> "primToLower"
    PrimCharToNat                         -> "primCharToNat"
    PrimCharToNatInjective                -> "primCharToNatInjective"
    PrimNatToChar                         -> "primNatToChar"
    PrimShowChar                          -> "primShowChar"
    -- String
    PrimStringToList                      -> "primStringToList"
    PrimStringToListInjective             -> "primStringToListInjective"
    PrimStringFromList                    -> "primStringFromList"
    PrimStringFromListInjective           -> "primStringFromListInjective"
    PrimStringAppend                      -> "primStringAppend"
    PrimStringEquality                    -> "primStringEquality"
    PrimShowString                        -> "primShowString"
    PrimStringUncons                      -> "primStringUncons"
    -- "Other stuff"
    PrimErase                             -> "primErase"
    PrimEraseEquality                     -> "primEraseEquality"
    PrimForce                             -> "primForce"
    PrimForceLemma                        -> "primForceLemma"
    PrimQNameEquality                     -> "primQNameEquality"
    PrimQNameLess                         -> "primQNameLess"
    PrimShowQName                         -> "primShowQName"
    PrimQNameFixity                       -> "primQNameFixity"
    PrimQNameToWord64s                    -> "primQNameToWord64s"
    PrimQNameToWord64sInjective           -> "primQNameToWord64sInjective"
    PrimMetaEquality                      -> "primMetaEquality"
    PrimMetaLess                          -> "primMetaLess"
    PrimShowMeta                          -> "primShowMeta"
    PrimMetaToNat                         -> "primMetaToNat"
    PrimMetaToNatInjective                -> "primMetaToNatInjective"
    PrimLockUniv                          -> "primLockUniv"

-- | Lookup a primitive by its identifier.
primitiveById :: String -> Maybe PrimitiveId
primitiveById = flip M.lookup m where
  m = M.fromList [(getBuiltinId x, x) | x <- [(minBound :: PrimitiveId)..]]
