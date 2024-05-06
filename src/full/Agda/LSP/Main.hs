{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Agda.LSP.Main (runAgdaLSP) where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.Text.Utf16.Rope.Mixed as Rope
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text.Lines as TextLines
import qualified Data.Text.Lazy as TL
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Strict.Tuple (Pair(..))
import Data.Aeson.Types as Aeson
import Data.Foldable
import Data.Default
import Data.Proxy
import Data.IORef
import Data.Coerce
import Data.Maybe
import Data.List (find, sortOn, intercalate)

import GHC.Generics

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Lens
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText, rangeLinesFromVfs, VirtualFile, file_text)

import System.Exit
import System.IO

import qualified Text.PrettyPrint.Annotated.HughesPJ as Ppr
import qualified Text.PrettyPrint.Annotated as Ppr

import qualified Agda.Syntax.Concrete.Name as C
import qualified Agda.Syntax.Position as Agda

import Agda.Syntax.Translation.InternalToAbstract (Reify(reify))
import Agda.Syntax.Translation.AbstractToConcrete
import Agda.Syntax.Abstract.Pretty
import Agda.Syntax.Concrete.Pretty () -- Instances only
import Agda.Syntax.Scope.Base
import Agda.Syntax.Concrete (Module(modDecls))
import Agda.Syntax.Position
import Agda.Syntax.Internal

import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Telescope
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Monad as I

import Agda.LSP.Translation
import Agda.LSP.Position

import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Highlighting.Precise
import Agda.Interaction.Highlighting.Common (toAtoms)
import Agda.Interaction.Response.Base
import Agda.Interaction.FindFile (SourceFile(SourceFile))
import Agda.Interaction.BasicOps (getWarningsAndNonFatalErrors)
import Agda.Interaction.JSONTop () -- Instances only

import qualified Agda.Utils.RangeMap as RangeMap
import qualified Agda.Utils.BiMap as BiMap

import Agda.Utils.Impossible
import Agda.Utils.FileName
import Agda.Utils.Either
import Agda.Utils.Lens
import Agda.Syntax.Parser (runPMIO)
import Agda.Mimer.Mimer as Mimer
import qualified Agda.Syntax.Parser as P
import qualified Agda.Utils.Maybe.Strict as Strict
import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Utils.RangeMap (RangeMap)
import Agda.Syntax.Parser.Tokens (Token(TokSymbol), Symbol (SymQuestionMark))
import Agda.LSP.Commands
import Agda.Syntax.Common (InteractionId)

data LspConfig = LspConfig
  { lspHighlightingLevel :: HighlightingLevel
  }
  deriving (Show, Generic)

instance FromJSON HighlightingLevel
instance FromJSON LspConfig

initLspConfig :: LspConfig
initLspConfig = LspConfig
  { lspHighlightingLevel = NonInteractive
  }

data Worker = Worker
  { workerUri  :: !NormalizedUri
    -- ^ URI managed by this worker.

  , workerFilePath :: FilePath
    -- ^ File path managed by this worker.

  , workerLoadedState :: !(MVar (IORef TCState))
    -- ^ Mutable variable for the “main” TC state in this worker.

  , workerThread      :: ThreadId
    -- ^ Thread for this worker.

  , workerTasks       :: Chan (Worker -> TCM ())

  , workerContext     :: LanguageContextEnv LspConfig

  , workerPosDelta    :: !(MVar PosDelta)
  }

data LspState = LspState
  { lspStateConfig  :: LanguageContextEnv LspConfig
  , lspStateWorkers :: MVar (HashMap NormalizedUri Worker)
  , lspStateSetup   :: TCM ()
  }

newtype WorkerM a = WorkerM { unWorkerM :: ReaderT LspState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader LspState)

instance MonadLsp LspConfig WorkerM where
  getLspEnv = asks lspStateConfig

syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose         = Just True
  , _change            = Just TextDocumentSyncKind_Incremental
  , _willSave          = Just False
  , _willSaveWaitUntil = Just False
  , _save              = Just (InR (SaveOptions (Just False)))
  }

runAgdaLSP :: TCM () -> TCM ()
runAgdaLSP setup = do
  liftIO do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering

  exc <- liftIO $ runServer ServerDefinition
    { defaultConfig    = initLspConfig
    , configSection    = "agda"
    , parseConfig      = \_ -> mapLeft Text.pack . parseEither parseJSON
    , onConfigChange   = const (pure ())
    , doInitialize     = lspInit setup
    , staticHandlers   = lspHandlers
    , interpretHandler = \state -> Iso (flip runReaderT state . unWorkerM) liftIO
    , options          = def
      { optTextDocumentSync = Just syncOptions
      , optDocumentOnTypeFormattingTriggerCharacters = Just ('?' :| [])
      , optExecuteCommandCommands = Just (map commandName [minBound..maxBound])
      }
    }

  liftIO case exc of
    0 -> exitSuccess
    n -> exitWith (ExitFailure n)

lspOutputCallback :: Uri -> LanguageContextEnv LspConfig -> InteractionOutputCallback
lspOutputCallback uri config = liftIO . runLspT config . \case
  Resp_RunningInfo _ s -> lspDebug s
  _ -> pure ()

lspInit
  :: TCM ()
  -> LanguageContextEnv LspConfig
  -> a
  -> IO (Either ResponseError LspState)
lspInit setup config _ = do
  workers  <- newMVar mempty

  pure $ Right LspState
    { lspStateConfig  = config
    , lspStateWorkers = workers
    , lspStateSetup   = setup
    }

lspDebug :: MonadLsp cfg m => String -> m ()
lspDebug s = sendNotification SMethod_WindowLogMessage (LogMessageParams MessageType_Log (Text.pack s))

spawnOrGet :: Uri -> WorkerM (Maybe Worker)
spawnOrGet uri = withRunInIO \run -> case uriToFilePath uri of
  Just fp -> do
    state <- run ask
    let norm = toNormalizedUri uri
    modifyMVar (lspStateWorkers state) \workers -> case HashMap.lookup norm workers of
      Just worker -> pure (workers, Just worker)

      Nothing -> do
        lock   <- newMVar =<< newIORef initState
        deltas <- newMVar mempty
        chan   <- newChan

        rec
          wthread <- forkIO . forever $ do
            task <- readChan chan

            let
              report :: TCErr -> TCM ()
              report terr = do
                diag <- errorToDiagnostic terr
                liftIO . run $
                  sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams
                    { _uri         = uri
                    , _version     = Nothing
                    , _diagnostics = diag
                    }

              task' = do
                lspStateSetup state
                setInteractionOutputCallback (lspOutputCallback uri (lspStateConfig state))
                conf <- liftIO (run getConfig)
                locallyTC eHighlightingLevel (const (lspHighlightingLevel conf)) do
                  task worker `catchError` report

            withMVar lock \state -> do
              unTCM task' state initEnv
                `catch` \(e :: SomeException) -> run (lspDebug (show e))

          let
            worker = Worker
              { workerUri         = norm
              , workerFilePath    = fp
              , workerLoadedState = lock
              , workerThread      = wthread
              , workerTasks       = chan
              , workerContext     = lspStateConfig state
              , workerPosDelta    = deltas
              }

        run $ lspDebug $ "Spawned worker " <> show (workerThread worker) <> " to manage file " <> fp
        pure (HashMap.insert norm worker workers, Just worker)

  Nothing -> pure Nothing

type Task = (?worker :: Worker) => TCM ()

runAtURI :: Uri -> Task -> WorkerM ()
runAtURI uri task = spawnOrGet uri >>= \case
  Just Worker{workerTasks = chan} -> liftIO $ writeChan chan (\worker -> let ?worker = worker in task)
  Nothing -> lspDebug $ "URI is not a file: " <> show uri

onTextDocumentOpen :: Handlers WorkerM
onTextDocumentOpen = notificationHandler SMethod_TextDocumentDidOpen \notif ->
  runAtURI (notif ^. params . textDocument . uri) do
  reportSLn "lsp.lifecycle" 10 "document opened"
  reloadURI

onTextDocumentSaved :: Handlers WorkerM
onTextDocumentSaved = notificationHandler SMethod_TextDocumentDidSave \notif ->
  runAtURI (notif ^. params . textDocument . uri) do reloadURI

onTextDocumentChange :: Handlers WorkerM
onTextDocumentChange = notificationHandler SMethod_TextDocumentDidChange \notif ->
  runAtURI (notif ^. params . textDocument . uri) do
  reportSLn "lsp.lifecycle" 10 $ "document changed\n" <> unlines (map show (notif ^. params . contentChanges))

  liftIO $ modifyMVar_ (workerPosDelta ?worker) \old -> do
    let new = foldMap changeToDelta (notif ^. params . contentChanges)
    -- Important: mappend for PosDelta is *applicative order*, so this
    -- means "first apply the old diff, then apply the new diff".
    new `seq` pure (new <> old)

onTextDocumentClosed :: Handlers WorkerM
onTextDocumentClosed = notificationHandler SMethod_TextDocumentDidClose \notif ->
  withRunInIO \run -> do
    let uri' = notif ^. params . textDocument . uri
    case uriToFilePath uri' of
      Just fp -> do
        state <- run ask
        let norm = toNormalizedUri uri'
        worker <- modifyMVar (lspStateWorkers state) \workers -> pure (HashMap.delete norm workers, HashMap.lookup norm workers)
        traverse_ (killThread . workerThread) worker

      Nothing -> pure ()

notifyTCM
  :: forall (m :: Method 'ServerToClient 'Notification). (?worker :: Worker)
  => SServerMethod m -> MessageParams m -> TCM ()
notifyTCM notif param = liftIO . runLspT (workerContext ?worker) $ sendNotification notif param

requestTCM
  :: forall (m :: Method 'ServerToClient 'Request). (?worker :: Worker)
  => SServerMethod m -> MessageParams m -> (Either ResponseError (MessageResult m) -> Task) -> TCM ()
requestTCM notif param handle = liftIO . runLspT (workerContext ?worker) $ void $
  sendRequest notif param \resp -> liftIO do
    writeChan (workerTasks ?worker) (\worker -> let ?worker = worker in handle resp)

diagnoseTCM :: (?worker :: Worker) => [Diagnostic] -> TCM ()
diagnoseTCM diag = notifyTCM SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams
  { _uri         = fromNormalizedUri (workerUri ?worker)
  , _version     = Nothing
  , _diagnostics = diag
  }

toDiagnostic :: forall a. (?worker :: Worker, PrettyTCM a, Agda.HasRange a) => a -> TCM Lsp.Diagnostic
toDiagnostic x = do
  msg <- Ppr.render <$> prettyTCM x
  pure $ Lsp.Diagnostic
    { _range              = toLsp (Agda.getRange x)
    , _severity           = Just Lsp.DiagnosticSeverity_Error
    , _code               = Nothing
    , _codeDescription    = Nothing
    , _source             = Just "agda"
    , _message            = Text.pack (msg <> "\n" <> show (toLsp (Agda.getRange x)))
    , _tags               = Nothing
    , _relatedInformation = Nothing
    , _data_              = Nothing
    }

reloadURI :: Task
reloadURI = do
  resetState
  liftIO $ modifyMVar_ (workerPosDelta ?worker) \_ -> pure mempty
  diagnoseTCM []

  sf <- SourceFile <$> liftIO (absolute (workerFilePath ?worker))
  src <- Imp.parseSource sf
  cr <- Imp.typeCheckMain Imp.TypeCheck src
  requestTCM SMethod_WorkspaceSemanticTokensRefresh Nothing (const (pure ()))

  unless (null (Imp.crWarnings cr)) do
    WarningsAndNonFatalErrors warn err <- getWarningsAndNonFatalErrors
    warn <- traverse toDiagnostic warn
    err <- traverse toDiagnostic err
    diagnoseTCM (warn ++ err)

  reportSDoc "lsp.lifecycle" 10 $ "got warnings: " <+> prettyTCM (Imp.crWarnings cr)

  -- ii <- useR stInteractionPoints
  -- let
  --   edits = BiMap.elems ii >>= \ip -> do
  --     guard (isQuestion ip)
  --     pure $! TextEdit (toLsp (ipRange ip)) "{! !}"

  --   edit = ApplyWorkspaceEditParams
  --     { _label = Just "Question marks"
  --     , _edit = WorkspaceEdit
  --       { _documentChanges   = Nothing
  --       , _changeAnnotations = Nothing
  --       , _changes           = Just (Map.singleton (fromNormalizedUri (workerUri ?worker)) edits)
  --       }
  --     }

  -- requestTCM SMethod_WorkspaceApplyEdit edit (const (pure ()))

isQuestion :: InteractionPoint -> Bool
isQuestion InteractionPoint{ipRange = r}
  | Just i <- rangeToInterval r, posPos (iEnd i) == posPos (iStart i) + 1 = True
isQuestion _ = False

requestHandlerTCM
  :: forall (m :: Method 'ClientToServer 'Request) a. SMethod m
  -> (TRequestMessage m -> Uri)
  -> ((?worker :: Worker) => TRequestMessage m -> (Either ResponseError (MessageResult m) -> TCM ()) -> TCM a)
  -> Handlers WorkerM
requestHandlerTCM method uri cont = requestHandler method \req res -> withRunInIO \run -> run do
  runAtURI (uri req) $ void $ cont req \m -> liftIO (run (res m))

provideSemanticTokens :: Handlers WorkerM
provideSemanticTokens =
  requestHandlerTCM SMethod_TextDocumentSemanticTokensFull (view (params . textDocument . uri)) \req res -> do
  info <- useTC stSyntaxInfo

  delta <- liftIO $ readMVar (workerPosDelta ?worker)
  let tokens = aspectMapToTokens delta info
  reportSLn "lsp.highlighting.semantic" 10 $ "returning " <> show (Prelude.length tokens) <> " semantic tokens"

  case encodeTokens agdaTokenLegend (relativizeTokens (sortOn (\x -> (x ^. line, x ^. startChar)) tokens)) of
    Left  e    -> res $ Right (InR Lsp.Null)
    Right ints -> res $ Right $ InL (SemanticTokens Nothing ints)

onInitialized :: Handlers WorkerM
onInitialized = notificationHandler SMethod_Initialized \notif -> do
  sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/highlightingInit"))
    $ Object $ KeyMap.singleton "legend" (toJSON agdaTokenLegend)

-- | Get a substring of a file based on an Lsp range.
getFileSubstring :: VirtualFile -> Lsp.Range -> Text.Text
getFileSubstring file (Lsp.Range (Lsp.Position sL sC) (Lsp.Position eL eC)) =
  let
    rope = file ^. file_text
    (_, prefix) = Rope.charSplitAtPosition (TextLines.Position (fromIntegral sL) (fromIntegral sC)) rope

    len = if sL == eL then eC - sC else eC
    (main, _) = Rope.charSplitAtPosition (TextLines.Position (fromIntegral (eL - sL)) (fromIntegral len)) prefix
  in
  Rope.toText main

-- | Get the current/latest range of the current interaction point.
interactionRange :: PosDelta -> InteractionPoint -> Maybe Lsp.Range
interactionRange delta ip = rangeToInterval (ipRange ip) >>= updatePosition delta . toLsp

-- | Get the contents of an interaction point, if still present in the document.
interactionContents :: VirtualFile -> Lsp.Range -> Maybe Text.Text
interactionContents file range =
  fmap Text.strip . Text.stripPrefix "{!" <=< Text.stripSuffix "!}" $ getFileSubstring file range

findInteractionPoint :: (?worker :: Worker) => Lsp.Position -> TCM (Maybe (InteractionId, InteractionPoint))
findInteractionPoint pos = do
  ii <- useR stInteractionPoints
  delta <- liftIO $ readMVar (workerPosDelta ?worker)
  pure $ find (rangeContains delta pos . ipRange . snd) (BiMap.toList ii)

withPosition :: (?worker :: Worker) => Lsp.Position -> (InteractionPoint -> TCM a) -> TCM (Maybe a)
withPosition pos cont = do
  containing <- findInteractionPoint pos
  case containing of
    Just (iid, ip) -> Just <$> withInteractionId iid (cont ip)
    Nothing -> pure Nothing

goal :: Handlers WorkerM
goal = requestHandler (SMethod_CustomMethod (Proxy :: Proxy "agda/interactionPoint")) \req res -> withRunInIO \run -> run do
  case fromJSON @Location (req ^. params) of
    Success loc -> runAtURI (loc ^. uri) do
      reportSLn "lsp.goal" 10 $ show req
      ii <- useR stInteractionPoints
      delta <- liftIO $ tryReadMVar (workerPosDelta ?worker)
      reportSLn "lsp.goal" 10 $ "got delta? " <> show (isJust delta)

      let
        pos = loc ^. range . start
        containing = do
          delta <- delta
          find (rangeContains delta pos . ipRange . snd) (BiMap.toList ii)

      case containing of
        Just (iid, ip) | Just mv <- ipMeta ip -> withMetaId mv do
          gtype <- fsep
            [ pretty iid
            , colon
            , prettyTCM =<< getMetaTypeInContext mv ]
          liftIO . run . res $ Right $ Object $ KeyMap.fromList
            [ ("data", renderToJSON gtype)
            ]
          pure ()
        _ -> liftIO . run . res $ Right $ Aeson.Null
    Aeson.Error _ -> pure ()

namedCompletionItem :: Text.Text -> CompletionItem
namedCompletionItem !name = CompletionItem
  { _textEditText        = Nothing
  , _sortText            = Nothing
  , _preselect           = Nothing
  , _labelDetails        = Nothing
  , _insertTextFormat    = Nothing
  , _insertText          = Nothing
  , _filterText          = Nothing
  , _documentation       = Nothing
  , _deprecated          = Nothing
  , _commitCharacters    = Nothing
  , _insertTextMode      = Nothing
  , _textEdit            = Nothing
  , _additionalTextEdits = Nothing
  , _command             = Nothing
  , _detail              = Nothing
  , _kind                = Nothing
  , _label               = name
  , _data_               = Nothing
  , _tags                = Nothing
  }

localCompletionItem :: Int -> Dom' Term (Name, Type) -> TCM (Maybe CompletionItem)
localCompletionItem ix var@Dom{unDom = (name, ty)} = runMaybeT do
  ty <- Text.pack . Ppr.render <$> lift (prettyATop =<< reify (unEl (raise (ix + 1) ty)))

  concrete <- lift (abstractToConcrete_ name)
  guard (C.InScope == C.isInScope concrete)

  name <- Text.pack . Ppr.renderStyle (Ppr.Style Ppr.OneLineMode 0 0) <$> lift (prettyTCM name)

  pure $! namedCompletionItem name
    & sortText     ?~ "00_" <> Text.pack (show ix)
    & labelDetails ?~ CompletionItemLabelDetails (Just (" : " <> ty)) Nothing
    & kind         ?~ CompletionItemKind_Variable

headMatches :: Maybe Type -> Type -> TCM Bool
headMatches Nothing _ = pure True
headMatches (Just t) t' = do
  TelV ctx t' <- telView t'
  addContext ctx do
  t' <- fmap unEl <$> reduceB t'
  t <- reduce (raise (Prelude.length ctx) t)
  case (ignoreBlocking t', unEl t) of
    (Var{}, _)   -> pure True
    (_, MetaV{}) -> pure True
    (MetaV{}, _) -> pure True
    (Def q _, _)
      | Blocked{} <- t'    -> pure True
      | Def q' _ <- unEl t -> pure $! q == q'
    (Sort{},  Sort{})  -> pure True
    (Level{}, Level{}) -> pure True
    _ -> pure False

definedCompletionItem :: Maybe Type -> QName -> TCM (Maybe CompletionItem)
definedCompletionItem _ qnm | isExtendedLambdaName qnm = pure Nothing
definedCompletionItem _ qnm | isAbsurdLambdaName qnm = pure Nothing
definedCompletionItem want qnm = getConstInfo qnm >>= \def -> runMaybeT do
  name <- Text.pack . Ppr.render <$> lift (prettyTCM qnm)

  let
    defKind = case theDef def of
      Axiom{}                             -> Just CompletionItemKind_Constant
      I.Datatype{}                        -> Just CompletionItemKind_Enum
      I.Record{}                          -> Just CompletionItemKind_Struct
      I.Function{funProjection = Right _} -> Just CompletionItemKind_Field
      I.Function{}                        -> Just CompletionItemKind_Function
      I.Primitive{}                       -> Just CompletionItemKind_Function
      I.Constructor{}                     -> Just CompletionItemKind_EnumMember
      _                                   -> Nothing

  guard =<< lift (headMatches want (defType def))
  ty <- Text.pack . Ppr.render <$> lift (inTopContext (prettyATop =<< reify (defType def)))

  pure $! namedCompletionItem name
    & labelDetails ?~ CompletionItemLabelDetails (Just (" : " <> ty)) Nothing
    & kind         .~ defKind

completion :: Handlers WorkerM
completion = requestHandlerTCM SMethod_TextDocumentCompletion (view (params . textDocument . uri)) \req res ->
  withPosition (req ^. params . position) \ip -> do
    ctx  <- getContext
    comp <- traverse (uncurry localCompletionItem) (zip [0..] ctx)

    want  <- traverse getMetaTypeInContext (ipMeta ip)
    comp' <- traverse (definedCompletionItem want) . Set.toList =<< fmap (^. scopeInScope) getScope

    reportSLn "lsp.completion" 10 $ show req
    res (Right (InL (catMaybes comp ++ catMaybes comp')))

rangeContains :: PosDelta -> Lsp.Position -> Range' a -> Bool
rangeContains delta pos rng = any go (rangeIntervals rng) where
  go ival = isJust do
    start <- updatePosition delta (toLsp (iStart ival))
    end <- updatePosition delta (toLsp (iEnd ival))
    guard (start <= pos && pos <= end)

data DocTree = Node SemanticTokenTypes [DocTree] | Text Text.Text | Mark (Maybe SemanticTokenTypes)
  deriving Generic

instance ToJSON DocTree where
  toJSON = \case
    Node tt ds -> Object $ KeyMap.fromList [ ("tag", toJSON tt), ( "children", toJSONList ds) ]
    Text t -> toJSON t
    Mark t -> toJSON ("mark" :: String)

renderToJSON :: Doc -> Value
renderToJSON = toJSON . Ppr.fullRenderAnn Ppr.LeftMode 100 1.5 cont [] where
  consText (Ppr.Chr c) (Text t:ts) = Text (c `Text.cons` t):ts
  consText (Ppr.Str c) (Text t:ts) = Text (Text.pack c <> t):ts
  consText (Ppr.PStr c) (Text t:ts) = Text (Text.pack c <> t):ts
  consText (Ppr.Chr c) ts = Text (Text.singleton c):ts
  consText (Ppr.Str c) ts = Text (Text.pack c):ts
  consText (Ppr.PStr c) ts = Text (Text.pack c):ts

  annotate acc (Mark (Just t):ts) = Node t (reverse acc):ts
  annotate acc (Mark Nothing:ts) = reverse acc <> ts
  annotate acc (t:ts) = annotate (t:acc) ts
  annotate acc [] = __IMPOSSIBLE__

  cont :: Ppr.AnnotDetails Aspects -> [DocTree] -> [DocTree]
  cont ann acc = case ann of
    Ppr.AnnotStart  -> annotate [] acc
    Ppr.NoAnnot d _ -> consText d acc
    Ppr.AnnotEnd a
      | Just asp <- aspect a -> Mark (Just (toLsp asp)):acc
      | otherwise -> Mark Nothing:acc -- uncurry (<>) (break acc)

runLspTCM :: (?worker :: Worker) => NormalizedUri -> TCM (Maybe VirtualFile)
runLspTCM uri = liftIO $ runLspT (workerContext ?worker) (getVirtualFile uri)

onTypeFormatting :: Handlers WorkerM
onTypeFormatting = requestHandler SMethod_TextDocumentOnTypeFormatting \req res -> do
  text <- fmap virtualFileText <$> getVirtualFile (toNormalizedUri (req ^. params . textDocument . uri))
  case text of
    Just text -> do
      let
        linum = req ^. params . position . line
        l = Text.lines text !! fromIntegral linum

        edit (TokSymbol SymQuestionMark ival)
          | posPos (iEnd ival) == posPos (iStart ival) + 1
          = pure $ TextEdit {_newText="{! !}", _range=toLsp ival}
        edit _ = []

      (toks, _) <- runPMIO $ P.parsePosString P.tokensParser (Pn Strict.Nothing 0 (fromIntegral linum + 1) 1) $ Text.unpack l
      case toks of
        Right (toks, _) -> res $ Right $ InL $ toks >>= edit
        Left  _         -> res $ Right $ InR Lsp.Null

    Nothing -> pure ()

goToDefinition :: ClientCapabilities -> Handlers WorkerM
goToDefinition caps = requestHandlerTCM SMethod_TextDocumentDefinition (view (params . textDocument . uri)) \req res -> do
  reportSLn "lsp.definition" 10 $ show req

  info <- useTC stSyntaxInfo
  delta <- liftIO $ readMVar (workerPosDelta ?worker)
  tc <- getTCState

  let currentAspect = findAspect delta (req ^. params . position) info
  res case currentAspect >>= definitionSite of
    Just (DefinitionSite mod pos _ _)
      | Just path <- tc ^. stModuleToSource . key mod
      , Just info <- tc ^. stVisitedModules . key mod
      , Just (_, RangeMap.PairInt (_ :!: node)) <- Map.lookupLE pos . RangeMap.rangeMap . iHighlighting . miInterface $ info
      , aspectRange node /= noRange
      ->
        let
          srcUri = Lsp.filePathToUri (filePath path)
          -- TODO: upgrade position if in the current file.
          srcPos = toLsp (aspectRange node)
        in Right $
          if fromMaybe False ((caps ^. textDocument) >>= view declaration >>= view linkSupport)
          then
            let
              link = Lsp.LocationLink
                { _originSelectionRange = toLsp . aspectRange <$> currentAspect
                , _targetUri = srcUri
                , _targetRange = srcPos
                , _targetSelectionRange = srcPos
                }
            in InR . InL $ [DefinitionLink link]
          else InL . Definition . InL $ Lsp.Location srcUri srcPos
    _ -> notFound

  where
    notFound = Right . InR . InR $ Lsp.Null

-- | Find an aspect at the specified position.
findAspect :: PosDelta -> Lsp.Position -> RangeMap Aspects -> Maybe Aspects
findAspect delta position =
  -- TODO: This is currently a linear search. Ideally we'd be able to map
  -- positions to byte offsets directly, which'd make this much more efficient.
  -- Alternatively we could do a binary search over the aspect map, we just need
  -- to ensure that all elements have a range.
  find (rangeContains delta position . aspectRange) . map snd . RangeMap.toList

highlightReferences :: Handlers WorkerM
highlightReferences = requestHandlerTCM SMethod_TextDocumentDocumentHighlight (view (params . textDocument . uri)) \req res -> do
  reportSLn "lsp.documentHighlight" 10 $ show req

  info <- useTC stSyntaxInfo
  delta <- liftIO $ readMVar (workerPosDelta ?worker)

  let currentAspect = findAspect delta (req ^. params . position) info
  res case currentAspect >>= definitionSite of
    Nothing -> Right . InR $ Lsp.Null
    Just s@(DefinitionSite mod pos _ _) -> Right . InL
      . nubOrd
      . concatMap (mapMaybe (fmap makeHighlight . updatePosition delta . toLsp) . rangeIntervals . aspectRange . snd)
      . filter (\(_, x) -> elem s (definitionSite x))
      $ RangeMap.toList info

  where makeHighlight r = DocumentHighlight r (Just DocumentHighlightKind_Read)

getCodeActions :: Handlers WorkerM
getCodeActions = requestHandlerTCM SMethod_TextDocumentCodeAction (view (params . textDocument . uri)) \req res -> do
  let fileUri = req ^. params . textDocument . uri
  reportSLn "lsp.codeAction" 10 $ show req

  ii <- useR stInteractionPoints
  delta <- liftIO $ readMVar (workerPosDelta ?worker)

  let
    reqRange@(Lsp.Range reqStart reqEnd) = req ^. params . range

    -- Determine if this interaction point overlaps with the provided range
    -- at all
    containing =
      filter (\(Lsp.Range iS iE) -> iE >= reqStart && iS <= reqEnd)
        . mapMaybe (interactionRange delta)
        $ BiMap.elems ii

  res . Right $ case containing of
    [] -> InL []
    [range] -> InL
      [ InR $
          makeCodeAction "Run proof search on this goal" (Command_Auto fileUri (range ^. start))
        -- TODO: If the hole is non-empty, offer an option to fill
      ]
    _ -> InL []

  where
      makeCodeAction title command =
        CodeAction
        { _title = title
        , _kind = Just CodeActionKind_QuickFix
        , _diagnostics = Nothing
        , _isPreferred = Nothing
        , _disabled = Nothing
        , _edit = Nothing
        , _command = Just (toCommand title command)
        , _data_ = Nothing
        }

-- | Handle a command inside the TCM monad.
commandHandlerTCM
  :: Uri
  -> (Either ResponseError (Value |? Null) -> WorkerM ())
  -> ((?worker :: Worker)
    => (forall a. WorkerM a -> IO a)
    -> TCM (Either ResponseError (Value |? Null)))
  -> WorkerM ()
commandHandlerTCM uri res cont = withRunInIO \run -> run $
  runAtURI uri (cont run >>= (liftIO . run . res))

-- | Handle a command at an interaction point.
commandHandlerInteractionPoint
  :: Uri
  -> Lsp.Position
  -> (Either ResponseError (Value |? Null) -> WorkerM ())
  -> ((?worker :: Worker)
    => (forall a. WorkerM a -> IO a)
    -> InteractionId -> InteractionPoint -> Lsp.Range -> Text.Text
    -> TCM (Either ResponseError (Value |? Null)))
  -> WorkerM ()
commandHandlerInteractionPoint uri pos res cont = commandHandlerTCM uri res \run -> do
    pos <- findInteractionPoint pos
    virtualFile <- liftIO . run . getVirtualFile $ toNormalizedUri uri
    delta <- liftIO $ readMVar (workerPosDelta ?worker)

    case pos of
      Just (ii , ip)
        | Just range <- interactionRange delta ip
        , Just contents <- virtualFile >>= flip interactionContents range
        -> cont run ii ip range contents

      _ -> pure . Left $
        ResponseError (InL LSPErrorCodes_RequestFailed) "Cannot find interaction at this point" Nothing

executeAgdaCommand :: Handlers WorkerM
executeAgdaCommand = requestHandler SMethod_WorkspaceExecuteCommand \req res ->
  case parseCommand (req ^. params . command) (fromMaybe [] (req ^. params . arguments)) of
    Nothing -> res (Left (ResponseError (InL LSPErrorCodes_RequestFailed) "Cannot parse command" Nothing))
    Just (Command_Auto uri pos) -> commandHandlerInteractionPoint uri pos res \run ii ip range contents -> do
      -- TODO: What's the proper range here? This gets passed from the elisp side, so
      -- I think it's the *inside* of the interaction point, after edits have been applied.
      result <- Mimer.mimer ii (ipRange ip) (Text.unpack contents)
      case result of
        Mimer.MimerNoResult -> showMessage run MessageType_Error "No solution found"
        MimerExpr str -> do
          let edit = ApplyWorkspaceEditParams (Just "Proof Search") $ WorkspaceEdit
                { _changes = Just (Map.singleton uri [TextEdit range (Text.pack str)])
                , _documentChanges = Nothing
                , _changeAnnotations = Nothing
                }
          void . liftIO . run $ sendRequest SMethod_WorkspaceApplyEdit edit (\_ -> pure ())
        MimerList sols -> showMessage run MessageType_Info . Text.pack $
            "Multiple solutions:" ++
            intercalate "\n" ["  " ++ show i ++ ". " ++ s | (i, s) <- sols]
        MimerClauses{} -> __IMPOSSIBLE__    -- Mimer can't do case splitting yet

      pure (Right (InR Lsp.Null))

  where
    showMessage run kind msg = liftIO . run $ sendNotification SMethod_WindowShowMessage $ ShowMessageParams kind msg

lspHandlers :: ClientCapabilities -> Handlers WorkerM
lspHandlers caps = mconcat
  [ onTextDocumentOpen
  , onTextDocumentSaved
  , onTextDocumentClosed
  , onInitialized
  , onTextDocumentChange
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration (const (pure ()))
  , provideSemanticTokens
  , goal
  , Agda.LSP.Main.completion
  , Agda.LSP.Main.onTypeFormatting
  , goToDefinition caps
  , highlightReferences
  , getCodeActions
  , executeAgdaCommand
  ]
