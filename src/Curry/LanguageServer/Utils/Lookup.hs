-- | (Value) environments and position lookup in the AST.
module Curry.LanguageServer.Utils.Lookup (
    LM,
    runLM,
    findQualIdentAtPos,
    findTypeAtPos,
    getModuleIdentifier,
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.SpanInfo as CSPI
import qualified Base.Types as CT

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax
import qualified Language.LSP.Types as J

type LM a = MaybeT (ReaderT ModuleAST IO) a

-- | Runs the lookup monad with the given environment and
-- module identifier.
runLM :: LM a -> ModuleAST -> IO (Maybe a)
runLM = runReaderT . runMaybeT

-- | Finds identifier and (occurrence) span info at a given position.
findQualIdentAtPos :: J.Position -> LM (Maybe (CI.QualIdent, CSPI.SpanInfo))
findQualIdentAtPos pos = do
    ast <- lift ask
    let qualIdent = withSpanInfo <$> elementAt pos (qualIdentifiers ast)
        exprIdent = joinFst $ qualIdentifier <.$> withSpanInfo <$> elementAt pos (expressions ast)
        basicIdent = CI.qualify <.$> withSpanInfo <$> elementAt pos (identifiers ast)
    return $ qualIdent <|> exprIdent <|> basicIdent

-- | Finds the type at the given position.
findTypeAtPos :: J.Position -> LM (Maybe (TypedSpanInfo CT.PredType))
findTypeAtPos pos = do
    ast <- lift ask
    let typedSpanInfo = elementAt pos $ typedSpanInfos ast
    return typedSpanInfo

getModuleIdentifier :: LM CI.ModuleIdent
getModuleIdentifier = moduleIdentifier <$> lift ask

withSpanInfo :: CSPI.HasSpanInfo a => a -> (a, CSPI.SpanInfo)
withSpanInfo x = (x, CSPI.getSpanInfo x)
