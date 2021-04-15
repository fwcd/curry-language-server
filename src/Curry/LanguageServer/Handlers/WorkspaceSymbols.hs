module Curry.LanguageServer.Handlers.WorkspaceSymbols (workspaceSymbolHandler) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Curry.LanguageServer.Index.Store as I
import qualified Curry.LanguageServer.Index.Symbol as I
import Curry.LanguageServer.Monad
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger

workspaceSymbolHandler :: S.Handlers LSM
workspaceSymbolHandler = S.requestHandler J.SWorkspaceSymbol $ \req responder -> do
    liftIO $ debugM "cls.workspaceSymbols" "Processing workspace symbols request"
    let query = req ^. J.params . J.query
    store <- getStore
    symbols <- liftIO $ fetchWorkspaceSymbols store query
    let maxSymbols = 150
    responder $ Right $ J.List $ take maxSymbols symbols

fetchWorkspaceSymbols :: I.IndexStore -> T.Text -> IO [J.SymbolInformation]
fetchWorkspaceSymbols store query = do
    debugM "cls.workspaceSymbols" $ "Searching " ++ show (I.storedSymbolCount store) ++ " symbol(s)..."
    let symbols = mapMaybe toWorkspaceSymbol $ I.storedSymbolsWithPrefix query store
    infoM "cls.workspaceSymbols" $ "Found " ++ show (length symbols) ++ " symbol(s)"
    return symbols

toWorkspaceSymbol :: I.Symbol -> Maybe J.SymbolInformation
toWorkspaceSymbol s = (\loc -> J.SymbolInformation name kind tags deprecated loc containerName) <$> I.sLocation s
    where name = I.sIdent s
          kind = case I.sKind s of
              I.ValueFunction    | I.sArrowArity s == Just 0 -> J.SkConstant
                                 | otherwise                 -> J.SkFunction
              I.ValueConstructor | I.sArrowArity s == Just 0 -> J.SkEnumMember
                                 | otherwise                 -> J.SkConstructor
              I.Module                                       -> J.SkModule
              I.TypeData | length (I.sConstructors s) == 1   -> J.SkStruct
                         | otherwise                         -> J.SkEnum
              I.TypeNew                                      -> J.SkStruct
              I.TypeAlias                                    -> J.SkInterface
              I.TypeClass                                    -> J.SkInterface
              I.TypeVar                                      -> J.SkVariable
              I.Other                                        -> J.SkNamespace
          tags = Nothing
          deprecated = Nothing
          containerName = Just $ I.sParentIdent s
