{-# LANGUAGE OverloadedStrings, FlexibleInstances, ViewPatterns #-}
module Curry.LanguageServer.Index.Convert
    ( ToSymbols (..)
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Base.TopEnv as CTE
import qualified Base.Types as CT
import qualified Base.Kinds as CK
import qualified Env.TypeConstructor as CETC
import qualified Env.Value as CEV

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Maybe (runMaybeT)
import Curry.LanguageServer.Index.Symbol (Symbol (..), SymbolKind (..))
import Curry.LanguageServer.Utils.Convert (ppToText, currySpanInfo2Location, ppToTextPrec)
import Curry.LanguageServer.Utils.General (lastSafe)
import Data.Default (Default (..))
import Data.List (inits)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

class ToSymbols s where
    toSymbols :: MonadIO m => s -> m [Symbol]
    
instance ToSymbols (CI.QualIdent, CEV.ValueInfo) where
    toSymbols (q, vinfo)
        | CI.isQualified q' = pure <$> case vinfo of
            CEV.DataConstructor _ _ ls t  -> (\s -> s { constructors = ppToText <$> ls })
                                        <$> makeValueSymbol ValueConstructor q' t
            CEV.NewtypeConstructor _ _ t  -> makeValueSymbol ValueConstructor q' t
            CEV.Value _ _ _ t             -> makeValueSymbol ValueFunction q' t
            CEV.Label _ _ t               -> makeValueSymbol ValueFunction q' t
        | otherwise         = return []
        where q' = qualifyWithModuleFrom vinfo q

instance ToSymbols (CI.QualIdent, CETC.TypeInfo) where
    toSymbols (q, tinfo)
        | CI.isQualified q' = case tinfo of
            CETC.DataType _ k _     -> pure <$> makeTypeSymbol TypeData q' k
            CETC.RenamingType _ k _ -> pure <$> makeTypeSymbol TypeNew q' k
            CETC.AliasType _ k _ _  -> pure <$> makeTypeSymbol TypeAlias q' k
            CETC.TypeClass _ k _    -> pure <$> makeTypeSymbol TypeClass q' k
            CETC.TypeVar _          -> return []
        | otherwise         = return []
        where q' = qualifyWithModuleFrom tinfo q

instance ToSymbols CI.ModuleIdent where
    toSymbols mid = do
        loc <- runMaybeT $ currySpanInfo2Location mid
        return $ do
            quals <- tail $ inits $ T.pack <$> CI.midQualifiers mid
            return def
                { kind = Module
                , qualIdent = T.intercalate "." quals
                , ident = fromMaybe "" $ lastSafe quals
                , location = loc
                }

qualifyWithModuleFrom :: CTE.Entity a => a -> CI.QualIdent -> CI.QualIdent
qualifyWithModuleFrom (CTE.origName -> CI.qidModule -> mid) q = q { CI.qidModule = CI.qidModule q <|> mid }

makeValueSymbol :: MonadIO m => SymbolKind -> CI.QualIdent -> CT.TypeScheme -> m Symbol
makeValueSymbol k q t = do
    loc <- runMaybeT $ currySpanInfo2Location $ CI.qidIdent q
    return def
        { kind = k
        , qualIdent = ppToText q
        , ident = ppToText $ CI.qidIdent q
        , printedType = Just $ ppToText t
        -- We explicitly perform the Type -> TypeExpr conversion here since
        -- the Pretty Type instance ignores the precedence.
        , printedArgumentTypes = ppToTextPrec 2 . CT.fromType CI.identSupply <$> CT.arrowArgs (CT.rawType t)
        , printedResultType = Just $ ppToText $ CT.arrowBase (CT.rawType t)
        , arrowArity = Just $ CT.arrowArity $ CT.rawType t
        , location = loc
        }

makeTypeSymbol :: MonadIO m => SymbolKind -> CI.QualIdent -> CK.Kind -> m Symbol
makeTypeSymbol k q k' = do
    loc <- runMaybeT $ currySpanInfo2Location $ CI.qidIdent q
    return def
        { kind = k
        , qualIdent = ppToText q
        , ident = ppToText $ CI.qidIdent q
        , printedType = Just $ ppToText k'
        -- We explicitly perform the Kind conversion here since
        -- the Pretty Kind instance ignores the precedence.
        , printedArgumentTypes = ppToTextPrec 2 . CK.fromKind <$> CK.kindArgs k'
        , printedResultType = Just $ ppToText $ kindBase k'
        , arrowArity = Just $ CK.kindArity k'
        , location = loc
        }
    where kindBase (CK.KindArrow _ k'') = kindBase k''
          kindBase k''                  = k''
