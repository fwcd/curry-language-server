{-# LANGUAGE OverloadedStrings #-}
module Curry.LanguageServer.Index.Convert (
    ToSymbols (..)
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Base.Types as CT
import qualified Base.Kinds as CK
import qualified Env.TypeConstructor as CETC
import qualified Env.Value as CEV

import Control.Monad.Trans.Maybe (runMaybeT)
import Curry.LanguageServer.Index.Symbol (Symbol (..), SymbolKind (..))
import Curry.LanguageServer.Utils.Convert (ppToText, currySpanInfo2Location)
import Curry.LanguageServer.Utils.General (lastSafe)
import Data.Default (Default (..))
import Data.List (inits)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

class ToSymbols s where
    toSymbols :: s -> IO [Symbol]
    
instance ToSymbols CEV.ValueInfo where
    toSymbols vinfo = pure <$> case vinfo of
        CEV.DataConstructor q _ ls t  -> (\s -> s { sConstructors = ppToText <$> ls })
                                     <$> makeValueSymbol ValueConstructor q t
        CEV.NewtypeConstructor q _ t  -> makeValueSymbol ValueConstructor q t
        CEV.Value q _ _ t             -> makeValueSymbol ValueFunction q t
        CEV.Label q _ t               -> makeValueSymbol ValueFunction q t

instance ToSymbols CETC.TypeInfo where
    toSymbols tinfo = case tinfo of
        CETC.DataType q k _     -> pure <$> makeTypeSymbol TypeData q k
        CETC.RenamingType q k _ -> pure <$> makeTypeSymbol TypeNew q k
        CETC.AliasType q k _ _  -> pure <$> makeTypeSymbol TypeAlias q k
        CETC.TypeClass q k _    -> pure <$> makeTypeSymbol TypeClass q k
        CETC.TypeVar _          -> return []

instance ToSymbols CI.ModuleIdent where
    toSymbols mid = do
        loc <- runMaybeT $ currySpanInfo2Location mid
        return $ do
            quals <- tail $ inits $ T.pack <$> CI.midQualifiers mid
            return def
                { sKind = Module
                , sQualIdent = T.intercalate "." quals
                , sIdent = fromMaybe "" $ lastSafe quals
                , sLocation = loc
                }

makeValueSymbol :: SymbolKind -> CI.QualIdent -> CT.TypeScheme -> IO Symbol
makeValueSymbol k q t = do
    loc <- runMaybeT $ currySpanInfo2Location q
    return def
        { sKind = k
        , sQualIdent = ppToText q
        , sIdent = ppToText $ CI.qidIdent q
        , sPrintedType = Just $ ppToText t
        , sArrowArity = Just $ CT.arrowArity $ CT.rawType t
        , sLocation = loc
        }

makeTypeSymbol :: SymbolKind -> CI.QualIdent -> CK.Kind -> IO Symbol
makeTypeSymbol k q k' = do
    loc <- runMaybeT $ currySpanInfo2Location q
    return def
        { sKind = k
        , sQualIdent = ppToText q
        , sIdent = ppToText $ CI.qidIdent q
        , sPrintedType = Just $ ppToText k'
        , sArrowArity = Just $ CK.kindArity k'
        , sLocation = loc
        }
