module Curry.LanguageServer.Index.Convert (
    ToSymbol (..)
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
import Data.Default (Default (..))

class ToSymbol s where
    toSymbol :: s -> IO (Maybe Symbol)
    
instance ToSymbol CEV.ValueInfo where
    toSymbol vinfo = Just <$> case vinfo of
        CEV.DataConstructor q _ _ t   -> makeValueSymbol ValueConstructor q t
        CEV.NewtypeConstructor q _ t  -> makeValueSymbol ValueConstructor q t
        CEV.Value q _ _ t             -> makeValueSymbol ValueFunction q t
        CEV.Label q _ t               -> makeValueSymbol ValueFunction q t

instance ToSymbol CETC.TypeInfo where
    toSymbol tinfo = case tinfo of
        CETC.DataType q k _     -> Just <$> makeTypeSymbol TypeData q k
        CETC.RenamingType q k _ -> Just <$> makeTypeSymbol TypeNew q k
        CETC.AliasType q k _ _  -> Just <$> makeTypeSymbol TypeAlias q k
        CETC.TypeClass q k _    -> Just <$> makeTypeSymbol TypeClass q k
        CETC.TypeVar _          -> return Nothing

makeValueSymbol :: SymbolKind -> CI.QualIdent -> CT.TypeScheme -> IO Symbol
makeValueSymbol k q t = do
    loc <- runMaybeT $ currySpanInfo2Location q
    return def
        { sKind = k
        , sQualIdent = ppToText q
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
        , sPrintedType = Just $ ppToText k'
        , sArrowArity = Just $ CK.kindArity k'
        , sLocation = loc
        }
