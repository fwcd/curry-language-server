module Curry.LanguageServer.CPM.Config (invokeCPMConfig) where

import Control.Monad.Trans.Class (lift)
import Curry.LanguageServer.CPM.Monad
import Curry.LanguageServer.CPM.Process (invokeCPM)
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (mapMaybe)
import System.FilePath (FilePath)
import Text.Parsec

type Parser a = Parsec String () a

-- Finds the dependencies (name-version) in the project with the given path.
invokeCPMConfig :: FilePath -> CM [(String, String)]
invokeCPMConfig fp = ((mapMaybe $ rightToMaybe . parse configLine "") . lines) <$> invokeCPM fp ["config"]

configLine :: Parser (String, String)
configLine = do
    name <- many (noneOf [' '])
    spaces
    _ <- char ':'
    spaces
    value <- many (noneOf [' '])
    return (name, value)
