module Curry.LanguageServer.CPM.Deps (invokeCPMDeps) where

import Curry.LanguageServer.CPM.Monad
import Curry.LanguageServer.CPM.Process (invokeCPM)
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (mapMaybe)
import System.FilePath (FilePath)
import Text.Parsec

type Parser a = Parsec String () a

-- | Finds the dependencies (name-version) in the project with the given path.
invokeCPMDeps :: FilePath -> CM [String]
invokeCPMDeps fp = (mapMaybe $ rightToMaybe . parse depLine "") . lines <$> invokeCPM fp ["deps"]

depLine :: Parser String
depLine = do
    spaces
    _ <- string "|-"
    spaces
    many (noneOf [' '])
