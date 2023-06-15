module Curry.LanguageServer.CPM.Deps
    ( generatePathsJsonWithCPM
    , readPathsJson
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Curry.LanguageServer.CPM.Monad (CPMM)
import Curry.LanguageServer.CPM.Process (invokeCPM)
import Control.Monad (void)
import Data.Aeson (decodeFileStrict)
import System.FilePath ((</>))

-- | Tries generating the '.curry/language-server/paths.json' from a CPM package's dependencies.
generatePathsJsonWithCPM :: FilePath -> FilePath -> CPMM ()
generatePathsJsonWithCPM dirPath = void . invokeCPM dirPath ["deps", "--language-server"]

-- | Reads the '.curry/language-server/paths.json'.
readPathsJson :: FilePath -> CPMM [FilePath]
readPathsJson dirPath = do
    result <- liftIO $ decodeFileStrict $ dirPath </> ".curry" </> "language-server" </> "paths.json"
    case result of
        Just paths -> return paths
        Nothing    -> fail "Could not read paths.json!"
