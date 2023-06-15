module Curry.LanguageServer.CPM.Deps
    ( generatePathsJsonWithCPM
    , readPathsJson
    ) where

import Control.Monad.Error.Class (MonadError (..))
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
    let pathsJsonPath = dirPath </> ".curry" </> "language-server" </> "paths.json"
    result <- liftIO $ decodeFileStrict pathsJsonPath
    case result of
        Just paths -> return paths
        Nothing    -> throwError $ "Could not read or decode " <> pathsJsonPath <> "!"
