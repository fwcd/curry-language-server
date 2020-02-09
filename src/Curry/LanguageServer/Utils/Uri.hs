-- Provides accurate (canonicalized) mappings between URIs and file paths.
module Curry.LanguageServer.Utils.Uri (
    filePathToUri,
    uriToFilePath,
    filePathToNormalizedUri,
    normalizedUriToFilePath
) where

import Control.Monad.IO.Class (liftIO)
import Curry.LanguageServer.Logging
import qualified Language.Haskell.LSP.Types as J
import System.Directory
import System.FilePath

filePathToUri :: FilePath -> IO J.Uri
filePathToUri = (J.filePathToUri <$>) . canonicalizePath

uriToFilePath :: J.Uri -> Maybe FilePath
uriToFilePath = J.uriToFilePath

filePathToNormalizedUri :: FilePath -> IO J.NormalizedUri
filePathToNormalizedUri = (J.toNormalizedUri <$>) . filePathToUri

normalizedUriToFilePath :: J.NormalizedUri -> Maybe FilePath
normalizedUriToFilePath = uriToFilePath . J.fromNormalizedUri
