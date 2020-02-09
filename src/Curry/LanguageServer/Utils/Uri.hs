-- Provides accurate (canonicalized) mappings between URIs and file paths.
module Curry.LanguageServer.Utils.Uri (
    filePathToUri,
    uriToFilePath,
    filePathToNormalizedUri,
    normalizedUriToFilePath,
    normalizeUriWithPath
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

-- | Normalizes a URI by converting to a file path and back (thus ensuring
-- consistent formatting e.g. of drive letters on Windows).
normalizeUriWithPath :: J.Uri -> IO J.NormalizedUri
normalizeUriWithPath uri = J.toNormalizedUri <$> (maybe (return uri) id $ filePathToUri <$> uriToFilePath uri)
