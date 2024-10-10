-- | Accurate (canonicalized) mappings between URIs and file paths.
module Curry.LanguageServer.Utils.Uri
    ( filePathToUri
    , uriToFilePath
    , filePathToNormalizedUri
    , normalizedUriToFilePath
    , normalizeUriWithPath
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Language.LSP.Protocol.Types as J
import System.Directory (canonicalizePath)

filePathToUri :: MonadIO m => FilePath -> m J.Uri
filePathToUri = liftIO . (J.filePathToUri <$>) . canonicalizePath

uriToFilePath :: J.Uri -> Maybe FilePath
uriToFilePath = J.uriToFilePath

filePathToNormalizedUri :: MonadIO m => FilePath -> m J.NormalizedUri
filePathToNormalizedUri = liftIO . (J.toNormalizedUri <$>) . filePathToUri

normalizedUriToFilePath :: J.NormalizedUri -> Maybe FilePath
normalizedUriToFilePath = uriToFilePath . J.fromNormalizedUri

-- | Normalizes a URI by converting to a file path and back (thus ensuring
-- consistent formatting e.g. of drive letters on Windows).
normalizeUriWithPath :: MonadIO m => J.Uri -> m J.NormalizedUri
normalizeUriWithPath uri = liftIO (J.toNormalizedUri <$> maybe (return uri) filePathToUri (uriToFilePath uri))
