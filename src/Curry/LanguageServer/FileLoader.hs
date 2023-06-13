module Curry.LanguageServer.FileLoader (fileLoader) where

import Control.Monad.IO.Unlift (askRunInIO)
import qualified Curry.LanguageServer.Compiler as C
import Curry.LanguageServer.Monad (LSM)
import Curry.LanguageServer.Utils.Uri (filePathToNormalizedUri)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.VFS as VFS

fileLoader :: LSM C.FileLoader
fileLoader = do
    runInIO <- askRunInIO
    return $ \fp -> do
        normUri <- filePathToNormalizedUri fp
        vfile <- runInIO $ S.getVirtualFile normUri
        
        case T.unpack . VFS.virtualFileText <$> vfile of
            Just vfsContent -> return vfsContent
            Nothing -> readFile fp
