{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Curry.LanguageServer.Utils.VFS
    ( PosPrefixInfo (..)
    , getCompletionPrefix
    ) where

import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Utf16.Rope.Mixed as Rope
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.VFS as VFS
import Data.Char (isAlphaNum)

-- Source: https://github.com/haskell/haskell-language-server/blob/a4bcaa31/ghcide/src/Development/IDE/Plugin/Completions/Types.hs#L134-L152
-- License: Apache 2.0

-- | Describes the line at the current cursor position
data PosPrefixInfo = PosPrefixInfo
    { fullLine    :: !T.Text
      -- ^ The full contents of the line the cursor is at

    , prefixScope :: !T.Text
      -- ^ If any, the module name that was typed right before the cursor position.
      --  For example, if the user has typed "Data.Maybe.from", then this property
      --  will be "Data.Maybe"
      -- If OverloadedRecordDot is enabled, "Shape.rect.width" will be
      -- "Shape.rect"

    , prefixText  :: !T.Text
      -- ^ The word right before the cursor position, after removing the module part.
      -- For example if the user has typed "Data.Maybe.from",
      -- then this property will be "from"
    , cursorPos   :: !J.Position
      -- ^ The cursor position
    } deriving (Show,Eq)

-- Source: https://github.com/haskell/haskell-language-server/blob/a4bcaa31/ghcide/src/Development/IDE/Plugin/Completions/Logic.hs#L889-L916
-- License: Apache 2.0

getCompletionPrefix :: J.Position -> VFS.VirtualFile -> PosPrefixInfo
getCompletionPrefix pos (VFS.VirtualFile _ _ ropetext) = getCompletionPrefixFromRope pos ropetext

getCompletionPrefixFromRope :: J.Position -> Rope.Rope -> PosPrefixInfo
getCompletionPrefixFromRope pos@(J.Position l c) ropetext =
    fromMaybe (PosPrefixInfo "" "" "" pos) $ do -- Maybe monad
        let headMaybe = listToMaybe
            lastMaybe = headMaybe . reverse

        -- grab the entire line the cursor is at
        curLine <- headMaybe $ Rope.lines
                             $ fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (fromIntegral l) ropetext
        let beforePos = T.take (fromIntegral c) curLine
        -- the word getting typed, after previous space and before cursor
        curWord <-
            if | T.null beforePos        -> Just ""
               | T.last beforePos == ' ' -> Just "" -- don't count abc as the curword in 'abc '
               | otherwise               -> lastMaybe (T.words beforePos)

        let parts = T.split (=='.')
                      $ T.takeWhileEnd (\x -> isAlphaNum x || x `elem` ("._'"::String)) curWord
        case reverse parts of
          [] -> Nothing
          (x:xs) -> do
            let modParts = reverse $ filter (not .T.null) xs
                modName = T.intercalate "." modParts
            return $ PosPrefixInfo { fullLine = curLine, prefixScope = modName, prefixText = x, cursorPos = pos }
