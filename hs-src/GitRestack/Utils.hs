{-# LANGUAGE OverloadedStrings #-}

module GitRestack.Utils
  ( trimBS
  , trimText
  , extractJiraPrefix
  , formatTimestamp
  , currentMillis
  , isBinary
  , encodeBase64
  , decodeBase64
  , containsIgnoreCase
  , detectMergetoolSide
  , fixBranchPrefix
  , makeFixBranchName
  , isFixBranch
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as Base64
import Data.Char (isAlpha, isDigit, toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)

trimBS :: ByteString -> ByteString
trimBS = BSC.dropWhile isSpaceByte . dropWhileEnd isSpaceByte
  where
    isSpaceByte c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
    dropWhileEnd p = BSC.reverse . BSC.dropWhile p . BSC.reverse

trimText :: Text -> Text
trimText = Text.strip

extractJiraPrefix :: Text -> Maybe Text
extractJiraPrefix name =
  let stripped
        | "feature/" `Text.isPrefixOf` name = Text.drop 8 name
        | "bugfix/" `Text.isPrefixOf` name = Text.drop 7 name
        | otherwise = name
      (prefix, rest) = Text.span isAlpha stripped
  in case Text.uncons rest of
       Just ('-', tailText) ->
         let (digits, _) = Text.span isDigit tailText
         in if Text.null prefix || Text.null digits then Nothing else Just prefix
       _ -> Nothing

formatTimestamp :: IO Text
formatTimestamp = do
  now <- getCurrentTime
  pure (Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))

currentMillis :: IO Integer
currentMillis = do
  now <- getPOSIXTime
  pure (floor (now * 1000))

isBinary :: ByteString -> Bool
isBinary = BS.any (== 0)

encodeBase64 :: ByteString -> ByteString
encodeBase64 = Base64.encode

decodeBase64 :: ByteString -> Either String ByteString
decodeBase64 = Base64.decode

containsIgnoreCase :: Text -> Text -> Bool
containsIgnoreCase haystack needle
  | Text.null needle = False
  | Text.length haystack < Text.length needle = False
  | otherwise =
      let lowerHay = Text.map toLower haystack
          lowerNeedle = Text.map toLower needle
      in lowerNeedle `Text.isInfixOf` lowerHay

detectMergetoolSide :: Maybe Text -> Maybe Text
detectMergetoolSide Nothing = Nothing
detectMergetoolSide (Just tool)
  | containsIgnoreCase tool "theirs" = Just "theirs"
  | containsIgnoreCase tool "ours" = Just "ours"
  | otherwise = Nothing

fixBranchPrefix :: Text
fixBranchPrefix = "git-restack/fix/"

makeFixBranchName :: Text -> Text
makeFixBranchName name = fixBranchPrefix <> name

isFixBranch :: Text -> Bool
isFixBranch = Text.isPrefixOf fixBranchPrefix
