{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2025 Robin Palotai, githash: 2018 Michael Snoyman, 2015 Adam C. Foltzer
-- License     :  BSD3
-- Maintainer  :  palotai.robin@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some handy Template Haskell splices for including the current fossil
-- hash and branch in the code of your project. Useful for including
-- in panic messages, @--version@ output, or diagnostic info for more
-- informative bug reports.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import FossilHash
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", giBranch gi, "@", giHash gi
-- >                  , " (", giCommitDate gi, ")"
-- >                  , " (", show (giCommitCount gi), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | giDirty gi = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >         gi = $$tGitInfoCwd
-- >
-- > main = panic "oh no!"
--
-- > % stack runghc Example.hs
-- > Example.hs: [panic master@2ae047ba5e4a6f0f3e705a43615363ac006099c1 (Mon Jan 11 11:50:59 2016 -0800) (14 commits in HEAD) (uncommitted files present)] oh no!
--
-- WARNING: None of this will work in a fossil repository without any commits. (TBD)
--
-- @since 0.1.0.0
module FossilHash
  ( -- * Types
    GitInfo
  , GitHashException (..)
    -- ** Getters
  , giHash
  , giBranch
  , giDirty
  , giCommitDate
  , giCommitCount
  , giCommitMessage
  , giDescribe
  , giTag
  , giFiles
    -- * Creators
  , getGitInfo
  , getGitRoot
    -- * Template Haskell
  , tGitInfo
  , tGitInfoCwd
  , tGitInfoTry
  , tGitInfoCwdTry
  ) where

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Process
import Text.Read (readMaybe)

-- | Various pieces of information about a Git repository.
--
-- @since 0.1.0.0
data GitInfo = GitInfo
  { _giHash :: !String
  , _giBranch :: !String
  , _giDirty :: !Bool
  , _giCommitDate :: !String
  , _giCommitCount :: !Int
  , _giFiles :: ![FilePath]
  , _giCommitMessage :: !String
  , _giDescribe :: !String
  , _giTag :: !String
  }
  deriving (Lift, Show)

-- | The hash of the most recent commit.
--
-- @since 0.1.0.0
giHash :: GitInfo -> String
giHash = _giHash

-- | The hash of the most recent commit.
--
-- @since 0.1.0.0
giBranch :: GitInfo -> String
giBranch = _giBranch

giDirty :: GitInfo -> Bool
giDirty = _giDirty

giCommitDate :: GitInfo -> String
giCommitDate = _giCommitDate

giCommitCount :: GitInfo -> Int
giCommitCount = _giCommitCount

-- | The message of the most recent commit.
--
-- @since 0.1.1.0
giCommitMessage :: GitInfo -> String
giCommitMessage = _giCommitMessage

-- | The output of @git describe --always@ for the most recent commit.
--
-- @since 0.1.4.0
giDescribe :: GitInfo -> String
giDescribe = _giDescribe

-- | The output of @git describe --always --tags@ for the most recent commit.
--
-- @since 0.1.5.0
giTag :: GitInfo -> String
giTag = _giTag

-- | The files used to determine whether recompilation is necessary in splices.
--
-- @since 0.1.7.0
giFiles :: GitInfo -> [FilePath]
giFiles = _giFiles

-- | Get a list of dependent fossil related files.
--
-- NOTE(fslckout): Note: need to perform a fossil command for the .fslckout to
-- update.
--
getFossilFiles :: FilePath -> IO [FilePath]
getFossilFiles fslckout = pure [fslckout]

-- | Get the 'GitInfo' for the given root directory. Root directory
-- should be the directory containing the @.fslckout@ file.
--
-- @since 0.1.0.0
getGitInfo :: FilePath -> IO (Either GitHashException GitInfo)
getGitInfo root = try $ do
  let run args = do
        eres <- runFossil root args
        case eres of
          Left e -> throwIO e
          Right str -> pure (takeWhile (/= '\n') str)

  let runKey key args = do
        eres <- runFossil root args
        case eres of
          Left e -> throwIO e
          Right str ->
            pure (concatMap (drop 1) 
                  . take 1 . filter (\ws -> headMay ws == Just key) 
                  . map words . lines
                  $ str)

  _giFiles <- getFossilFiles (root </> ".fslckout")
  _giHash <- unwords . take 1 <$> runKey "checkout:" ["info"]
  _giBranch <- fromMaybe "?" . headMay . map dropTrailingComma <$> runKey "tags:" ["info"]

  extraString <- run ["extra"]
  changesString <- run ["changes"]
  let _giDirty = not $ (null (extraString :: String) && null (changesString :: String))

  commitCount <- unwords <$> runKey "check-ins:" ["info"]
  _giCommitCount <-
    case readMaybe commitCount of
      Nothing -> throwIO $ GHEInvalidCommitCount root commitCount
      Just x -> return x

  _giCommitDate <- unwords . drop 1 <$> runKey "checkout:" ["info"]

  _giCommitMessage <- unwords <$> runKey "comment:" ["info"]

  _giDescribe <- {- approximate -} pure (take 8 _giHash)

  _giTag <- unwords <$> runKey "tags:" ["info"]

  return GitInfo {..}

dropTrailingComma :: String -> String
dropTrailingComma = takeWhile (/= ',')

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

-- | Get the root directory of the Git repo containing the given file
-- path.
--
-- @since 0.1.0.0
getGitRoot :: FilePath -> IO (Either GitHashException FilePath)
getGitRoot dir = fmap (normalise . takeWhile (/= '\n') . getLocalRoot) `fmap` (runFossil dir ["info"])
  where
    getLocalRoot = dropFinalSlash . unwords . concatMap (drop 1) . take 1 
      . filter (\ws -> headMay ws == Just "local-root:") . map words . lines
    dropFinalSlash = reverse . dropWhile (== '/') . reverse

runFossil :: FilePath -> [String] -> IO (Either GitHashException String)
runFossil root args = do
  let cp = (proc "fossil" args) { cwd = Just root }
  eres <- try $ readCreateProcessWithExitCode cp ""
  return $ case eres of
    Left e -> Left $ GHEGitRunException root args e
    Right (ExitSuccess, out, _) -> Right out
    Right (ec@ExitFailure{}, out, err) -> Left $ GHEGitRunFailed root args ec out err

-- | Exceptions which can occur when using this library's functions.
--
-- @since 0.1.0.0
data GitHashException
  = GHECouldn'tReadFile !FilePath !IOException
  | GHEInvalidCommitCount !FilePath !String
  | GHEInvalidGitFile !String
  | GHEGitRunFailed !FilePath ![String] !ExitCode !String !String
  | GHEGitRunException !FilePath ![String] !IOException
  deriving (Show, Eq, Typeable)
instance Exception GitHashException

-- | Load up the 'GitInfo' value at compile time for the given
-- directory. Compilation fails if no info is available.
--
-- @since 0.1.0.0
tGitInfo :: FilePath -> SpliceQ GitInfo
tGitInfo fp = unsafeSpliceCoerce $ do
  gi <- runIO $
    getGitRoot fp >>=
    either throwIO return >>=
    getGitInfo >>=
    either throwIO return
  -- NOTE(fslckout): getGitInfo ran fossil, so file should be updated
  mapM_ addDependentFile (_giFiles gi)  
  lift (gi :: GitInfo) -- adding type sig to make the unsafe look slightly better

-- | Try to load up the 'GitInfo' value at compile time for the given
-- directory.
--
-- @since 0.1.2.0
tGitInfoTry :: FilePath -> SpliceQ (Either String GitInfo)
tGitInfoTry fp = unsafeSpliceCoerce $ do
  egi <- runIO $ do
    eroot <- getGitRoot fp
    case eroot of
      Left e -> return $ Left $ show e
      Right root -> do
        einfo <- getGitInfo root
        case einfo of
          Left e -> return $ Left $ show e
          Right info -> return $ Right info
  case egi of
    Left _ -> return ()
    -- NOTE(fslckout): getGitInfo ran fossil, so file should be updated
    Right gi -> mapM_ addDependentFile (_giFiles gi)
  lift (egi :: Either String GitInfo) -- adding type sig to make the unsafe look slightly better

-- | Load up the 'GitInfo' value at compile time for the current
-- working directory.
--
-- @since 0.1.0.0
tGitInfoCwd :: SpliceQ GitInfo
tGitInfoCwd = tGitInfo "."

-- | Try to load up the 'GitInfo' value at compile time for the current
-- working directory.
--
-- @since 0.1.2.0
tGitInfoCwdTry :: SpliceQ (Either String GitInfo)
tGitInfoCwdTry = tGitInfoTry "."
