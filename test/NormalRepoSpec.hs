{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module NormalRepoSpec
    ( spec
    ) where

import Control.Monad
import qualified Data.ByteString as SB
import FossilHash
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec
import UnliftIO.Temporary

spec :: Spec
spec =
    around setupFossilRepo $ do
        describe "getGitInfo" $ do
            it "it makes sensible git info for a regular fossil repository" $ \fp -> do
                errOrGi <- getGitInfo fp
                case errOrGi of
                    Left err -> expectationFailure $ show err
                    Right gi -> do
                        length (giHash gi) `shouldNotBe` 128
                        giBranch gi `shouldBe` "trunk"
                        giDirty gi `shouldBe` False
                        giCommitDate gi `shouldNotBe` []
                        giCommitCount gi `shouldBe` {- empty is 1 -} 2
                        giCommitMessage gi `shouldBe` "Initial commit (user: usery)"
                        length (giDescribe gi) `shouldBe` 8
        describe "getGitRoot" $ do
            it "it gets the expected git root for a regular git repository" $ \fp ->
                getGitRoot fp `shouldReturn` Right fp

setupFossilRepo :: (FilePath -> IO ()) -> IO ()
setupFossilRepo runTest =
    withSystemTempDirectory "normal" $ \fp -> do
        createDirectoryIfMissing True fp
        let runFossil args =
                void $ readCreateProcess ((proc "fossil" args) {cwd = Just fp}) ""
        runFossil ["init", repoFile]
        runFossil ["open", repoFile]
        SB.writeFile
            (fp </> "README.md")
            "This is a readme, you should read it."
        runFossil ["add", "README.md"]
        runFossil
            [ "commit"
            , "--user-override", "usery"
            , "-m"
            , "Initial commit"
            ]
        runTest fp

repoFile :: String
repoFile = "fossil.db"
