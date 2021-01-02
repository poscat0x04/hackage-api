{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Exception
import Data.Foldable
import Distribution.Hackage.API
import Distribution.Hackage.Types
import Network.HTTP.Client.TLS
import Servant.Client

data TestCase = forall a. TestCase (ClientM a)

main :: IO ()
main = do
  manager <- newTlsManager
  let testCases =
        [ TestCase getPackages,
          TestCase $ getCabalFile (Package "Cabal" Default),
          TestCase $ getCabalFile' (Package "Cabal" Default) 0,
          TestCase $ getVersions (Package "Cabal" Default),
          TestCase $ getRevisions (Package "Cabal" Default)
        ]
  traverse_ (checkRun manager) testCases

check :: (Exception a) => Either a b -> IO ()
check (Left e) = throwIO e
check (Right _) = pure ()

checkRun manager (TestCase m) = runClient manager m >>= check
