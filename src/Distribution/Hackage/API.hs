module Distribution.Hackage.API where

import Data.Proxy
import Distribution.Hackage.Types
import Distribution.PackageDescription
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client

getPackages :: ClientM [Package]
getVersions :: Package -> ClientM Versions
getRevisions :: Package -> ClientM [Revision]
getCabalFile :: Package -> ClientM GenericPackageDescription
getCabalFile' :: Package -> Int -> ClientM GenericPackageDescription
getPackages :<|> getVersions :<|> getRevisions :<|> getCabalFile :<|> getCabalFile' = client (Proxy @HackageAPI)

runClient :: Manager -> ClientM a -> IO (Either ClientError a)
runClient manager m = do
  url <- parseBaseUrl "https://hackage.haskell.org/"
  let env = mkClientEnv manager url
  runClientM m env

runClient' :: ClientM a -> IO (Either ClientError a)
runClient' m = newTlsManager >>= flip runClient m
