{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Distribution.Hackage.Types where

import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Function
import Data.List.NonEmpty (toList)
import Data.Maybe
import Data.Proxy
import Data.Text (Text, unpack)
import Data.Time
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Parsec
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.API.ContentTypes
import Servant.Client
import Servant.Client.Core

data JSON0

instance Accept JSON0 where
  contentType _ = "application/json"

instance FromJSON a => MimeUnrender JSON0 a where
  mimeUnrender _ = eitherDecodeLenient

-----------------------------------------------------------

newtype Time = Time UTCTime
  deriving newtype (Show, Eq)

instance FromJSON Time where
  parseJSON = withText "time" $ \t ->
    case parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%T%Z")) (unpack t) of
      Just time -> pure $ Time time
      Nothing -> fail "failed to parse time"

data Version
  = Default
  | Version Text
  deriving (Show, Eq, Generic)

instance FromJSON Version where
  parseJSON = withText "version" $ pure . Version

data Package = Package
  { packageName :: Text,
    version :: Version
  }
  deriving (Show, Eq, Generic)

instance FromJSON Package where
  parseJSON = withObject "package" $ \o -> do
    packageName <- o .: "packageName"
    let version = Default
    pure $ Package {..}

instance ToHttpApiData Package where
  toUrlPiece Package {..}
    | Default <- version = packageName
    | Version ver <- version = packageName <> "-" <> ver

data Revision = Revision
  { time :: Time,
    user :: Text,
    number :: Int
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON)

data Versions = Versions
  { normal :: [Text],
    unpreferred :: [Text],
    deprecated :: [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Versions where
  parseJSON = withObject "versions" $ \o -> do
    let withDefault = fmap (fromMaybe [])
    normal <- withDefault (o .:? "normal-version")
    unpreferred <- withDefault (o .:? "unprefereed-version")
    deprecated <- withDefault (o .:? "deprecated-version")
    pure Versions {..}

data Cabal

instance Accept Cabal where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

instance MimeUnrender Cabal GenericPackageDescription where
  mimeUnrender _ f = r
    where
      res' = parseGenericPackageDescription $ toStrict f
      (_, res) = runParseResult res'
      showErrors es =
        "cabal file parse failed with the following errors:\n"
          <> unlines (map (showPError "") es)
      r = case res of
        Left (_, es') -> Left $ showErrors $ toList es'
        Right desc -> Right desc

data CabalFile

instance HasClient m api => HasClient m (CabalFile :> api) where
  type Client m (CabalFile :> api) = Package -> Client m api

  clientWithRoute pm Proxy req = \p@Package {..} ->
    clientWithRoute pm (Proxy @api) $
      req
        & appendToPath (toQueryParam p)
        & appendToPath (packageName <> ".cabal")

  hoistClientMonad pm Proxy f cl = hoistClientMonad pm (Proxy @api) f . cl

-----------------------------------------------------------
-- Servant API Types

type GetPackages = "packages" :> Get '[JSON0] [Package]

type GetVersions = "package" :> Capture "package" Package :> "preferred" :> Get '[JSON0] Versions

type GetRevisions = "package" :> Capture "package" Package :> "revisions" :> Get '[JSON0] [Revision]

type GetCabalFile = "package" :> CabalFile :> Get '[Cabal] GenericPackageDescription

type GetCabalFile' = "package" :> Capture "package" Package :> "revision" :> Capture "revision" Int :> Get '[Cabal] GenericPackageDescription

type HackageAPI =
  GetPackages
    :<|> GetVersions
    :<|> GetRevisions
    :<|> GetCabalFile
    :<|> GetCabalFile'
