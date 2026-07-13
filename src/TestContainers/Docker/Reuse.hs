{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestContainers.Docker.Reuse
  ( -- * Container identity
    ContainerIdentity (..),
    containerIdentityHash,
    hashCopiedFiles,

    -- * Labels
    reuseHashLabel,
    reuseDiscoverabilityLabel,
  )
where

import Crypto.Hash (SHA1 (SHA1), hashWith)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (toStrict)
import Data.List (sort)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

-- | The subset of a 'TestContainers.Docker.ContainerRequest''s fields that
-- determine whether two requests would create an identical container.
-- Mirrors the fields fed into @docker create@, excluding anything that
-- doesn't affect the container's identity (naming, reaper registration,
-- readiness checks, log following).
--
-- @since 0.5.5.0
data ContainerIdentity = ContainerIdentity
  { image :: Text,
    cmd :: Maybe [Text],
    env :: [(Text, Text)],
    exposedPorts :: [Text],
    volumeMounts :: [(Text, Text)],
    network :: Maybe Text,
    networkAlias :: Maybe Text,
    cpus :: Maybe Text,
    memory :: Maybe Text,
    links :: [Text],
    workDirectory :: Maybe Text,
    labels :: [(Text, Text)],
    -- | Hash of the paths and contents of any files staged to be copied
    -- into the container, as computed by 'hashCopiedFiles'.
    copiedFilesHash :: Text
  }

-- | List-valued fields are sorted before encoding so that two requests built
-- with the same content in a different order hash identically.
instance ToJSON ContainerIdentity where
  toJSON ContainerIdentity {..} =
    object
      [ "image" .= image,
        "cmd" .= cmd,
        "env" .= sort env,
        "exposedPorts" .= sort exposedPorts,
        "volumeMounts" .= sort volumeMounts,
        "network" .= network,
        "networkAlias" .= networkAlias,
        "cpus" .= cpus,
        "memory" .= memory,
        "links" .= links,
        "workDirectory" .= workDirectory,
        "labels" .= sort labels,
        "copiedFilesHash" .= copiedFilesHash
      ]

-- | Hashes the create-relevant fields of a container request into a stable
-- identifier. Used to look up a previously-created, still-running container
-- to reuse instead of creating a new one.
--
-- @since 0.5.5.0
containerIdentityHash :: ContainerIdentity -> Text
containerIdentityHash identity =
  pack (show (hashWith SHA1 (toStrict (Aeson.encode identity))))

-- | Hashes the paths and contents of the files staged to be copied into the
-- container, so that requests which differ only in copied files don't
-- collide on the same reuse hash.
--
-- @since 0.5.5.0
hashCopiedFiles :: [(FilePath, FilePath)] -> IO Text
hashCopiedFiles files = do
  chunks <-
    traverse
      ( \(hostFile, containerFile) -> do
          contents <- ByteString.readFile hostFile
          pure (encodeUtf8 (pack containerFile) <> contents)
      )
      files
  pure (pack (show (hashWith SHA1 (ByteString.concat chunks))))

-- | Label stamped on a reused container recording the hash of the request
-- that created it. Looked up on subsequent runs via @docker ps --filter
-- label=...@ to find a container to adopt.
--
-- @since 0.5.5.0
reuseHashLabel :: Text
reuseHashLabel = "org.testcontainers.hash"

-- | Fixed, non-opaque label stamped on every reused container purely so a
-- human can find every reused container on their machine with @docker ps
-- --filter label=...@ without knowing any hash values.
--
-- @since 0.5.5.0
reuseDiscoverabilityLabel :: (Text, Text)
reuseDiscoverabilityLabel = ("org.testcontainers.hs.reuse", "true")
