{-# LANGUAGE OverloadedStrings #-}

module TestContainers.Docker.ReuseSpec (main, spec_reuse) where

import Test.Hspec
import TestContainers.Docker.Reuse
  ( ContainerIdentity (..),
    containerIdentityHash,
    hashCopiedFiles,
  )

baseIdentity :: ContainerIdentity
baseIdentity =
  ContainerIdentity
    { image = "postgres:17-alpine",
      cmd = Nothing,
      env = [("A", "1"), ("B", "2")],
      exposedPorts = ["5432/tcp"],
      volumeMounts = [],
      network = Nothing,
      networkAlias = Nothing,
      cpus = Nothing,
      memory = Nothing,
      links = [],
      workDirectory = Nothing,
      labels = [],
      copiedFilesHash = ""
    }

main :: IO ()
main = hspec spec_reuse

spec_reuse :: Spec
spec_reuse =
  describe "TestContainers.Docker.Reuse" $ do
    describe "containerIdentityHash" $ do
      it "is stable regardless of the order of list-valued fields" $
        containerIdentityHash baseIdentity {env = [("B", "2"), ("A", "1")]}
          `shouldBe` containerIdentityHash baseIdentity

      it "differs when the image tag differs" $
        containerIdentityHash baseIdentity {image = "postgres:16-alpine"}
          `shouldNotBe` containerIdentityHash baseIdentity

      it "differs when the env differs" $
        containerIdentityHash baseIdentity {env = [("A", "1")]}
          `shouldNotBe` containerIdentityHash baseIdentity

      it "differs when the copied files hash differs" $
        containerIdentityHash baseIdentity {copiedFilesHash = "deadbeef"}
          `shouldNotBe` containerIdentityHash baseIdentity

    describe "hashCopiedFiles" $ do
      it "is deterministic for the same paths and contents" $ do
        a <- hashCopiedFiles [("test/data/init-script.sql", "/docker-entrypoint-initdb.d/")]
        b <- hashCopiedFiles [("test/data/init-script.sql", "/docker-entrypoint-initdb.d/")]
        a `shouldBe` b

      it "differs when the destination path in the container differs" $ do
        a <- hashCopiedFiles [("test/data/init-script.sql", "/docker-entrypoint-initdb.d/")]
        b <- hashCopiedFiles [("test/data/init-script.sql", "/some/other/dir/")]
        a `shouldNotBe` b

      it "differs from the hash of an empty file list" $ do
        withFiles <- hashCopiedFiles [("test/data/init-script.sql", "/docker-entrypoint-initdb.d/")]
        withoutFiles <- hashCopiedFiles []
        withFiles `shouldNotBe` withoutFiles
