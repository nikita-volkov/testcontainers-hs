{-# LANGUAGE OverloadedStrings #-}

module TestContainers.Docker.ReuseIntegrationSpec (test_reuse) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestContainers.Docker (containerId)
import TestContainers.Monad (runTestContainer)
import TestContainers.Tasty

-- | Exercises the reuse mechanism against a real Docker daemon: a second
-- 'run' of an identical, 'withReuse'-marked request must adopt the container
-- created by the first rather than starting a new one, and a request that
-- differs must not be adopted. Reused containers aren't registered with the
-- reaper, so each case removes its own container explicitly at the end.
test_reuse :: TestTree
test_reuse =
  testGroup
    "Container reuse"
    [ testCase "second run of an identical request adopts the first container" $ do
        config <- determineConfig
        (firstId, secondId) <- runTestContainer config $ do
          let request = containerRequest redis & withReuse
          first <- run request
          second <- run request
          rm second
          pure (containerId first, containerId second)
        firstId @?= secondId,
      testCase "a request that differs is not adopted" $ do
        config <- determineConfig
        (firstId, secondId) <- runTestContainer config $ do
          first <- run (containerRequest redis & withReuse)
          second <- run (containerRequest redis & withReuse & setEnv [("FOO", "bar")])
          rm first
          rm second
          pure (containerId first, containerId second)
        assertBool "expected different container ids" (firstId /= secondId)
    ]
