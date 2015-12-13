{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Data.ByteString.Lazy.Char8 (unpack)
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleBody))
import Test.Hspec
import Test.Hspec.Wai

import Main (appToTest)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with appToTest $ do
  describe "GET /health" $ do
    it "responds with 200" $
      get "/health" `shouldRespondWith` 200

    it "responds with 'OK'" $
      get "/health" `shouldRespondWith` "OK"

    it "responds with 200 'OK'" $
      get "/health" `shouldRespondWith` "OK" { matchStatus = 200 }

  describe "GET /employees" $ do
    it "responds with 200" $
      get "/employees" `shouldRespondWith` 200

    it "contains 'logged in as:'" $ do
      r <- request methodGet "/employees" [] ""
      liftIO $
        -- test below can be also written as:
        -- simpleStatus r `shouldBe` status200
        unpack (simpleBody r) `shouldContain` unpack "logged in as:"
