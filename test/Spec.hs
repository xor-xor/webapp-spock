{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Internal (ByteString)
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleBody, simpleHeaders))
import Test.Hspec
import Test.Hspec.Wai

import App (appToTest)

testUser :: String
testUser = "test_user"

testPassword :: String
testPassword = "test_password"

loginTestUser :: WaiSession SResponse
loginTestUser = postHtmlForm "/login" [ ("username", testUser)
                                      , ("password", testPassword)
                                      ]

getCookie :: SResponse -> Maybe ByteString
getCookie = lookup "Set-Cookie" . simpleHeaders

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (appToTest testUser testPassword) $ do
  describe "GET /health" $ do
    it "responds with 200" $
      get "/health" `shouldRespondWith` 200

    it "responds with 'OK'" $
      get "/health" `shouldRespondWith` "OK"

    it "responds with 200 'OK'" $
      get "/health" `shouldRespondWith` "OK" { matchStatus = 200 }

  describe "GET /" $
    it "responds with 302" $
       get "/" `shouldRespondWith` 302

  describe "GET /login" $
    it "contains 'Please log in'" $ do
      r <- request methodGet "/login" [] ""
      liftIO $
        unpack (simpleBody r) `shouldContain` unpack "Please log in"

  describe "GET /employees" $ do
    it "responds with 200" $ do
      -- what about moving this getCookie stuff to the enclosing monad..?
      (Just c) <- getCookie <$> loginTestUser
      request methodGet "/employees" [("Cookie", c)] "" `shouldRespondWith` 200

    it "contains 'logged in as:'" $ do
      (Just c) <- getCookie <$> loginTestUser
      r <- request methodGet "/employees" [("Cookie", c)] ""
      liftIO $
        -- test below can be also written as:
        -- simpleStatus r `shouldBe` status200
        unpack (simpleBody r) `shouldContain` unpack "logged in as:"
