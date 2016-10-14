{-# LANGUAGE OverloadedStrings #-}

module ApiHandlers
  (handleAdRequest) where

import qualified Data.Aeson as A
import           Control.Applicative
import           Control.Monad ((>>=), (>>))
-- import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Map.Syntax ((##))
import qualified Data.Text as T
import           Data.Text.Encoding
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Heist.Interpreted as I
import           Control.Lens hiding (element)
------------------------------------------------------------------------------
import           Application
import           Types

import Debug.Trace

handleAdRequest :: Handler App (AuthManager App) ()
handleAdRequest = do
  mRequest <- adFromRequest
  case mRequest of
    Nothing ->
      finishEarly 400 Nothing
    Just request ->
      writeLBS . A.encode $ request

adFromRequest :: Handler App (AuthManager App) (Maybe AdRequest)
adFromRequest = do
  mChannelId <- getParam "channelId" >>= readInteger
  -- mInt <- readInteger mChannelId
  mInterests <- getParam "interest"
  let interests = maybe [] interestsFromRequest mInterests
  case mChannelId of
    Nothing -> return Nothing
    Just channelId' -> do
      mAdId <- getParam "ad-id" >>= readInteger
      mLanguage <- getParam "language"
      mCountry <- getParam "country"
      case (mAdId, mLanguage, mCountry) of
        (Nothing, Nothing, Nothing) -> return Nothing
        (Just _, _, _) ->
          return $ Just AdRequest {
                      reqChannelId = channelId'
                    , reqAdId      = mAdId
                    , reqCountry   = Nothing
                    , reqLanguage  = Nothing
                    , reqInterests = interests }
        (Nothing, Just _, Just _) ->
          return $ Just AdRequest {
                      reqChannelId = channelId'
                    , reqAdId      = Nothing
                    , reqCountry   = decodeUtf8 <$> mCountry
                    , reqLanguage  = decodeUtf8 <$> mLanguage
                    , reqInterests = interests }
        _ -> return Nothing

  where
    interestsFromRequest interests = Interest . decodeUtf8 <$> BS8.words interests
    readInteger mBS =
      case BS8.readInteger <$> mBS of
        Just Nothing -> return Nothing
        Just iValue -> return $ fst <$> iValue
        _ -> return Nothing

finishEarly :: Int -> Maybe BS8.ByteString -> Handler App (AuthManager App) ()
finishEarly code (Just description) = do
  modifyResponse $ setResponseStatus code description
  getResponse >>= finishWith
finishEarly code Nothing = do
  modifyResponse $ setResponseCode code
  getResponse >>= finishWith
