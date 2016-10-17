{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ApiHandlers
  (handleAdRequest) where

import qualified Data.Aeson as A
import           Control.Applicative
import           Control.Monad ((>>=), (>>))
import           Control.Monad.IO.Class (liftIO)
-- import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.DateTime
import           Data.Map.Syntax ((##))
import           Data.Maybe (listToMaybe, isNothing)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text.Encoding
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Random
import qualified Heist.Interpreted as I
import           Control.Lens hiding (element)
------------------------------------------------------------------------------
import           Application
import           Types
import           FakeData

import Debug.Trace

handleAdRequest :: Handler App (AuthManager App) ()
handleAdRequest = do
  mRequest <- adFromRequest
  case mRequest of
    Nothing ->
      finishEarly 400 Nothing
    Just request -> do
      ad <- liftIO $ getAd request
      maybe (finishEarly 404 $ Just "No ad found") writeJSON ad
      return ()
      -- writeLBS . A.encode $ request
  where
    writeJSON ad = do
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeLBS . A.encode $ ad

adFromRequest :: Handler App (AuthManager App) (Maybe AdRequest)
adFromRequest = do
  mChannelId <- getParam "channelId" >>= readInteger
  mInterests <- getParam "interest"
  let interests = maybe [] interestsFromRequest mInterests
  traceShowM interests
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
                      reqChannelId   = channelId'
                    , reqAdId        = mAdId
                    , reqCountryName = Nothing
                    , reqLanguage    = Nothing
                    , reqInterests   = interests }
        (Nothing, Just _, Just _) ->
          return $ Just AdRequest {
                      reqChannelId   = channelId'
                    , reqAdId        = Nothing
                    , reqCountryName = decodeUtf8 <$> mCountry
                    , reqLanguage    = Language . decodeUtf8 <$> mLanguage
                    , reqInterests   = interests }
        _ -> return Nothing

  where
    interestsFromRequest interests = Interest . decodeUtf8 <$> BS8.words interests
    readInteger mBS =
      case BS8.readInteger <$> mBS of
        Just Nothing -> return Nothing
        Just iValue -> return $ fst <$> iValue
        _ -> return Nothing

-- | We need to wrap this and subsecuent functions into IO Monad because we should
--   return a random ad and due to Haskell is a referentially transparent language
--   a pure function always returns the same value for the same parameters.
getAd :: AdRequest -> IO (Maybe Ad)
getAd adRequest@AdRequest{..} =
  if channelExist
    then maybe (lookupByLocation adRequest) lookupById reqAdId
    else return Nothing
  where
    lookupById adId' = return $ listToMaybe $ filter (\a -> adId a == adId') ads
    channelExist = reqChannelId `elem` map channelId channels

lookupByLocation :: AdRequest -> IO (Maybe Ad)
lookupByLocation AdRequest{..} = do
  -- In case we have both country and language set we can lookup a suitable ad
  case (reqCountryName, reqLanguage) of
    (Just _, Just language) -> do
      currentTime <- getCurrentTime
      let byCountryAds = filter (isFromCountry True reqCountryName) ads
      -- We could filter once but this way looks clearer
      let byLanguage = filter (isWithLanguage language) (traceShowId byCountryAds)
      let byInterests = filter haveInterest byLanguage
      let byDate = filter (inTime currentTime) byInterests
      if null byDate
        then return Nothing
        else do
          rndd <- genRandomIndex 0 (length byDate - 1)
          return $ Just $ byDate !! rndd
    _ -> return Nothing
  where
    isFromCountry allowNoCountry matchCountry Ad{ adCountry } =
      (isNothing adCountry && allowNoCountry)
      || matchCountry == fmap countryName adCountry
    isWithLanguage matchLanguage Ad{ adLanguage } =
      matchLanguage == adLanguage
    haveInterest Ad{ adInterest } =
      adInterest `elem` traceShowId (reqInterests <> interestsByChannelId reqChannelId)
    inTime currentTime Ad{ adStartDate, adEndDate } = do
      (currentTime >= adStartDate) && (currentTime <= adEndDate)

interestsByChannelId :: Integer -> [Interest]
interestsByChannelId chId = maybe mempty channelInterests $
                              listToMaybe $ filter (\ch -> channelId ch == chId) channels


genRandomIndex :: Int -> Int -> IO Int
genRandomIndex fromIndex toIndex = getStdRandom (randomR (fromIndex, toIndex))

finishEarly :: Int -> Maybe BS8.ByteString -> Handler App (AuthManager App) ()
finishEarly code mDescription = do
  maybe (modifyResponse $ setResponseCode code) withDescription mDescription
  getResponse >>= finishWith
  where
    withDescription description = modifyResponse $ setResponseStatus code description
