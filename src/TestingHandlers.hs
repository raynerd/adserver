{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module TestingHandlers
  (handleTesting) where

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
import qualified Heist.Interpreted as I
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

handleTesting :: Handler App (AuthManager App) ()
handleTesting =
  renderWithSplices "testing/test_ui" $ do
    "availableAdIds"     ## splicesForAdIds
    "availableChannels"  ## splicesForChannels
    "availableCountries" ## splicesForCountries
    "availableLanguages" ## splicesForLanguages
    "availableInterests" ## splicesForInterests

splicesForAdIds :: SnapletISplice App
splicesForAdIds =
  I.textSplice $ commaSeparatedValues adId integerToText ads

splicesForChannels :: SnapletISplice App
splicesForChannels =
  I.textSplice $ commaSeparatedValues channelId integerToText channels

splicesForCountries :: SnapletISplice App
splicesForCountries =
  I.textSplice $ commaSeparatedValues countryName id countries

splicesForLanguages :: SnapletISplice App
splicesForLanguages =
  I.textSplice $ commaSeparatedValues unLanguage id languages

splicesForInterests :: SnapletISplice App
splicesForInterests =
  I.textSplice $ commaSeparatedValues unInterest id interests

commaSeparatedValues :: (a -> b) -> (b -> T.Text) -> [a] -> T.Text
commaSeparatedValues f g s =
  T.intercalate ", " $ map (g . f) s

integerToText :: Integer -> T.Text
integerToText = T.pack . show
