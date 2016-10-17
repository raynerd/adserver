{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module TestingHandlers
  (handleTesting) where

------------------------------------------------------------------------------
import           Data.Map.Syntax ((##))
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application
import           Types
import           FakeData
------------------------------------------------------------------------------

-- | This function serves the testing page, it prepares the hints for the user
-- in order to see which request parameters are available.
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

-- | This utility function returns a Text with all the array element separated
-- by comma 
commaSeparatedValues :: (a -> b) -> (b -> T.Text) -> [a] -> T.Text
commaSeparatedValues f g s =
  T.intercalate ", " $ map (g . f) s

integerToText :: Integer -> T.Text
integerToText = T.pack . show
