{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Types where

import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Time.Calendar
import           Data.Maybe (fromMaybe)
import           Data.DateTime
import           Text.Read (readMaybe)
import           GHC.Generics

data Ad = Ad
  { adId         :: !Integer
  , adStartDate  :: !DateTime
  , adEndDate    :: !DateTime
  , adContent    :: !T.Text
  , adMaxViews   :: !(Maybe Integer)
  , adViewsCount :: !Integer
  , adCountry    :: !(Maybe Integer) }

data AdLimit = AdLimit
  { limitId        :: !Integer
  , limitChannelId :: !Integer
  , limitAdId      :: !Integer
  , limitMaxViews  :: !Integer }

data Channel = Channel
  { channelId        :: !Integer
  , channelName      :: !T.Text
  , channelType      :: !T.Text
  , channelCountryId :: !Integer }

data Country = Country
  { countryId   :: !Integer
  , countryName :: !T.Text }

data Language = Language
  { languageId   :: !Integer
  , languageName :: !T.Text }

data CountryLanguage = CountryLanguage
  { clCountryId  :: !Integer
  , clLanguageId :: !Integer }

data Interest = Interest
 { interestId   :: !Integer
 , interestName :: !T.Text }
  deriving (Show, Generic)

data ChannelInterest = ChannelInterest
  { ciChannelId  :: !Integer
  , ciInterestId :: !Interest }

--------------------------------------------------------------------------------
data AdRequest = AdRequest
  { reqChannelId :: !Integer
  , reqAdId      :: !(Maybe Integer)
  , reqCountry   :: !(Maybe T.Text)
  , reqLanguage  :: !(Maybe T.Text)
  , reqInterests :: ![Interest]
  } deriving(Show, Generic)
