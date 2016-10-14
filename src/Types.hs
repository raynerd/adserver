{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens hiding (element)
import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Time.Calendar
import           Data.Maybe (fromMaybe)
import           Data.DateTime
import           Text.Read (readMaybe)
import           GHC.Generics

data Ad = Ad
  { _adId         :: Integer
  , _adStartDate  :: DateTime
  , _adEndDate    :: DateTime
  , _adContent    :: T.Text
  , _adMaxViews   :: Maybe Integer
  , _adViewsCount :: Integer
  , _adCountry    :: Maybe Country }

data AdLimit = AdLimit
  { _limitAdId      :: Integer
  , _limitMaxViews  :: Integer }
  deriving Show

data Channel = Channel
  { _channelId        :: Integer
  , _channelName      :: T.Text
  , _channelType      :: T.Text
  , _channelCountry   :: Country
  , _channelLimits    :: [AdLimit]
  , _channelInterests :: [Interest] }

data Country = Country
  { _countryName :: T.Text
  , _countryLanguages :: [Language] }
  deriving Show

newtype Language = Language { unLanguage :: T.Text }
  deriving Show

newtype Interest = Interest { unInterest :: T.Text }
  deriving (Show, Generic)

instance ToJSON Interest

--------------------------------------------------------------------------------
data AdRequest = AdRequest
  { reqChannelId :: !Integer
  , reqAdId      :: !(Maybe Integer)
  , reqCountry   :: !(Maybe T.Text)
  , reqLanguage  :: !(Maybe T.Text)
  , reqInterests :: ![Interest]
  } deriving(Show, Generic)

instance ToJSON AdRequest

makeLenses ''Ad
makeLenses ''Channel
makeLenses ''Country
makeLenses ''AdLimit
