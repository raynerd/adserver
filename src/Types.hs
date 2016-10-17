{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

-- import           Control.Lens hiding (element)
import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Time.Calendar
import           Data.Maybe (fromMaybe)
import           Data.DateTime
import           Text.Read (readMaybe)
import           GHC.Generics

data Ad = Ad
  { adId         :: Integer
  , adInterest   :: Interest
  , adStartDate  :: DateTime
  , adEndDate    :: DateTime
  , adContent    :: T.Text
  , adMaxViews   :: Maybe Integer
  , adViewsCount :: Integer
  , adCountry    :: Maybe Country
  , adLanguage   :: Language }
  deriving (Generic, Show)

data AdLimit = AdLimit
  { limitAdId      :: Integer
  , limitMaxViews  :: Integer }
  deriving Show

data Channel = Channel
  { channelId        :: Integer
  , channelName      :: T.Text
  , channelType      :: ChannelType
  , channelCountry   :: Country
  , channelLimits    :: [AdLimit]
  , channelInterests :: [Interest] }

newtype ChannelType = ChannelType { unChannelType :: T.Text }
  deriving Show

data Country = Country
  { countryName :: T.Text
  , countryLanguages :: [Language] }
  deriving (Generic, Show)

newtype Language = Language { unLanguage :: T.Text }
  deriving (Eq, Show)

newtype Interest = Interest { unInterest :: T.Text }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
data AdRequest = AdRequest
  { reqChannelId   :: !Integer
  , reqAdId        :: !(Maybe Integer)
  , reqCountryName :: !(Maybe T.Text)
  , reqLanguage    :: !(Maybe Language)
  , reqInterests   :: ![Interest]
  } deriving(Show, Generic)

instance ToJSON Ad

instance ToJSON Country

instance ToJSON Language where
  toJSON (Language language) = String language

instance ToJSON Interest where
  toJSON = String . unInterest
-- makeLenses ''Ad
-- makeLenses ''Channel
-- makeLenses ''Country
-- makeLenses ''AdLimit
