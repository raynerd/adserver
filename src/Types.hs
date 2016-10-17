{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

------------------------------------------------------------------------------
import qualified Data.Text as T
import           Data.Aeson
import           Data.DateTime
import           GHC.Generics
------------------------------------------------------------------------------

-- | Due we are sending a JSON as answer we need to encode this type and the
-- simplest way is to deriving from Generic and instance ToJSON
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

-- | By using newtypes to wrap data members we avoid confussion at
-- development time
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
