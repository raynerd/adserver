{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE OverloadedStrings #-}

module FakeData where

------------------------------------------------------------------------------
import           Data.DateTime
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
------------------------------------------------------------------------------
import Types

-- Here we have the mock data we need for the application, we could have used
-- real data on a database but it would require more steps on deployment.
-- Also we could use the State Monad to update the view count but it can add
-- more complexity for just a testing data.

-- | Testing languages
german = Language "German"
english = Language "English"
french = Language "French"
spanish = Language "Spanish"

languages = [german, english, french, spanish]

-- | Testing countries
germany = Country "Germany" [german]
canada =  Country "Canada"  [english, french]
usa    =  Country "USA"     [english]
mexico =  Country "Spanish" [spanish]

countries = [germany, canada, usa, mexico]

-- | Testing interests
interest1 = Interest "Nature"
interest2 = Interest "Physics"
interest3 = Interest "Biology"
interest4 = Interest "Baseball"
interest5 = Interest "Football"
interest6 = Interest "Politics"
interest7 = Interest "Economics"
interest8 = Interest "Social"
interest9 = Interest "Movies"
interest10 = Interest "Trailers"
interest11 = Interest "Tickets"
interest12 = Interest "Concerts"
interest13 = Interest "Albums"
interest14 = Interest "Music instruments"

interests = [interest1,interest2,interest3,interest4,interest5,interest6,interest7,
             interest8,interest9,interest10,interest11,interest12,interest13,interest14 ]

-- | Testing ChannelTypes
chType1 = ChannelType "Science"
chType2 = ChannelType "Sports"
chType3 = ChannelType "News"
chType4 = ChannelType "Films"
chType5 = ChannelType "Music"

-- | Testing channels
ch1 = Channel 1 "ch1" chType1 canada [AdLimit 1 20, AdLimit 2 30] [interest1, interest2, interest3]
ch2 = Channel 2 "ch2" chType2 usa [AdLimit 3 25, AdLimit 4 35] [interest1, interest2, interest3]
ch3 = Channel 3 "ch3" chType3 germany [AdLimit 5 40, AdLimit 6 50, AdLimit 7 30] [interest6, interest7, interest8]
ch4 = Channel 4 "ch4" chType4 usa [AdLimit 8 20] [interest9, interest10, interest11]
ch5 = Channel 5 "ch5" chType5 mexico [] [interest12, interest13, interest14]

channels = [ch1, ch2, ch3, ch4, ch5]

-- | Testing dates      year  m  d
date1  = fromGregorian' 2015 11  1
date2  = fromGregorian' 2016  1  1
date3  = fromGregorian' 2016  1 15
date4  = fromGregorian' 2016  2  5
date5  = fromGregorian' 2016  5  1
date6  = fromGregorian' 2016  9  1
date7  = fromGregorian' 2016 10  1
date8  = fromGregorian' 2016 10 10
date9  = fromGregorian' 2016 10 29
date10 = fromGregorian' 2016 11  5
date11 = fromGregorian' 2016 11 11
date12 = fromGregorian' 2016 11 30
date13 = fromGregorian' 2016 12 10
date14 = fromGregorian' 2016 12 30
date15 = fromGregorian' 2017  2 28

-- | Testing Ads
--          id  interest  start    end  maxviews currentviews       country   language
ad1 = makeAd 1 interest1  date1 date2  (Just 60)           20 (Just canada)   "French"
ad2 = makeAd 2 interest1  date7 date10 (Just 60)            2 (Just canada)   "English"
ad3 = makeAd 3 interest2  date6 date9  (Just 50)            6 (Just canada)   "English"
ad4 = makeAd 4 interest6  date6 date9  (Just 30)            7 (Just germany)  "German"
ad5 = makeAd 5 interest12 date6 date9  (Just 30)            7 (Just germany)  "German"
ad6 = makeAd 6 interest13 date6 date7  (Just 30)            7 (Just germany)  "German"
ad7 = makeAd 7 interest13 date6 date9  (Just 30)            7 (Just germany)  "German"

ads = [ad1, ad2, ad3, ad4, ad5, ad6]

-- | Utilities functions
genAdContent :: Integer -> Interest -> Maybe Country -> T.Text
genAdContent adId' (Interest interest) mCountry =
  "Content for ad " <> (T.pack . show) adId'
    <> " with " <> interest <> " as interest from "
    <> countryToText
  where
    countryToText = fromMaybe "No Country" $ countryName <$> mCountry

makeAd :: Integer -> Interest -> DateTime -> DateTime -> Maybe Integer -> Integer -> Maybe Country -> T.Text -> Ad
makeAd adId' interest startDate endDate mMaxViews viewsCount mCountry language =
  Ad adId' interest startDate endDate (genAdContent adId' interest mCountry)
      mMaxViews viewsCount mCountry (Language language)
