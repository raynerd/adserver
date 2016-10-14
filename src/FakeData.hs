{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE OverloadedStrings #-}

module FakeData where

import Types

germany = Country "Germany" [Language "German"]
canada =  Country "Canada"  [Language "English", Language "French"]
usa    =  Country "USA"     [Language "English"]
