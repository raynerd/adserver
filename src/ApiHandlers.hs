{-# LANGUAGE OverloadedStrings #-}

module ApiHandlers
  (handleAdRequest) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Map.Syntax ((##))
import qualified Data.Text as T
import           Snap.Core
-- import           Snap.Extras.Coreutils
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application

handleAdRequest :: Handler App (AuthManager App) ()
handleAdRequest = do
  -- mChannelId <- getParam' "channelId"
  writeLBS "testing handler"
