{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           ApiHandlers
import           TestingHandlers
------------------------------------------------------------------------------

-- | The application's routes. Here we are listing 3 main routes for the
-- application, the first one handles the API response, the second one for
-- the testing page and the last one for needed resources.
routes :: [(ByteString, Handler App App ())]
routes = [ ("api/:channelId", with auth handleAdRequest)
         , ("testing",        with auth handleTesting)
         , ("/static/",       serveDirectory "static") ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "AdServer Snaplet." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    let sessionExpiresIn = 24*60*60
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just sessionExpiresIn)
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a
