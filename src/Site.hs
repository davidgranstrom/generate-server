{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Data.Char (ord)
import           Control.Monad.Trans
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Applicative
import           Data.ByteString (ByteString)
-- import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet

import Data.Int
import Data.Word
import qualified Data.Enumerator.List as EL
import qualified Blaze.ByteString.Builder as Builder
-- import           Snap.Snaplet.Auth
-- import           Snap.Snaplet.Auth.Backends.JsonFile
-- import           Snap.Snaplet.Heist
-- import           Snap.Snaplet.Session.Backends.CookieSession
-- import           Snap.Util.FileServe
-- import           Heist
-- import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
{-
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"      -}



------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ 
         --   ("/login",    with auth handleLoginSubmit)
         -- , ("/logout",   with auth handleLogout)
         -- , ("/new_user", with auth handleNewUser)
         -- , ("",          serveDirectory "static")
         
         ("/my-error", liftSnap serverError),
         ("/forever", liftSnap writeForever),
         ("/forever.wav", liftSnap writeForever)

         ]


-- values :: [Word8]
-- values = map (fromIntegral . ord) "hans\n"

-- TODO dither
floatToPcm :: Float -> Int16
floatToPcm = floor . corr . (* 2^15)
    where
        corr x | x > 32767  = 32767
               | x < -32768 = (-32768)
               | otherwise  = x
    
values :: [Int16]
values = fmap (floatToPcm . (* 0.1) . sin . (/200)) [0,1..199]

-- value :: Float
-- value = 0

-- TODO 16 bit pcm

writeForever :: Snap ()
writeForever = do
   modifyResponse $ addHeader "Content-Type" "audio/wave"
   modifyResponse $ addHeader "Connection" "Keep-Alive"
   modifyResponse $ setBufferingMode False
   modifyResponse $ setResponseBody $ EL.unfoldM (\s -> do
       -- threadDelay 500000
       return (Just (Builder.fromStorables values, s))) ()

   -- replicateM_ 3 $ do
   --     liftIO $ threadDelay 500000
   --     writeText "For ever\n"
   -- r <- getResponse
   -- finishWith r


serverError :: Snap ()
serverError = do
   modifyResponse $ setResponseStatus 500 "Internal Server Error"
   writeBS "This is an error"
   r <- getResponse
   finishWith r

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet 
        "generate-server" 
        "The generate server."
        Nothing 
        $ do

    -- h <- nestSnaplet "" heist $ heistInit "templates"
    -- s <- nestSnaplet "sess" sess $
    --        initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    -- 
    -- -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- -- doesn't require any kind of database server to run.  In practice,
    -- -- you'll probably want to change this to a more robust auth backend.
    -- a <- nestSnaplet "auth" auth $
    --        initJsonFileAuthManager defAuthSettings sess "users.json"

    addRoutes routes
    -- addAuthSplices h auth
    return $ App

