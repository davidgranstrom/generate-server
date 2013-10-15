{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
--
module Site (app) where

import Data.Char (ord)
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Applicative
import Data.ByteString (ByteString)
import Snap.Core
import Snap.Snaplet

import Data.Int
import Data.Word
import qualified Data.Enumerator.List as EL
import qualified Blaze.ByteString.Builder as Builder

import Application

-- values :: [Word8]
-- values = map (fromIntegral . ord) "hans\n"

-- TODO dither
floatToPcm :: Float -> Int16
floatToPcm = floor . corr . (* 2^15)
    where
        corr x | x > 32767  = 32767
               | x < -32768 = (-32768)
               | otherwise  = x

sineTable :: Float -> [Int16]
sineTable fq = fmap (floatToPcm . (* 0.1) . sin . (*tau) . (/len)) [0,1..len-1]
    where
        len = 44100/fq
        
tau = 2*pi


values = sineTable 880

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

serverError :: Snap ()
serverError = do
   modifyResponse $ setResponseStatus 500 "Internal Server Error"
   writeBS "This is an error"
   r <- getResponse
   finishWith r


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

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet
        "generate-server"
        "The generate server."
        Nothing
        $ do

    addRoutes routes
    return $ App

