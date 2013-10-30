{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
--
module Site (app) where

import Data.Char (ord)
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Applicative
import Data.ByteString (ByteString)
import Snap.Core
import Snap.Snaplet

-- import Data.WAVE
import Data.Int
import Data.Monoid
import Data.Word
import Data.String
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
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


values = sineTable 220

-- value :: Float
-- value = 0

-- TODO 16 bit pcm

-- Taken from https://gist.github.com/andreasjansson/1428176
infWavHeader :: [Word8]
infWavHeader = [
    0x52, 0x49, 0x46, 0x46, 0x24, 0x40, 0x00, 0x00, 0x57, 0x41, 0x56, 0x45, 0x66, 0x6d, 0x74, 0x20,
    0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x02, 0x00, 0x44, 0xac, 0x00, 0x00, 0x10, 0xb1, 0x02, 0x00,
    0x04, 0x00, 0x10, 0x00, 0x64, 0x61, 0x74, 0x61, 0xff, 0xff, 0xff, 0xff]


writeForever :: Double -> Snap ()
writeForever freq = do
    modifyResponse $ addHeader "Content-Type" "audio/wave"
    modifyResponse $ addHeader "Connection" "Keep-Alive"
    modifyResponse $ setBufferingMode False

    modifyResponse $ setResponseBody $ EL.unfoldM (\s -> let
        d1 = if (s == 0) then Builder.fromStorables infWavHeader else mempty
        d2 = Builder.fromStorables (sineTable $ realToFrac freq)
        d = d1 <> d2
        in return (if {-s > (44100*2)-} False then Nothing else Just (d, s + 1))) 0

   -- modifyResponse $ setResponseBody $ EL.unfoldM (\s -> do
   --     return (Just (Builder.fromStorables values, s))) ()

serverError :: Snap ()
serverError = do
   modifyResponse $ setResponseStatus 500 "Internal Server Error"
   writeBS "This is an error"
   r <- getResponse
   finishWith r

sines' :: Snap ()
sines' = pathArg $ \subpath -> do
    let x = read (checkForWavSuffix (T.unpack subpath)) :: Double
    -- writeBS $ "This is sine " <> BS8.pack (show x)
    writeForever x

checkForWavSuffix :: String -> String
checkForWavSuffix s = if L.isSuffixOf ".wav" s then dropAtEnd 4 s else s 

inReverse :: ([a] -> [b]) -> [a] -> [b]
inReverse f = reverse . f . reverse

takeAtEnd :: Int -> [a] -> [a]
takeAtEnd n = inReverse (take n)

dropAtEnd :: Int -> [a] -> [a]
dropAtEnd n = inReverse (drop n)

wrapHtmlBody x = "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"/></head><body>" <> x <> "</body></html>\n"

index :: Snap ()
index = writeText $ wrapHtmlBody "<h1>Index!</h1>"

audio' :: Double -> Snap ()
audio' freq = writeText $ wrapHtmlBody $ ""
    <> "<h1>Audio!</h1>\n"
    <> "\n"
    <> "<audio controls>\n"
    <> "  <source src=\"sines/" <> fromString (show (round freq)) <> ".wav\" type=\"audio/wav\">\n"
    <> "Your browser does not support the audio element.\n"
    <> "</audio>\n"

audio :: Snap ()
audio = pathArg $ \subpath -> let
    x = read (checkForWavSuffix (T.unpack subpath)) :: Double
    in audio' x 

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
         --   ("/login",    with auth handleLoginSubmit)
         -- , ("/logout",   with auth handleLogout)
         -- , ("/new_user", with auth handleNewUser)
         -- , ("",          serveDirectory "static")

         ("/my-error", liftSnap serverError),
         ("/forever", liftSnap (writeForever 440)),
         ("/sines", liftSnap sines'), -- only responds /sines/FREQ.wav
         ("/", liftSnap audio)
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

