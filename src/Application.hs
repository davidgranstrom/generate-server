{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Sound.Generate
-- import Snap.Snaplet.Heist
-- import Snap.Snaplet.Auth
-- import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { 
    -- _site :: IORef Site
    }

makeLenses ''App

type AppHandler = Handler App App


