
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Sound.Generate where

import Control.Lens
import Data.Int
import Data.String
import Data.Monoid
import qualified Codec.Digest.SHA as SHA -- TODO move
import qualified Data.ByteString.Char8 as BS -- TODO move

type Time = Int64 -- milliseconds since unix epoch

-- TODO
type Expression = ()    


-- TODO use real hash tags
newtype TrailRef  = TrailRef { getTrailRef :: String }
    deriving (Eq, Ord, Show, IsString)
    
newtype StreamRef = StreamRef { getStreamRef :: String }
    deriving (Eq, Ord, Show, IsString)

newtype SourceRef = SourceRef { getSourceRef :: String }
    deriving (Eq, Ord, Show, IsString)

hashTrail :: Trail -> TrailRef
hashStream :: Stream -> StreamRef
hashSource :: Source -> SourceRef
hashTrail = TrailRef . hash
hashStream = StreamRef . hash
hashSource = SourceRef . hash


data Site = Site {
    _users :: [User]
    }
    deriving (Eq, Ord, Show)
instance Monoid Site where
    mempty = Site mempty
    Site x `mappend` Site y = Site (x <> y)


    
data User = User {
    _name :: String,
    _email :: String,
    _trails :: [(TrailRef, Trail)],
    _streams :: [(StreamRef, Stream)],
    _sources :: [(SourceRef, Source)]
    }
    deriving (Eq, Ord, Show)
    
data Trail = Trail {
    _tstreams :: [StreamRef] -- last element is latest version
    }
    deriving (Eq, Ord, Show)

data Stream = Stream {
    _expression :: Expression,
    _channels :: Int,
    _sname :: String
    }    
    deriving (Eq, Ord, Show)

data Source
    = LiveSource String String -- name descr
    | SoundCloudSource String -- soundcloud URL
    deriving (Eq, Ord, Show)





hash :: Show a => a -> String
hash = SHA.showBSasHex . SHA.hash SHA.SHA256 . BS.pack . show

-- TODO exact type of this function
renderStream :: Site -> Time -> Stream -> [Double]
renderStream = undefined


makeLenses ''Site
-- makeLenses ''Site
-- makeLenses ''Site
makeLenses ''Source
makeLenses ''Stream
makeLenses ''Trail
makeLenses ''User



testSite :: Site
testSite = 
    Site [User "hans" "h@h.c" [] [] [],
          User "david" "h@h.c" [] [] []]
