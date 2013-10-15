
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sound.Generate where

import Data.Int
import Data.String
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


data Site = Site {
    _users :: [User]
    }
    deriving (Eq, Ord, Show)
    
data User = User {
    _name :: String,
    _email :: String,
    _trails :: [Trail],
    _streams :: [Stream],
    _sources :: [Source]
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

