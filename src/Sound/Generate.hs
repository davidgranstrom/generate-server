
module Sound.Generate where

import Data.Int

data Site = Site {
    _users :: [User]
    }
    
data User = User {
    _name :: String,
    _email :: String,
    _trails :: [Trail],
    _streams :: [Stream],
    _sources :: [Source]
    }
    
data Trail = Trail {
    _tstreams :: [StreamRef] -- last element is latest version
    }

data Stream = Stream {
    _expression :: Expression,
    _channels :: Int,
    _sname :: String
    }    

data Source
    = LiveSource String String -- name descr
    | SoundCloudSource String -- soundcloud URL

type Expression = ()    
type StreamRef = String
type Time = Int64 -- milliseconds since unix epoch


-- TODO exact type of this function
renderStream :: Site -> Time -> Stream -> [Double]
renderStream = undefined

