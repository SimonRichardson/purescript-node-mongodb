module Database.Mongo.ConnectionInfo where

import Data.Maybe

-- | The type for Mongo connection options
type ConnectionInfo =
  { host     :: Host
  , port     :: Maybe Port
  , db       :: Maybe Db
  , user     :: Maybe User
  , password :: Maybe Password
  }

-- | Standard default options for connecting to mongodb.
defaultOptions :: ConnectionInfo
defaultOptions =
  { host     : "127.0.0.1"
  , port     : Just 27017
  , db       : Just "test"
  , user     : Nothing
  , password : Nothing
  }

-- | Type alias for URI connections to aid readability of types.
type Uri      = String
type Host     = String
type Port     = Number
type Db       = String
type User     = String
type Password = String

dialUri :: ConnectionInfo -> Uri
dialUri info = "mongodb://" ++ 
    auth info ++ 
    uri info ++
    port info ++
    db info
  
  where
    auth :: ConnectionInfo -> String
    auth info = do
      case info.user of
        Nothing -> ""
        Just u  -> (showUser u) ++ (maybe "" (\x -> ":" ++ showPassword x) info.password) ++ "@"
    uri :: ConnectionInfo -> String
    uri info = showHost info.host
    port :: ConnectionInfo -> String
    port info = maybe "" (\x -> ":" ++ showPort x) info.port
    db :: ConnectionInfo -> String
    db info = maybe "" (\x -> "/" ++ showDb x) info.db

showHost :: Host -> String
showHost h = h

showPort :: Port -> String
showPort p = show p

showDb :: Db -> String
showDb d = d

showUser :: User -> String
showUser u = u

showPassword :: Password -> String
showPassword p = p