module Database.Mongo.Updatable 
  ( Updatable(..)
  , printUpdatable
  ) where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Core (Json(), JString(), toString)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Maybe

import Database.Mongo.Bson.BsonValue (printBson)

data Updatable
  = USet Json
  | URaw Json

newtype UpdateType = UpdateType
  { set :: Maybe Json
  }

instance decodeJsonEvent :: DecodeJson UpdateType where
  decodeJson json = pure $ UpdateType { set : Nothing }

instance encodeJsonEvent :: EncodeJson UpdateType where
  encodeJson (UpdateType u)
    = "$set" := u.set
    ~> jsonEmptyObject

printUpdatable :: Updatable -> Json
printUpdatable (USet a) = encodeJson $ UpdateType { set : Just a }
printUpdatable (URaw a) = a