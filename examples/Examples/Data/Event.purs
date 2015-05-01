module Data.Event where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Core (Json(), JString(), toString)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Date
import Data.Either
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)

import Debug.Trace

newtype Event = Event
  { name :: Maybe String
  , date :: Maybe Date
  }

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj  <- decodeJson json
    name <- obj .? "name"
    pure $ Event
      { name : name
      , date : Nothing
      }

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event e)
    = "name" := e.name
    ~> jsonEmptyObject

instance showEvent :: Show Event where
  show (Event e) = "Event " ++
    "{ name: " ++ show e.name ++
    "}"
  