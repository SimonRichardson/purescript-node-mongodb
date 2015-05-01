module Database.Mongo.Options
  ( InsertOptions()
  , WriteConcern()
  , defaultInsertOptions
  , insertOptions
  ) where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject)
import Data.Argonaut.Core (Json())
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either
import Data.Maybe

newtype InsertOptions = InsertOptions
  { writeConcern :: Maybe WriteConcern 
  , journaled    :: Maybe Boolean
  }

defaultInsertOptions :: InsertOptions
defaultInsertOptions = InsertOptions
  { writeConcern : Nothing
  , journaled    : Just false
  }

type WriteConcern = Number

insertOptions :: InsertOptions -> Json
insertOptions = encodeJson

instance decodeJsonInsertOptions :: DecodeJson InsertOptions where
  decodeJson json = do
    obj  <- decodeJson json
    w <- obj .? "w"
    j <- obj .? "j"
    pure $ InsertOptions
      { writeConcern : w
      , journaled    : j  
      }

instance encodeJsonInsertOptions :: EncodeJson InsertOptions where
  encodeJson (InsertOptions o)
    =  "w" := o.writeConcern
    ~> "j" := o.journaled
    ~> jsonEmptyObject