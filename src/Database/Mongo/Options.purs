module Database.Mongo.Options
  ( WriteConcern()
  , InsertOptions()
  , defaultInsertOptions, insertOptions
  , UpdateOptions()
  , defaultUpdateOptions, updateOptions
  ) where

import Prelude
import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject)
import Data.Argonaut.Core (Json())
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Either
import Data.Maybe

-- | The type of WriteConcern
type WriteConcern = Number

-- | Typed options for inserting documents into a collection.
newtype InsertOptions = InsertOptions
  { writeConcern :: Maybe WriteConcern
  , journaled    :: Maybe Boolean
  }

defaultInsertOptions :: InsertOptions
defaultInsertOptions = InsertOptions
  { writeConcern : Nothing
  , journaled    : Just false
  }

insertOptions :: InsertOptions -> Json
insertOptions = encodeJson

instance decodeJsonInsertOptions :: DecodeJson InsertOptions where
  decodeJson json = do
    obj  <- decodeJson json
    w    <- obj .? "w"
    j    <- obj .? "j"
    pure $ InsertOptions
      { writeConcern : w
      , journaled    : j
      }

instance encodeJsonInsertOptions :: EncodeJson InsertOptions where
  encodeJson (InsertOptions o)
    =  "w" := o.writeConcern
    ~> "j" := o.journaled
    ~> jsonEmptyObject

-- | Typed options for updating documents into a collection.
newtype UpdateOptions = UpdateOptions
  { writeConcern :: Maybe WriteConcern
  , journaled    :: Maybe Boolean
  , upsert       :: Maybe Boolean
  }

defaultUpdateOptions :: UpdateOptions
defaultUpdateOptions = UpdateOptions
  { writeConcern : Nothing
  , journaled    : Just false
  , upsert       : Just false
  }

updateOptions :: UpdateOptions -> Json
updateOptions = encodeJson

instance decodeJsonUpdateOptions :: DecodeJson UpdateOptions where
  decodeJson json = do
    obj    <- decodeJson json
    w      <- obj .? "w"
    j      <- obj .? "j"
    upsert <- obj .? "upsert"
    pure $ UpdateOptions
      { writeConcern : w
      , journaled    : j
      , upsert       : upsert
      }

instance encodeJsonUpdateOptions :: EncodeJson UpdateOptions where
  encodeJson (UpdateOptions o)
    =  "w"      := o.writeConcern
    ~> "j"      := o.journaled
    ~> "upsert" := o.upsert
    ~> jsonEmptyObject
