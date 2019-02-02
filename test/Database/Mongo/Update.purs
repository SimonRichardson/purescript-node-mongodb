module Test.Database.Mongo.Update where

import Database.Mongo.Options

import Data.Argonaut.Core (Json, fromObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Database.Mongo.Bson.BsonValue as B
import Database.Mongo.Mongo (Database, updateOne, collection)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (fromFoldable)
import Prelude (Unit, bind, (==), ($), (<<<), discard)
import Test.Assert (assert)
import Test.Data.Event (Event(..))
import Test.Type (Test)

evt :: Event
evt = Event
  { name : Just "Wow"
  }

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< fromFoldable
 
main :: Database -> Test Unit
main database = do
  liftEffect $ log "should update"
  col <- collection "events" database
  res <- updateOne ["name" B.:= "Wow"] ["name" B.:= "lol"] defaultUpdateOptions col
  liftEffect $ assert $ (obj [ "ok" A.:= 1, "n" A.:= 1, "nModified" A.:= 1, "nInserted" A.:= 0]) == (encodeJson res)
