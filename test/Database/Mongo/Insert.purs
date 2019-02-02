module Test.Database.Mongo.Insert where

import Test.Assert

import Data.Argonaut.Core (Json, fromObject)
import Data.Argonaut.Encode (encodeJson, (:=))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Database.Mongo.Mongo (Database, insertOne, collection)
import Database.Mongo.Options (defaultInsertOptions)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (fromFoldable)
import Prelude (Unit, bind, ($), (==), (<<<), discard)
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
  liftEffect $ log "should insert"
  col <- collection "events" database
  res <- insertOne evt defaultInsertOptions col
  liftEffect $ assert $ (obj [ "ok" := 1, "n" := 1, "nInserted" := 0, "nModified" := 0]) == (encodeJson res)
