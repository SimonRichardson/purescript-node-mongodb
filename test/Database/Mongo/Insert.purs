module Test.Database.Mongo.Insert where

import Prelude (Unit, bind, ($), (==), (<<<), discard)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.StrMap (fromFoldable)
import Data.Argonaut.Encode (encodeJson, (:=))
import Data.Argonaut.Core (Json, fromObject)
import Database.Mongo.Mongo (Database, insertOne, collection)
import Database.Mongo.Options (defaultInsertOptions)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Console (log)

import Test.Data.Event (Event(..))
import Test.Assert
import Test.Type (Test)

evt :: Event
evt = Event
  { name : Just "Wow"
  }

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< fromFoldable

main :: Database -> Test Unit
main database = do
  log "should insert"
  col <- collection "events" database
  res <- insertOne evt defaultInsertOptions col
  liftEff $ assert $ (obj [ "ok" := 1, "n" := 1, "nInserted" := 0, "nModified" := 0]) == (encodeJson res)
