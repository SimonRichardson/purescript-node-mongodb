module Test.Database.Mongo.Update where

import Prelude (Unit, bind, (==), ($), (<<<))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.StrMap (fromFoldable)
import Data.Argonaut as A
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Test.Data.Event (Event(..))
import Test.Assert (assert)
import Database.Mongo.Mongo (Database, updateOne, collection)
import Database.Mongo.Bson.BsonValue as B
import Database.Mongo.Options
import Test.Type (Test)

evt :: Event
evt = Event
  { name : Just "Wow"
  }

obj :: Array (Tuple String Json) -> Json
obj = A.fromObject <<< fromFoldable

main :: Database -> Test Unit
main database = do
  liftEff $ log "should update"
  col <- collection "events" database
  res <- updateOne ["name" B.:= "Wow"] ["name" B.:= "lol"] defaultUpdateOptions col
  liftEff $ assert $ (obj [ "ok" A.:= 1, "n" A.:= 1, "nModified" A.:= 1, "nInserted" A.:= 0]) == (encodeJson res)
