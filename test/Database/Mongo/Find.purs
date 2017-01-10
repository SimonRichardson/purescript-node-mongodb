module Test.Database.Mongo.Find where

import Prelude (Unit, bind, (>), ($))
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Database.Mongo.Bson.BsonValue ((:=))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Test.Data.Event (Event(..))
import Test.Assert (assert)
import Database.Mongo.Mongo (Database, collect, find, collection)
import Test.Type (Test)

evt :: Event
evt = Event
  { name : Just "Wow"
  }

main :: Database -> Test Unit
main database = do
  liftEff $ log "should find"
  col <- collection "events" database
  cur <- find [ "name" := "Wow" ] [ "name" := 1.0 ] col
  res <- collect cur
  liftEff $ assert $ length (res :: Array Event) > 0
