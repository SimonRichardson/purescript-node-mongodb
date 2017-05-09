module Test.Database.Mongo.Find where

import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Exception (Error)
import Data.Either (either, Either)
import Data.Maybe (Maybe(..))
import Database.Mongo.Bson.BsonValue ((:=))
import Database.Mongo.Mongo (Database, collection, findOne)
import Prelude (Unit, bind, const, pure, unit, ($),  discard)
import Test.Data.Event (Event(..))
import Test.Type (Test)

evt :: Event
evt = Event
  { name : Nothing
  }

main :: Database -> Test Unit
main database = do
  log "should find"
  col <- collection "events" database
  (e :: Either Error Event) <- attempt $ findOne [ "name" := "Wow" ] [] col
  either (const $ log "Oh noes!") (const $ log "Yays!") e
  pure unit
