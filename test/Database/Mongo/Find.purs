module Test.Database.Mongo.Find where

import Data.Either (either, Either)
import Data.Maybe (Maybe(..))
import Database.Mongo.Bson.BsonValue ((:=))
import Database.Mongo.Mongo (Database, collection, findOne)
import Effect.Aff (attempt)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Prelude (Unit, bind, const, pure, unit, ($), discard)
import Test.Data.Event (Event(..))
import Test.Type (Test)

evt :: Event
evt = Event
  { name : Nothing 
  }

main :: Database -> Test Unit
main database = do
  liftEffect $ log "should find"
  col <- collection "events" database
  (e :: Either Error Event) <- attempt $ findOne [ "name" := "Wow" ] [] col
  either 
    (const $ liftEffect $ log "Oh noes!") 
    (const $ liftEffect $ log "Yays!") 
    e
  pure unit
