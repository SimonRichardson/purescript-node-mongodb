module Test.Database.Mongo.Find where

import Control.Alt ((<|>))
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut (encodeJson)
import Data.Array (length)
import Data.Either (either, Either)
import Data.Maybe (Maybe(..))
import Data.Unit (unit)
import Database.Mongo.Bson.BsonValue ((:=))
import Database.Mongo.Mongo (Database, collect, collection, find, findOne)
import Prelude (Unit, bind, const, pure, show, unit, ($), (>))
import Test.Assert (assert)
import Test.Data.Event (Event(..))
import Test.Type (Test)

evt :: Event
evt = Event
  { name : Nothing
  }

main :: Database -> Test Unit
main database = do
  liftEff $ log "should find"
  col <- collection "events" database
  (e :: Either Error Event) <- attempt $ findOne [ "name" := "Wow" ] [] col
  liftEff $ either (const $ log "Oh noes!") (const $ log "Yays!") e
  pure unit
