module Database.Mongo.Bson.ObjectId(ObjectId(..)) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Either (Either(..))
import Prelude (class Eq, class Show, ($), (<>))

data ObjectId = ObjectId String

derive instance eqObjectId :: Eq ObjectId
instance showObjectId :: Show ObjectId where
  show (ObjectId id) = "ObjectId(\"" <> id <> "\")"

foreign import hexStringFromObject :: Json -> String

instance decodeJsonObjectId :: DecodeJson ObjectId where
  decodeJson json = Right $ ObjectId $ hexStringFromObject json
