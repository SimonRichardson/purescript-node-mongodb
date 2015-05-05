module Database.Mongo.Bson.BsonValue
  ( Document(..)
  , Field(..)
  , ObjectId(..)
  , BsonValue(..)
  , IsBsonValue, toBson
  , (:=)
  , printBson
  ) where

import Data.Argonaut.Core (Json())
import Data.String.Regex
import Data.Tuple

type Field = Tuple String BsonValue

type Document = [Field]

data ObjectId = ObjectId String

data BsonValue
  = VString   String
  | VNumber   Number
  | VRegex    Regex
  | VDocument [Tuple String BsonValue]
  | VArray    [BsonValue]
  | VObjectId ObjectId
  | VJson     Json

class IsBsonValue a where
  toBson :: a -> BsonValue

instance stringBson :: IsBsonValue String where
  toBson = VString

instance numberBson :: IsBsonValue Number where
  toBson = VNumber

instance regexBson :: IsBsonValue Regex where
  toBson = VRegex

instance documentBson :: IsBsonValue [Tuple String BsonValue] where
  toBson = VDocument

instance arrayBson :: IsBsonValue [BsonValue] where
  toBson = VArray

instance objectIdBson :: IsBsonValue ObjectId where
  toBson = VObjectId

instance jsonBson :: IsBsonValue Json where
  toBson = VJson

infix 0 :=
    
(:=) :: forall a. (IsBsonValue a) => String -> a -> Tuple String BsonValue
(:=) f v = Tuple f (toBson v)

printBson :: Document -> Json
printBson = unsafeToBsonObject

foreign import unsafeToBsonObject
  """
  function extract(v) {
    if(Array.isArray(v)) {
      return v.map(function(a) {
        return unsafeToBsonObject([a]);
      });
    }
    return v;
  }
  function unsafeToBsonObject(doc){
    return doc.reduce(function(b, a) {
      b[a["value0"]] = extract(a["value1"]["value0"]);
      return b;
    }, {});
  }
  """ :: Document -> Json