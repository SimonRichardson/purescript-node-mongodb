module Database.Mongo.Results
  ( WriteResult()
  ) where

import Prelude (pure, bind, ($))
import Data.Argonaut ((.?), (:=), (~>), jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)

newtype WriteResult = WriteResult
  { success  :: Boolean
  , total    :: Number
  , inserted :: Maybe Number
  , modified :: Maybe Number
  }

instance decodeJsonWriteResult :: DecodeJson WriteResult where
  decodeJson json = do
    obj      <- decodeJson json
    ok       <- obj .? "ok"
    n        <- obj .? "n"

    let inserted = extract $ obj .? "nInserted"
    let modified = extract $ obj .? "nModified"

    pure $ WriteResult
      { success  : jsNumberToBool ok
      , total    : n
      , inserted : inserted
      , modified : modified
      }

instance encodeJsonWriteResult :: EncodeJson WriteResult where
  encodeJson (WriteResult w)
    =  "ok"        := boolToJsNumber w.success
    ~> "n"         := w.total
    ~> "nInserted" := fromMaybe 0.0 w.inserted
    ~> "nModified" := fromMaybe 0.0 w.modified
    ~> jsonEmptyObject

boolToJsNumber :: Boolean -> Int
boolToJsNumber false = 0
boolToJsNumber true = 1

-- node mongodb module sends back `1` to mean `true`, this is why we need types
-- as Javascript is abused!
jsNumberToBool :: Int -> Boolean
jsNumberToBool e = case e of
  1 -> true
  _ -> false

extract :: Either String Number -> Maybe Number
extract e = case e of
  Left _  -> Nothing
  Right x -> Just x
