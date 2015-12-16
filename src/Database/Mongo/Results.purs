module Database.Mongo.Results
  ( WriteResult()
  ) where

import Prelude
import Data.Argonaut ((.?), jsonEmptyObject)
import Data.Argonaut.Encode (EncodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either
import Data.Maybe

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
  encodeJson (WriteResult w) = jsonEmptyObject

-- node mongodb module sends back `1` to mean `true`, this is why we need types
-- as Javascript is abused!
jsNumberToBool :: Either String Int -> Boolean
jsNumberToBool e = case e of
  Left _  -> false
  Right x -> if x == 1 then true else false

extract :: Either String Number -> Maybe Number
extract e = case e of
  Left _  -> Nothing
  Right x -> Just x
