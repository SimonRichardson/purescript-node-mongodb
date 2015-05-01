module Database.Mongo.Results
  ( WriteResult()
  ) where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject)
import Data.Argonaut.Core (Json())
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either
import Data.Maybe

newtype WriteResult = WriteResult 
  { success  :: Boolean
  , inserted :: Maybe Number 
  }

instance decodeJsonWriteResult :: DecodeJson WriteResult where
  decodeJson json = do
    obj <- decodeJson json
    ok  <- obj .? "ok"
    n   <- obj .? "n"
    pure $ WriteResult
      { success  : jsNumberToBool ok
      , inserted : n
      }

instance encodeJsonWriteResult :: EncodeJson WriteResult where
  encodeJson (WriteResult w) = jsonEmptyObject

-- node mongodb module sends back `1` to mean `true`, this is why we need types
-- as Javascript is abused!
jsNumberToBool :: Either String Number -> Boolean
jsNumberToBool e = case e of
  Left _ -> false
  Right x -> if x == 1 then true else false