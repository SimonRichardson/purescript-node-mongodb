module Test.Type where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Database.Mongo.Mongo (DB)
import Test.Assert (ASSERT)

type Test a = forall e. Aff (console :: CONSOLE, db :: DB, assert :: ASSERT | e) a
