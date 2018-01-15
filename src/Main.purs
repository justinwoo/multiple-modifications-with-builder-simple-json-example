module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foreign (ForeignError)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromMaybe)
import Data.Nullable (toMaybe)
import Data.Record.Builder as Builder
import Simple.JSON (readJSON)
import Type.Prelude (SProxy(..))

type MyRecord =
  { apple :: String
  , banana :: Array Int
  , cherry :: String
  }

testJSON :: String
testJSON = """
{
  "banana": null,
  "grape": "originally a grape"
}
"""

parseMyRecord :: String -> Either (NonEmptyList ForeignError) MyRecord
parseMyRecord s = do
  let
    builder
        = Builder.modify (SProxy :: SProxy "banana") (fromMaybe [] <<< toMaybe)
      <<< Builder.rename (SProxy :: SProxy "grape") (SProxy :: SProxy "cherry")
      <<< Builder.insert (SProxy :: SProxy "apple") "i wasn't invited"
  obj <- readJSON s
  pure $ Builder.build builder obj

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case parseMyRecord testJSON of
    Right {apple, banana, cherry} ->
      log $ "apple: " <> apple <> ", banana: " <> show banana <> ", cherry: " <> cherry
    Left e ->
      log $ "failed to parse: " <> show e

  -- result
  -- apple: i wasn't invited, banana: [], cherry: originally a grape
