module Test.Main where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Freer.Free (Constructors, constructors, interpreter)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Class.Console (log)

data Talk a
  = Speak String a
  | Listen (String -> a)

derive instance functorTalk :: Functor Talk

derive instance genericTalk :: Generic (Talk a) _

f :: Constructors Talk (Free Talk)
f = constructors (liftF :: Talk ~> Free Talk)

program :: Free Talk Unit
program = do
  f.speak "Hello, what is your name?"
  name <- f.listen
  f.speak ("Nice to meet you, " <> name)

i0 :: Talk ~> Effect
i0 = case _ of
  Speak s a -> log s *> pure a
  Listen a -> a <$> pure "hello"

i1 :: Talk ~> Effect
i1 =
  interpreter
    { speak: \s -> log s *> pure unit
    , listen: pure "hello"
    }

main :: Effect Unit
main = foldFree i1 program