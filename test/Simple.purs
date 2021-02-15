module Test.Simple where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Free (Constructors, constructors, interpreter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

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

i0 :: Talk ~> Maybe
i0 = case _ of
  Speak s a -> pure a
  Listen a -> a <$> pure "hello"

i1 :: Talk ~> Maybe
i1 =
  interpreter
    { speak: const $ pure unit
    , listen: pure "hello"
    }
