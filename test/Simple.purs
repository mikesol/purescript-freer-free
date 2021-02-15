module Test.Simple where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Free (Constructors, constructors)
import Data.Generic.Rep (class Generic)

data Talk a
  = Speak String (Unit -> a)
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
