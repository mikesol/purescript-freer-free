module Test.Simple where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Yo (yo, AsFree)

data Talk a
  = Speak String (Unit -> a)
  | Listen (String -> a)

derive instance functorTalkF :: Functor Talk

f = liftF <<< yo :: AsFree

program :: Free Talk Unit
program = do
  f $ Speak "Hello, what is your name?"
  name <- f Listen
  f $ Speak ("Nice to meet you, " <> name)
  pure unit
