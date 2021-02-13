module Test.Simple where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Free (FreeMonster, freer)

data Talk a
  = Speak String a
  | Listen (String -> a)

derive instance functorTalkF :: Functor Talk

f = freer liftF :: FreeMonster Talk Talk

program :: Free Talk Unit
program = do
  (f.mo'1 Speak) $ "Hello, what is your name?"
  name <- (f.mo0 Listen)
  (f.mo'1 Speak) $ "Nice to meet you, " <> name
