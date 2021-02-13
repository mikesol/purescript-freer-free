module Test.Simple where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Free (FreeMonster, freer)
import Data.Functor.Variant (VariantF, inj)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)

data TalkF a
  = Speak String a
  | Listen (String -> a)

derive instance functorTalkF :: Functor TalkF

type TALK
  = FProxy TalkF

_talk = SProxy :: SProxy "talk"

f = freer (liftF <<< inj _talk) :: forall r. FreeMonster (VariantF ( talk :: TALK | r )) TalkF

data Food
  = Hummus
  | Falafel
  | Haloumi

type IsThereMore
  = Boolean

type Bill
  = Int

data DinnerF a
  = Eat Food (IsThereMore -> a)
  | CheckPlease (Bill -> a)

derive instance functorDinnerF :: Functor DinnerF

type DINNER
  = FProxy DinnerF

_dinner = SProxy :: SProxy "dinner"

d = freer (liftF <<< inj _dinner) :: forall r. FreeMonster (VariantF ( dinner :: DINNER | r )) DinnerF

type LovelyEvening r
  = ( dinner :: DINNER, talk :: TALK | r )

dinnerTime :: forall r. Free (VariantF (LovelyEvening r)) Unit
dinnerTime = do
  (f.mo'1 Speak) "I'm famished!"
  isThereMore <- (d.mo1 Eat) Hummus
  if isThereMore then
    dinnerTime
  else do
    bill <- (d.mo0 CheckPlease)
    (f.mo'1 Speak) "Outrageous!"
