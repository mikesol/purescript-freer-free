module Test.Complex where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Free (Constructors, constructors)
import Data.Functor.Variant (VariantF, inj)
import Data.Generic.Rep (class Generic)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)

data TalkF a
  = Speak String (Unit -> a)
  | Listen (String -> a)

derive instance functorTalkF :: Functor TalkF

derive instance genericTalkF :: Generic (TalkF a) _

type TALK
  = FProxy TalkF

_talk = SProxy :: SProxy "talk"

type TalkV r
  = (VariantF ( talk ∷ FProxy TalkF | r ))

f :: forall r. Constructors TalkF (Free (TalkV r))
f = constructors (liftF <<< inj _talk :: TalkF ~> (Free (TalkV r)))

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

type DINNER
  = FProxy DinnerF

derive instance functorDinnerF :: Functor DinnerF

derive instance genericDinnerF :: Generic (DinnerF a) _

_dinner = SProxy :: SProxy "dinner"

type DinnerV r
  = (VariantF ( dinner ∷ FProxy DinnerF | r ))

d :: forall r. Constructors DinnerF (Free (DinnerV r))
d = constructors (liftF <<< inj _dinner :: DinnerF ~> (Free (DinnerV r)))

type LovelyEvening r
  = ( dinner :: DINNER, talk :: TALK | r )

dinnerTime :: forall r. Free (VariantF (LovelyEvening r)) Unit
dinnerTime = do
  f.speak "I'm famished!"
  isThereMore <- d.eat Hummus
  if isThereMore then
    dinnerTime
  else do
    bill <- d.checkPlease
    f.speak "Outrageous!"
