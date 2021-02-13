module Test.Complex where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Yo (yo)
import Data.Functor.Variant (VariantF, inj)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant.Internal (FProxy)
import Prim.Row (class Cons)

data TalkF a
  = Speak String (Unit -> a)
  | Listen (String -> a)

derive instance functorTalkF :: Functor TalkF

type TALK
  = FProxy TalkF

_talk = SProxy :: SProxy "talk"

f :: forall end r func. Functor func => ((end -> end) -> func end) -> Free (VariantF ( talk ∷ FProxy func | r )) end
f = liftF <<< inj _talk <<< yo

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

type Foo (sym :: Symbol)
  = ∀ f a r1 r2. Cons sym (FProxy f) r1 r2 => IsSymbol sym => Functor f => ((a -> a) -> f a) -> Free (VariantF r2) a

d :: forall end r func. Functor func => ((end -> end) -> func end) -> Free (VariantF ( dinner ∷ FProxy func | r )) end
d = liftF <<< inj _dinner <<< yo

type LovelyEvening r
  = ( dinner :: DINNER, talk :: TALK | r )

dinnerTime :: forall r. Free (VariantF (LovelyEvening r)) Unit
dinnerTime = do
  f $ Speak "I'm famished!"
  isThereMore <- d $ Eat Hummus
  if isThereMore then
    dinnerTime
  else do
    bill <- d $ CheckPlease
    void $ f $ Speak "Outrageous!"
