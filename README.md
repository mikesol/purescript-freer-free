# purescript-freer-free

(free) boilerplate for working with free monads.

> We've found that this repo is only useful if you're working with free monads that have more than ~5 constructors in the underlying ADT. For smaller ADTs, boilerplate is fine.

# Motivation

The following example, copied from [`purescript-run`](https://github.com/natefaubion/purescript-run), shows how free monads are often constructed.

```purescript
data TalkF a
  = Speak String a
  | Listen (String -> a)

derive instance functorTalkF :: Functor TalkF

type Talk = Free TalkF

-- Boilerplate definitions for lifting our constructors
-- into the Free DSL.

speak :: String -> Talk Unit
speak str = liftF (Speak str unit)

listen :: Talk String
listen = liftF (Listen identity)

-- Now we can write programs using our DSL.

program :: Talk Unit
program = do
  speak $ "Hello, what is your name?"
  name <- listen
  speak $ "Nice to meet you, " <> name
```

Those boilerplate definitions are no fun. Let's fix that.

With `purescript-freer-free`, we can do.

```purescript
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
```

Voilà, no more boilerplate.

Going back to the `purescript-run` example, let's imagine that we want to change our free monad to use variants.

```purescript
data TalkF a
  = Speak String a
  | Listen (String -> a)

derive instance functorTalkF :: Functor TalkF

type TALK
  = FProxy TalkF

_talk = SProxy :: SProxy "talk"

speak :: forall r. String -> Free (VariantF ( talk :: TALK | r )) Unit
speak str = liftF (inj _talk (Speak str unit))

listen :: forall r. Free (VariantF ( talk :: TALK | r )) String
listen = liftF (inj _talk (Listen identity))

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

eat :: forall r. Food -> Free (VariantF ( dinner :: DINNER | r )) IsThereMore
eat food = liftF (inj _dinner (Eat food identity))

checkPlease :: forall r. Free (VariantF ( dinner :: DINNER | r )) Bill
checkPlease = liftF (inj _dinner (CheckPlease identity))

type LovelyEvening r
  = ( dinner :: DINNER, talk :: TALK | r )

dinnerTime :: forall r. Free (VariantF (LovelyEvening r)) Unit
dinnerTime = do
  speak "I'm famished!"
  isThereMore <- eat Hummus
  if isThereMore then
    dinnerTime
  else do
    bill <- checkPlease
    speak "Outrageous!"
```

Using `freer`, all we have to do is change the transformation function passed to `freer`. The rest of the code can stay the same.

```purescript
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
```

# How this helps

There are three ways this library has helped us at Meeshkan:

- When we modify our variants can change the signature of helper functions like `f` without changing helper methods.
- As actions in free monads spill into the 10s, this reduces the size of a file substantially.
- We are able to read the program in terms of the underlying functor (aka nicer colors in the IDE).
