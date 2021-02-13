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
data Talk a
  = Speak String (Unit -> a)
  | Listen (String -> a)

derive instance functorTalkF :: Functor Talk

f = liftF <<< yo :: AsFree

program :: Free Talk Unit
program = do
  f $ Speak "Hello, what is your name?"
  name <- f Listen
  void $ f $ Speak ("Nice to meet you, " <> name)
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
```

# How this helps

There are three ways this library has helped us at Meeshkan:

- When we modify our variants can change the signature of helper functions like `f` without changing helper methods.
- As actions in free monads spill into the 10s, this reduces the size of a file substantially.
- We are able to read the program in terms of the underlying functor (aka nicer colors in the IDE).
