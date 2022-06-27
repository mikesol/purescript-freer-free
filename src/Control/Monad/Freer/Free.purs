module Control.Monad.Freer.Free where

import Prelude
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Product(..), Sum(..), from)
import Data.Generic.Rep (to) as Generics.Rep
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.Symbol (class Cons) as Symbol
import Record (get, delete)
import Record (insert) as Record
import Type.Proxy (Proxy(..))

class LowerFirst (i :: Symbol) (o :: Symbol) | i -> o

instance lowerFirst ::
  ( Symbol.Cons l s i
  , LowerCase l l'
  , Symbol.Cons l' s o
  ) =>
  LowerFirst i o

class LowerCase (i :: Symbol) (o :: Symbol) | i -> o

instance lowerCaseA :: LowerCase "A" "a"

instance lowerCaseB :: LowerCase "B" "b"

instance lowerCaseC :: LowerCase "C" "c"

instance lowerCaseD :: LowerCase "D" "d"

instance lowerCaseE :: LowerCase "E" "e"

instance lowerCaseF :: LowerCase "F" "f"

instance lowerCaseG :: LowerCase "G" "g"

instance lowerCaseH :: LowerCase "H" "h"

instance lowerCaseI :: LowerCase "I" "i"

instance lowerCaseJ :: LowerCase "J" "j"

instance lowerCaseK :: LowerCase "K" "k"

instance lowerCaseL :: LowerCase "L" "l"

instance lowerCaseM :: LowerCase "M" "m"

instance lowerCaseN :: LowerCase "N" "n"

instance lowerCaseO :: LowerCase "O" "o"

instance lowerCaseP :: LowerCase "P" "p"

instance lowerCaseQ :: LowerCase "Q" "q"

instance lowerCaseR :: LowerCase "R" "r"

instance lowerCaseS :: LowerCase "S" "s"

instance lowerCaseT :: LowerCase "T" "t"

instance lowerCaseU :: LowerCase "U" "u"

instance lowerCaseV :: LowerCase "V" "v"

instance lowerCaseW :: LowerCase "W" "w"

instance lowerCaseX :: LowerCase "X" "x"

instance lowerCaseY :: LowerCase "Y" "y"

instance lowerCaseZ :: LowerCase "Z" "z"

data ConstructorPath

foreign import data Top :: ConstructorPath

foreign import data Inl :: ConstructorPath -> ConstructorPath

foreign import data Inr :: ConstructorPath -> ConstructorPath

class ReconstructGeneric (path :: ConstructorPath) a g | path a -> g where
  reconstructGeneric :: Proxy path -> a -> g

instance reconstructGenericTop :: ReconstructGeneric Top a a where
  reconstructGeneric _ a = a
else instance reconstructGenericInl ::
  (ReconstructGeneric p (Sum a t) b) =>
  ReconstructGeneric (Inl p) a b where
  reconstructGeneric _ a = reconstructGeneric (Proxy :: Proxy p) (Inl a :: Sum a t)
else instance reconstructGenericInr ::
  (ReconstructGeneric p (Sum t a) b) =>
  ReconstructGeneric (Inr p) a b where
  reconstructGeneric _ a = reconstructGeneric (Proxy :: Proxy p) (Inr a :: Sum t a)

class GenericFreeConstructor :: forall k. (Type -> Type) -> (Type -> Type) -> k -> ConstructorPath -> Row Type -> Row Type -> Constraint
class GenericFreeConstructor m (t :: Type -> Type) g (p :: ConstructorPath) rin rout | t g -> rin rout p where
  genericFreeConstructor :: (forall a. t a -> m a) -> Proxy g -> Proxy p -> { | rin } -> { | rout }

instance genericFreeConstructorSum ::
  ( GenericFreeConstructor m t l (Inl p) rin lout
  , GenericFreeConstructor m t r (Inr p) lout rout
  ) =>
  GenericFreeConstructor m t (Sum l r) p rin rout where
  genericFreeConstructor ntrans _ _ rin = rout
    where
    lout = genericFreeConstructor ntrans (Proxy :: Proxy l) (Proxy :: Proxy (Inl p)) rin

    rout = genericFreeConstructor ntrans (Proxy :: Proxy r) (Proxy :: Proxy (Inr p)) lout
else instance genericFreeConstructor0ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Argument Unit)) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Argument (Void))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Argument unit)) :: Constructor name (Argument Unit))) :: t Unit)
else instance genericFreeConstructor1ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Argument Unit))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Argument (Void)))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Argument unit))) :: Constructor name (Product (Argument arga) (Argument Unit)))) :: t Unit)
else instance genericFreeConstructor2ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Argument Unit)))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Argument (Void))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Argument unit)))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Argument Unit))))) :: t Unit)
else instance genericFreeConstructor3ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument Unit))))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument (Void)))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument unit))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument Unit)))))) :: t Unit)
else instance genericFreeConstructor4ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument Unit)))))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument (Void))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument unit)))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument Unit))))))) :: t Unit)
else instance genericFreeConstructor5ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument Unit))))))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument (Void)))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument unit))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument Unit)))))))) :: t Unit)
else instance genericFreeConstructor6ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> argf -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument Unit)))))))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument (Void))))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge argf = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument unit)))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument Unit))))))))) :: t Unit)
else instance genericFreeConstructor7ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> argf -> argg -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument Unit))))))))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument (Void)))))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge argf argg = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument unit))))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument Unit)))))))))) :: t Unit)
else instance genericFreeConstructor8ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument Unit)))))))))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument (Void))))))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge argf argg argh = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument unit)))))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument Unit))))))))))) :: t Unit)
else instance genericFreeConstructor9ParamThunk ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> argi -> m Unit) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument Unit))))))))))) g'
  , Generic (t Unit) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument (Void)))))))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge argf argg argh argi = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument unit))))))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument Unit)))))))))))) :: t Unit)
else instance genericFreeConstructor0ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Argument (args -> args))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Argument (args -> Void))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Argument identity)) :: Constructor name (Argument (args -> args)))) :: t args)
else instance genericFreeConstructor1ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Argument (args -> args)))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Argument (args -> Void)))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Argument identity))) :: Constructor name (Product (Argument arga) (Argument (args -> args))))) :: t args)
else instance genericFreeConstructor2ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Argument (args -> args))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Argument (args -> Void))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Argument identity)))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Argument (args -> args)))))) :: t args)
else instance genericFreeConstructor3ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument (args -> args)))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument (args -> Void)))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument identity))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument (args -> args))))))) :: t args)
else instance genericFreeConstructor4ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument (args -> args))))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument (args -> Void))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument identity)))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument (args -> args)))))))) :: t args)
else instance genericFreeConstructor5ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument (args -> args)))))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument (args -> Void)))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument identity))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument (args -> args))))))))) :: t args)
else instance genericFreeConstructor6ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> argf -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument (args -> args))))))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument (args -> Void))))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge argf = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument identity)))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument (args -> args)))))))))) :: t args)
else instance genericFreeConstructor7ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> argf -> argg -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument (args -> args)))))))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument (args -> Void)))))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge argf argg = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument identity))))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument (args -> args))))))))))) :: t args)
else instance genericFreeConstructor8ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument (args -> args))))))))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument (args -> Void))))))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge argf argg argh = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument identity)))))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument (args -> args)))))))))))) :: t args)
else instance genericFreeConstructor9ParamYoneda ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> argi -> m args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument (args -> args)))))))))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor m t (Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument (args -> Void)))))))))))) p rin rout where
  genericFreeConstructor ntrans _ p rin = Record.insert (Proxy :: Proxy name') f rin
    where
    f arga argb argc argd arge argf argg argh argi = ntrans $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument identity))))))))))) :: Constructor name (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument (args -> args))))))))))))) :: t args)

constructors :: forall m g rout t. Generic (t Void) g => GenericFreeConstructor m t g Top () rout => (forall a. t a -> m a) -> { | rout }
constructors ntrans = genericFreeConstructor ntrans (Proxy :: Proxy g) (Proxy :: Proxy Top) {}

class GenericFreeInterpreter (t :: Type -> Type) (g :: Type) (m :: Type -> Type) (a :: Type) (table :: Row Type) where
  genericFreeInterpreter :: Proxy t -> { | table } -> g -> m a

instance genericFreeInterpreterSum0Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Argument (b -> a))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Argument c)) -> c <$> (get (Proxy :: Proxy sym) r)
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x

else instance genericFreeInterpreterSum1Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Argument a))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Argument c))) -> (get (Proxy :: Proxy sym) r) arga *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum2Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Argument a)))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Argument c)))) -> (get (Proxy :: Proxy sym) r) arga argb *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum3Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument a))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument c))))) -> (get (Proxy :: Proxy sym) r) arga argb argc *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum4Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument a)))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument c)))))) -> (get (Proxy :: Proxy sym) r) arga argb argc argd *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum5Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument a))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument c))))))) -> (get (Proxy :: Proxy sym) r) arga argb argc argd arge *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum6Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument a)))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument c)))))))) -> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum7Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument a))))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument c))))))))) -> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum8Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument a)))))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument c)))))))))) -> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg argh *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum9Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> argi -> m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument a))))))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument c))))))))))) -> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg argh argi *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum9Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> argi -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument (b -> a)))))))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument c))))))))))) -> c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg argh argi
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum8Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument (b -> a))))))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument c)))))))))) -> c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg argh
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x

else instance genericFreeInterpreterSum7Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument (b -> a)))))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument c))))))))) -> c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum6Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument (b -> a))))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument c)))))))) -> c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum5Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument (b -> a)))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument c))))))) -> c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum4Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument (b -> a))))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument c)))))) -> c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum3Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument (b -> a)))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument c))))) -> c <$> (get (Proxy :: Proxy sym) r) arga argb argc
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum2Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Argument (b -> a))))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Product (Argument argb) (Argument c)))) -> c <$> (get (Proxy :: Proxy sym) r) arga argb
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum1Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> m b) prev table, GenericFreeInterpreter t cont m a prev, Functor m) => GenericFreeInterpreter t (Sum (Constructor sym' (Product (Argument arga) (Argument (b -> a)))) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Product (Argument arga) (Argument c))) -> c <$> (get (Proxy :: Proxy sym) r) arga
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreterSum0Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (m Unit) prev table, GenericFreeInterpreter t cont m a prev, Apply m, Applicative m) => GenericFreeInterpreter t (Sum (Constructor sym' (Argument a)) cont) m a table where
  genericFreeInterpreter a r g = case g of
    Inl (Constructor (Argument c)) -> (get (Proxy :: Proxy sym) r) *> pure c
    Inr x -> (genericFreeInterpreter :: (Proxy t -> { | prev } -> cont -> m a)) a (delete (Proxy :: Proxy sym) r) x
else instance genericFreeInterpreter0Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Argument (b -> a))) m a table where
  genericFreeInterpreter _ r (Constructor (Argument c)) = c <$> (get (Proxy :: Proxy sym) r)
else instance genericFreeInterpreter1Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Argument (b -> a)))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Argument c))) = c <$> (get (Proxy :: Proxy sym) r) arga
else instance genericFreeInterpreter2Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Argument (b -> a))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Argument c)))) = c <$> (get (Proxy :: Proxy sym) r) arga argb
else instance genericFreeInterpreter3Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument (b -> a)))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument c))))) = c <$> (get (Proxy :: Proxy sym) r) arga argb argc
else instance genericFreeInterpreter4Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument (b -> a))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument c)))))) = c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd
else instance genericFreeInterpreter5Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument (b -> a)))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument c))))))) = c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge
else instance genericFreeInterpreter6Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument (b -> a))))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument c)))))))) = c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf
else instance genericFreeInterpreter7Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument (b -> a)))))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument c))))))))) = c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg
else instance genericFreeInterpreter8Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument (b -> a))))))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument c)))))))))) = c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg argh
else instance genericFreeInterpreter9Yoneda :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> argi -> m b) prev table, Functor m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument (b -> a)))))))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument c))))))))))) = c <$> (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg argh argi
else instance genericFreeInterpreter0Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Argument a)) m a table where
  genericFreeInterpreter _ r (Constructor (Argument c)) = (get (Proxy :: Proxy sym) r) *> pure c
else instance genericFreeInterpreter9Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> argi -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument a))))))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Product (Argument argi) (Argument c))))))))))) = (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg argh argi *> pure c
else instance genericFreeInterpreter8Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> argh -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument a)))))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Product (Argument argh) (Argument c)))))))))) = (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg argh *> pure c
else instance genericFreeInterpreter7Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> argg -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument a))))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Product (Argument argg) (Argument c))))))))) = (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf argg *> pure c
else instance genericFreeInterpreter6Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> argf -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument a)))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Product (Argument argf) (Argument c)))))))) = (get (Proxy :: Proxy sym) r) arga argb argc argd arge argf *> pure c
else instance genericFreeInterpreter5Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> arge -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument a))))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Product (Argument arge) (Argument c))))))) = (get (Proxy :: Proxy sym) r) arga argb argc argd arge *> pure c
else instance genericFreeInterpreter4Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> argd -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument a)))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Product (Argument argd) (Argument c)))))) = (get (Proxy :: Proxy sym) r) arga argb argc argd *> pure c
else instance genericFreeInterpreter3Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> argc -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument a))))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Product (Argument argc) (Argument c))))) = (get (Proxy :: Proxy sym) r) arga argb argc *> pure c
else instance genericFreeInterpreter2Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> argb -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Product (Argument argb) (Argument a)))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Product (Argument argb) (Argument c)))) = (get (Proxy :: Proxy sym) r) arga argb *> pure c
else instance genericFreeInterpreter1Thunk :: (LowerFirst sym' sym, IsSymbol sym, IsSymbol sym', Lacks sym prev, Cons sym (arga -> m Unit) prev table, Apply m, Applicative m) => GenericFreeInterpreter t (Constructor sym' (Product (Argument arga) (Argument a))) m a table where
  genericFreeInterpreter _ r (Constructor (Product (Argument arga) (Argument c))) = (get (Proxy :: Proxy sym) r) arga *> pure c

interpreter :: forall t g m a table. GenericFreeInterpreter t g m a table => Generic (t a) g => Record table -> t a -> m a
interpreter ntrans ipt = genericFreeInterpreter (Proxy :: Proxy t) ntrans (from ipt)

type Constructors t m
  = forall g rout. Generic (t Void) g => GenericFreeConstructor m t g Top () rout => Record rout
