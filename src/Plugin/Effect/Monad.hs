{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Plugin.Effect.Monad where

import Control.Exception
import Control.Applicative     (Alternative(..))
import Control.Monad.Codensity (Codensity(..))
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.SearchTree
import Control.Monad.State

import qualified Data.Kind
import           Data.Map                  (Map)
import qualified Data.Map           as Map
import           Data.Set                  (Set)
import qualified Data.Set           as Set
import           Data.Typeable             (type (:~:)(..))
#ifdef TYPED
import           Data.Maybe                (fromMaybe)
import           Data.Typeable             (Typeable, cast)
#endif

#ifndef USE_WHAT4
import Plugin.Effect.SolverLibrary.SBV   ()
#else
import Plugin.Effect.SolverLibrary.What4 ()
#endif
import Plugin.Lifted

import Test.ChasingBottoms.IsBottom (isBottom)

import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

type ND s = Codensity (ReaderT s Search)

evalND :: ND s a -> s -> Search a
evalND nd = runReaderT (runCodensity nd return)

{-type ND1 s a = StateT s SearchTree a

evalND1 :: ND1 s a -> s -> SearchTree a
evalND1 = evalStateT

type ND2 s a = Codensity (ReaderT s SearchTree) a

evalND2 :: ND2 s a -> s -> SearchTree a
evalND2 nd = runReaderT (runCodensity nd return)

type ND3 s a = Codensity (ReaderT s (Codensity SearchTree)) a

evalND3 :: ND3 s a -> s -> SearchTree a
evalND3 nd s = runCodensity (runReaderT (runCodensity nd return) s) return-}

--------------------------------------------------------------------------------

type ID = Integer

--------------------------------------------------------------------------------

#ifdef TYPED
data Typed = forall a. Typeable a => Typed a
#else
data Untyped = forall a. Untyped a
#endif

#ifdef TYPED
type Heap = Map ID Typed
#else
type Heap = Map ID Untyped
#endif

emptyHeap :: Heap
emptyHeap = Map.empty

#ifdef TYPED
insertBinding :: Typeable a => ID -> a -> Heap -> Heap
insertBinding i = Map.insert i . Typed
#else
insertBinding :: ID -> a -> Heap -> Heap
insertBinding i = Map.insert i . Untyped
#endif

#ifdef TYPED
data TypeCastException = TypeCastException

instance Show TypeCastException where
  show TypeCastException = "type cast failed"

instance Exception TypeCastException

findBinding :: Typeable a => ID -> Heap -> Maybe a
findBinding i = fmap (\ (Typed x) -> fromMaybe (throw TypeCastException) (cast x)) . Map.lookup i
#else
findBinding :: ID -> Heap -> Maybe a
findBinding i = fmap (\ (Untyped x) -> unsafeCoerce x) . Map.lookup i
#endif

--------------------------------------------------------------------------------

class Narrowable a where
  narrow :: ID -> [(a, Integer)]
  --TODO: narrowSameConstr :: ID -> a -> (a, Integer)

--------------------------------------------------------------------------------

--TODO: Remark on redundancy (other word): although many constraints could be expressed using other constraints, we have all possible functions in our interface in order for libraries to be able to invoke the natively supported operation instead of imitating (other word) it...
class SolverLibrary where
  type Constraint

  checkConsistency :: [Constraint] -> Bool

  type Constrainable a :: Data.Kind.Constraint

  getModels :: Constrainable a => ID -> [Constraint] -> [a]

  eqConstraint :: Constrainable a => FLVal a -> FLVal a -> Constraint
  notConstraint :: Constraint -> Constraint
  neqConstraint :: Constrainable a => FLVal a -> FLVal a -> Constraint
  neqConstraint x y = notConstraint (eqConstraint x y)
  --TODO: das hier ist eigentlich definitiv nicht notwendig, da man es mit negate und eqConstraint bekommt. einige implementierungen wie sbv unterstützen es aber direkt. what4 bspw. nicht. in jedem fall wird es aber von jeder implementierung unterstützt und sollte daher nicht maybe sein.

  intPlusConstraint, intMinusConstraint, intMulConstraint :: Maybe (FLVal (Lifted FL Int) -> FLVal (Lifted FL Int) -> FLVal (Lifted FL Int) -> Constraint)
  intNegateConstraint, intAbsConstraint, intSignumConstraint:: Maybe (FLVal (Lifted FL Int) -> FLVal (Lifted FL Int) -> Constraint)

  intQuotConstraint, intRemConstraint, intDivConstraint, intModConstraint :: Maybe (FLVal (Lifted FL Int) -> FLVal (Lifted FL Int) -> FLVal (Lifted FL Int) -> Constraint)

  integerPlusConstraint, integerMinusConstraint, integerMulConstraint :: Maybe (FLVal (Lifted FL Integer) -> FLVal (Lifted FL Integer) -> FLVal (Lifted FL Integer) -> Constraint)
  integerNegateConstraint, integerAbsConstraint, integerSignumConstraint:: Maybe (FLVal (Lifted FL Integer) -> FLVal (Lifted FL Integer) -> Constraint)

  integerQuotConstraint, integerRemConstraint, integerDivConstraint, integerModConstraint :: Maybe (FLVal (Lifted FL Integer) -> FLVal (Lifted FL Integer) -> FLVal (Lifted FL Integer) -> Constraint)

  floatPlusConstraint, floatMinusConstraint, floatMulConstraint :: Maybe (FLVal (Lifted FL Float) -> FLVal (Lifted FL Float) -> FLVal (Lifted FL Float) -> Constraint)
  floatNegateConstraint, floatAbsConstraint, floatSignumConstraint:: Maybe (FLVal (Lifted FL Float) -> FLVal (Lifted FL Float) -> Constraint)

  floatDivConstraint :: Maybe (FLVal (Lifted FL Float) -> FLVal (Lifted FL Float) -> FLVal (Lifted FL Float) -> Constraint)

  doublePlusConstraint, doubleMinusConstraint, doubleMulConstraint :: Maybe (FLVal (Lifted FL Double) -> FLVal (Lifted FL Double) -> FLVal (Lifted FL Double) -> Constraint)
  doubleNegateConstraint, doubleAbsConstraint, doubleSignumConstraint:: Maybe (FLVal (Lifted FL Double) -> FLVal (Lifted FL Double) -> Constraint)

  doubleDivConstraint :: Maybe (FLVal (Lifted FL Double) -> FLVal (Lifted FL Double) -> FLVal (Lifted FL Double) -> Constraint)

  intLtConstraint, intLeqConstraint, intGtConstraint, intGeqConstraint :: Maybe (FLVal (Lifted FL Int) -> FLVal (Lifted FL Int) -> Constraint)
  intMaxConstraint, intMinConstraint :: Maybe (FLVal (Lifted FL Int) -> FLVal (Lifted FL Int) -> FLVal (Lifted FL Int) -> Constraint)

  integerLtConstraint, integerLeqConstraint, integerGtConstraint, integerGeqConstraint :: Maybe (FLVal (Lifted FL Integer) -> FLVal (Lifted FL Integer) -> Constraint)
  integerMaxConstraint, integerMinConstraint :: Maybe (FLVal (Lifted FL Integer) -> FLVal (Lifted FL Integer) -> FLVal (Lifted FL Integer) -> Constraint)

  floatLtConstraint, floatLeqConstraint, floatGtConstraint, floatGeqConstraint :: Maybe (FLVal (Lifted FL Float) -> FLVal (Lifted FL Float) -> Constraint)
  floatMaxConstraint, floatMinConstraint :: Maybe (FLVal (Lifted FL Float) -> FLVal (Lifted FL Float) -> FLVal (Lifted FL Float) -> Constraint)

  doubleLtConstraint, doubleLeqConstraint, doubleGtConstraint, doubleGeqConstraint :: Maybe (FLVal (Lifted FL Double) -> FLVal (Lifted FL Double) -> Constraint)
  doubleMaxConstraint, doubleMinConstraint :: Maybe (FLVal (Lifted FL Double) -> FLVal (Lifted FL Double) -> FLVal (Lifted FL Double) -> Constraint)

  charLtConstraint, charLeqConstraint, charGtConstraint, charGeqConstraint :: Maybe (FLVal (Lifted FL Char) -> FLVal (Lifted FL Char) -> Constraint)
  charMaxConstraint, charMinConstraint :: Maybe (FLVal (Lifted FL Char) -> FLVal (Lifted FL Char) -> FLVal (Lifted FL Char) -> Constraint)

--------------------------------------------------------------------------------
data ConstraintStore = ConstraintStore {
    constraints     :: [Constraint],
    --TODO: The most commonly executed action will be the insertion of a constraint. Therefore we use a list for which the insertion is done in constant time. As for the lookup action: Everytime we need to check for consistency, we have to visit each constraint anyway.
    constrainedVars :: Set ID
    --TODO: Consistency checks are very time-consuming: Each time we have to call the external SMT solver and go through its entire cycle. In order to be able to minimize the frequency of consistency checks, we record the set of constrained variables. This way we can avoid a new consistency check when a variable is constrained that has not been recorded before.
  }

--TODO: Combinatorial explosion -> constraintstore erforderlich. sonst würde bei x == 0 instanziiert werden müssen und ganz viele bäume erzeugt werden.

-- TODO: type miniterium hacken, weltherrschft an mich reissen

initConstraintStore :: ConstraintStore
initConstraintStore = ConstraintStore {
    constraints     = [],
    constrainedVars = Set.empty
  }

--TODO: [ID] parameter only for efficiency reason
insertConstraint :: Constraint -> [ID] -> ConstraintStore -> ConstraintStore
insertConstraint c ids ConstraintStore { .. } =
  ConstraintStore { constraints = c : constraints, constrainedVars = Set.fromList ids `Set.union` constrainedVars }

isUnconstrained :: ID -> ConstraintStore -> Bool
isUnconstrained i = Set.notMember i . constrainedVars

isConsistent :: ConstraintStore -> Bool
isConsistent = checkConsistency . constraints

generate :: Constrainable a => ID -> ConstraintStore -> [a]
generate i = getModels i . constraints

--------------------------------------------------------------------------------

data PrimitiveInfo a = Narrowable a => NoPrimitive
                     | Constrainable a => Primitive

#ifdef TYPED
class Typeable a => HasPrimitiveInfo a where
#else
class HasPrimitiveInfo a where
#endif
  primitiveInfo :: PrimitiveInfo a

--------------------------------------------------------------------------------

data FLVal a = HasPrimitiveInfo a => Var ID | Val a

--------------------------------------------------------------------------------

data FLState = FLState {
    nextID          :: ID,
    heap            :: Heap,
    constraintStore :: ConstraintStore
  }

initFLState :: FLState
initFLState = FLState {
    nextID          = 0,
    heap            = emptyHeap,
    constraintStore = initConstraintStore
  }

--------------------------------------------------------------------------------

newtype FL a = FL { unFL :: ND FLState (FLVal a) }

instance Functor FL where
  fmap = liftM

instance Applicative FL where
  pure x = FL (pure (Val x))
  (<*>) = ap

--TODO: ketten durchbrechen und variable zurückliefern (instanziierung hängt von groundNF oder NF ab)
{-resolve :: FL a -> ND FLState (FLVal a)
resolve fl = unFL fl >>= \case
  Val x -> return (Val x)
  Var i -> get >>= \ FLState { .. } -> case findBinding i heap of
    Nothing -> return (Var i)
    Just x  -> resolve x-}

resolve :: FL a -> ND FLState (FLVal a)
resolve = resolve' []

resolve' :: [ID] -> FL a -> ND FLState (FLVal a)
resolve' is fl = unFL fl >>= \case
  Val x -> return (Val x)
  Var i | i `elem` is -> return (Var i)
        | otherwise   -> get >>= \ FLState { .. } -> case findBinding i heap of
    Nothing -> return (Var i)
    Just x  -> resolve' (i:is) x

instantiate :: forall a. HasPrimitiveInfo a => ID -> ND FLState a
instantiate i = get >>= \ FLState { .. } ->
  case primitiveInfo @a of
    NoPrimitive -> msum (map update (narrow nextID))
      where update (x, o) = do
              put (FLState { nextID = nextID + o, heap = insertBinding i (return @FL x) heap, .. })
              return x
    Primitive   -> msum (map update (generate i constraintStore))
      where update x = do
              let c = eqConstraint (Var i) (Val x)
              put (FLState { heap =  insertBinding i (return @FL x) heap, constraintStore = insertConstraint c [i] constraintStore, .. })
              return x

{-
resolve :: FL a -> ND FLState (FLVal a)
resolve fl = unFL fl >>= \case
  Val x -> return (Val x)
  Var i -> get >>= \ FLState { .. } -> case findBinding i heap of
    Nothing -> return (Var i)
    Just x  -> return (Val x)

instantiate :: forall a. HasPrimitiveInfo a => ID -> ND FLState a
instantiate i = get >>= \ FLState { .. } ->
  case primitiveInfo @a of
    NoPrimitive -> msum (map update (narrow nextID))
      where update (x, o) = do
              put (FLState { nextID = nextID + o, heap = insertBinding i x heap, .. })
              return x
    Primitive   -> msum (map update (generate i constraintStore))
      where update x = do
              let c = eqConstraint (Var i) (Val x)
              put (FLState { heap =  insertBinding i x heap, constraintStore = insertConstraint c [i] constraintStore, .. })
              return x
-}

instance Monad FL where
  fl >>= f = FL $
    resolve fl >>= \case
      Var i -> instantiate i >>= unFL . f
      Val x -> unFL (f x)

instance Alternative FL where
  empty = FL empty
  FL a1 <|> FL a2 = FL (a1 <|> a2)

instance MonadPlus FL

instance MonadFail FL where
  fail s = FL (fail s)

free :: HasPrimitiveInfo a => ID -> FL a
free i = FL (return (Var i))

--------------------------------------------------------------------------------

class NormalForm a where
  normalFormWith :: Applicative m => (forall b. NormalForm b => FL (Lifted FL b) -> ND FLState (m (Lifted m b))) -> Lifted FL a -> ND FLState (m (Lifted m a))

--TODO: groundNormalFormND or groundNormalForm?
groundNormalFormFL :: NormalForm a => FL (Lifted FL a) -> ND FLState (Identity (Lifted Identity a))
groundNormalFormFL fl = resolve fl >>= \case
  Val x -> normalFormWith groundNormalFormFL x
  Var i -> instantiate i >>= normalFormWith groundNormalFormFL

normalFormFL :: NormalForm a => FL (Lifted FL a) -> ND FLState (Either ID (Lifted (Either ID) a))
normalFormFL fl = resolve fl >>= \case
  Val x -> normalFormWith normalFormFL x
  Var i -> return (Left i)

evalWith :: NormalForm a => (forall b. NormalForm b => FL (Lifted FL b) -> ND FLState (m (Lifted m b))) -> FL (Lifted FL a) -> Search (m (Lifted m a))
evalWith nf fl = evalND (nf fl) initFLState

--TODO: mit dre run equality stimmt nicht ganz, da das nur für die grundnormalform gilt. für die normalform ist trotzdem noch evalFL x /= evalFL (x >>= return)

--------------------------------------------------------------------------------

class Convertible a where
  to :: a -> Lifted FL a
  fromWith :: (forall b. Convertible b => m (Lifted m b) -> b) -> Lifted m a -> a

-- This function already incorporates the improvement from the paper for
-- partial values in the context of partial inversion with higher-order functions.
toFL :: Convertible a => a -> FL (Lifted FL a)
toFL x | isBottom x = empty
       | otherwise  = return (to x)

fromM :: Convertible a => (forall b. m b -> b) -> m (Lifted m a) -> a
fromM unM = fromWith (fromM unM) . unM

fromIdentity :: Convertible a => Identity (Lifted Identity a) -> a
fromIdentity = fromM runIdentity

data FreeVariableException = FreeVariableException ID

instance Show FreeVariableException where
  show (FreeVariableException _) = "free variable occured"

instance Exception FreeVariableException

fromEither :: Convertible a => Either ID (Lifted (Either ID) a) -> a
fromEither = fromM (either (throw . FreeVariableException) id)

--------------------------------------------------------------------------------

class Matchable a where
  match :: a -> Lifted FL a -> FL ()

--TODO: rename matchFL to something without FL?
matchFL :: forall a. (Convertible a, Matchable a) => a -> FL (Lifted FL a) -> FL ()
matchFL x fl = FL $ resolve fl >>= \case
  Var i -> get >>= \ FLState { .. } ->
    case primitiveInfo @(Lifted FL a) of
      NoPrimitive -> do
        put (FLState { heap = insertBinding i (toFL x) heap
                     , .. })
        return (Val ())
      Primitive   ->
        if isUnconstrained i constraintStore
          then do
            put (FLState { heap = insertBinding i (toFL x) heap
                        , .. })
            return (Val ())
          else
            let c = eqConstraint (Var i) (Val (to x))
                constraintStore' = insertConstraint c [i] constraintStore
            in if isConsistent constraintStore'
                 then do
                   put (FLState { heap = insertBinding i (toFL x) heap
                               , constraintStore = constraintStore'
                               , .. })
                   return (Val ())
                 else empty
  Val y  -> unFL $ match x y

-- linMatchFL :: forall a. (Convertible a, Matchable a) => a -> FL (Lifted a) -> FL ()
-- linMatchFL x (FL nd) = FL $ nd >>= \case
--   Var i -> lift get >>= \ (j, h, cst) -> do -- just do "update cst"
--     lift (put (j, insertBinding i (to x) h, cst))
--     return (Val ())
--   Val y -> unFL $ match x y

--------------------------------------------------------------------------------

class Unifiable a where
  lazyUnify :: Lifted FL a -> Lifted FL a -> FL ()

--TODO: eigentlich nur narrowable notwendig
narrowSameConstr :: (HasPrimitiveInfo (Lifted FL a), Unifiable a) => ID -> Lifted FL a -> FL ()
narrowSameConstr i x = FL $
  instantiate i >>= unFL . flip lazyUnify x

lazyUnifyVar :: forall a. (Unifiable a, HasPrimitiveInfo (Lifted FL a)) => ID -> Lifted FL a -> FL ()
lazyUnifyVar i x = FL $ get >>= \ FLState { .. } ->
  case primitiveInfo @(Lifted FL a) of
    NoPrimitive -> do
      --TODO: just narrow a single constructor
      unFL $ narrowSameConstr i x
      {-let (y, o) = undefined
      put (FLState { nextID = nextID + o, heap = insertBinding i (return @FL y) heap, .. })-}
    Primitive -> do
      let c = eqConstraint (Var i) (Val x)
      put (FLState { heap =  insertBinding i (return @FL x) heap, constraintStore = insertConstraint c [i] constraintStore, .. })
      return (Val ())

{-instance Unifiable a => Unifiable [a] where
  lazyUnifyVar j NilFL = FL $ get >>= \ FLState { .. } ->
    put (FLState { heap = insertBinding i fl1 heap
                , .. })
    return (Val ())
  lazyUnifyVar j (ConsFL x xs) = binde j an ConsFL (free h) (free i), 2 und mach lazyUnifyFL (free h) x und ....-}

-- output class lohnt sich für: $(inOutClassInv 'sort (Out [| [LT, var 1, GT] |] [| var 1 : var 2 |]))
-- $(inOutClassInv 'sort (Out [| [LT, var 1, GT] |] [| var 2 | ]))

-- $(inOutClassInv 'f (Out [| Just (var 1) |]) [| var 2 |])
-- f (Just x) = not x
-- f Nothing = False

--TODO: flip, rename fl1 to flx etc.
lazyUnifyFL :: forall a. (Unifiable a) => FL (Lifted FL a) -> FL (Lifted FL a) -> FL ()
lazyUnifyFL fl1 fl2 = FL $ resolve fl2 >>= \case
  Var i -> get >>= \ FLState { .. } ->
    case primitiveInfo @(Lifted FL a) of
      NoPrimitive -> do
        put (FLState { heap = insertBinding i fl1 heap
                    , .. })
        return (Val ())
      Primitive   ->
        if isUnconstrained i constraintStore
          then do
            put (FLState { heap = insertBinding i fl1 heap
                         , .. })
            return (Val ())
          else --TODO: i ist constrained, also müssen wir uns den anderen wert anschauen, um zu checken, ob er einem bereits bestehenden constraint widerspricht
            resolve fl1 >>= \case
              Var j ->
                let c = eqConstraint @(Lifted FL a) (Var i) (Var j)
                    constraintStore' = insertConstraint c [i, j] constraintStore
                in if isConsistent constraintStore'
                     then do
                       put (FLState { heap = insertBinding i (free @(Lifted FL a) j) heap
                                    , constraintStore = constraintStore'
                                    , .. })
                       return (Val ())
                     else empty
              Val x ->
                let c = eqConstraint (Var i) (Val x)
                    constraintStore' = insertConstraint c [i] constraintStore
                in if isConsistent constraintStore'
                     then do
                       put (FLState { heap = insertBinding i (return @FL x) heap
                                    , constraintStore = constraintStore'
                                    , .. })
                       return (Val ())
                     else empty
  Val y  -> resolve fl1 >>= \case
    Var j -> unFL $ lazyUnifyVar j y
    Val x -> unFL $ lazyUnify x y

-- "unify (error "bla") (var 1)" zeigt, dass es notwendig ist, dass man wissen muss ob 1 unconstrained ist.
-- var 1 == var 2
-- var 1 == var _

-- $(inOutClassInv 'id (Out [| var 1 |]) [| 3 |])
-- $(inOutClassInv 'id (Out [| 3 |]) [| var 1 |])

-- unifyFL (error "bla") (var 1)

-- f x = (x, if x == 42 then False else True)
-- unifyFL (43, False) $ fFL (var 1)
-- unifyFL (43, False) $ (var 1, )

-- g x y = (if x == y then False else True, x)

-- (1, error "bla") (var 1, var 1)
-- (1, var 1) (var 1, var 1)
--------------------------------------------------------------------------------

class ({-Unifiable a,-} Matchable a, Convertible a, NormalForm a, HasPrimitiveInfo (Lifted FL a)) => Invertible a

--------------------------------------------------------------------------------

--TODO: move?
infixr 0 :-->
type (:-->) = (-->) FL

data (-->) (m :: * -> *) (a :: *) (b :: *) where
  Func        :: (m a -> m b) -> (-->) m a b
  HaskellFunc :: (Convertible c, NormalForm c, Convertible d) => (c -> d) -> (-->) FL (Lifted FL c) (Lifted FL d)

infixr 0 -->

type instance Lifted m (->) = (-->) m
type instance Lifted m ((->) a) = (-->) m (Lifted m a)
type instance Lifted m ((->) a b) = (-->) m (Lifted m a) (Lifted m b)

-- TODO: GHC injectivity check cannot do decomposition, https://gitlab.haskell.org/ghc/ghc/-/issues/10833
-- Thus, we create the proof manually using unsafeCoerce
decomposeInjectivity :: Lifted m a ~ Lifted m b => a :~: b
decomposeInjectivity = unsafeCoerce Refl

instance (Convertible a, NormalForm a, Convertible b) => Convertible (a -> b) where
  to f = HaskellFunc f

  -- TODO: GHC injectivity check cannot do decomposition, https://gitlab.haskell.org/ghc/ghc/-/issues/10833
  -- from (HaskellFunc f) = unsafeCoerce f
  fromWith _ (HaskellFunc (f :: c -> d)) x = case (# decomposeInjectivity @FL @a @c, decomposeInjectivity @FL @b @d #) of
      (# Refl, Refl #) -> f x
  fromWith _ (Func        _) _ = error "Cannot convert function type"

--TODO: appM :: Monad m => m ((-->) m a b) -> m a -> m b
appFL :: FL ((-->) FL a b) -> FL a -> FL b
mf `appFL` x = mf >>= \case
  Func f        -> f x
  HaskellFunc f -> FL $ groundNormalFormFL x >>= (unFL . toFL . f . fromIdentity)
