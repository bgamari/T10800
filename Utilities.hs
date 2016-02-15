{-# LANGUAGE FlexibleInstances, GADTs #-}
{-# LANGUAGE CPP, TypeFamilies, FlexibleContexts, RankNTypes, ScopedTypeVariables  #-}

module Utilities where

import Test.QuickCheck

import qualified Data.Vector as DV
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector.Fusion.Bundle as S

import Control.Monad (foldM, foldM_, zipWithM, zipWithM_)
import Control.Monad.Trans.Writer
import Data.Function (on)
import Data.Functor.Identity
import Data.List ( sortBy )
import Data.Monoid

instance Show a => Show (S.Bundle v a) where
    show s = "Data.Vector.Fusion.Bundle.fromList " ++ show (S.toList s)


instance Arbitrary a => Arbitrary (DV.Vector a) where
    arbitrary = fmap DV.fromList arbitrary

instance CoArbitrary a => CoArbitrary (DV.Vector a) where
    coarbitrary = coarbitrary . DV.toList

instance (Arbitrary a, DVP.Prim a) => Arbitrary (DVP.Vector a) where
    arbitrary = fmap DVP.fromList arbitrary

instance (CoArbitrary a, DVP.Prim a) => CoArbitrary (DVP.Vector a) where
    coarbitrary = coarbitrary . DVP.toList

instance (Arbitrary a, DVS.Storable a) => Arbitrary (DVS.Vector a) where
    arbitrary = fmap DVS.fromList arbitrary

instance (CoArbitrary a, DVS.Storable a) => CoArbitrary (DVS.Vector a) where
    coarbitrary = coarbitrary . DVS.toList

instance (Arbitrary a, DVU.Unbox a) => Arbitrary (DVU.Vector a) where
    arbitrary = fmap DVU.fromList arbitrary

instance (CoArbitrary a, DVU.Unbox a) => CoArbitrary (DVU.Vector a) where
    coarbitrary = coarbitrary . DVU.toList

instance Arbitrary a => Arbitrary (S.Bundle v a) where
    arbitrary = fmap S.fromList arbitrary

instance CoArbitrary a => CoArbitrary (S.Bundle v a) where
    coarbitrary = coarbitrary . S.toList

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

instance CoArbitrary a => CoArbitrary (Identity a) where
    coarbitrary = coarbitrary . runIdentity

instance Arbitrary a => Arbitrary (Writer a ()) where
    arbitrary = fmap (writer . ((,) ())) arbitrary

instance CoArbitrary a => CoArbitrary (Writer a ()) where
    coarbitrary = coarbitrary . runWriter

class (Testable (EqTest a), Conclusion (EqTest a)) => TestData a where
  type Model a
  model :: a -> Model a
  unmodel :: Model a -> a

  type EqTest a
  equal :: a -> a -> EqTest a

instance Eq a => TestData (S.Bundle v a) where
  type Model (S.Bundle v a) = [a]
  model = S.toList
  unmodel = S.fromList

  type EqTest (S.Bundle v a) = Property
  equal x y = property (x == y)

instance Eq a => TestData (DV.Vector a) where
  type Model (DV.Vector a) = [a]
  model = DV.toList
  unmodel = DV.fromList

  type EqTest (DV.Vector a) = Property
  equal x y = property (x == y)

instance (Eq a, DVP.Prim a) => TestData (DVP.Vector a) where
  type Model (DVP.Vector a) = [a]
  model = DVP.toList
  unmodel = DVP.fromList

  type EqTest (DVP.Vector a) = Property
  equal x y = property (x == y)

instance (Eq a, DVS.Storable a) => TestData (DVS.Vector a) where
  type Model (DVS.Vector a) = [a]
  model = DVS.toList
  unmodel = DVS.fromList

  type EqTest (DVS.Vector a) = Property
  equal x y = property (x == y)

instance (Eq a, DVU.Unbox a) => TestData (DVU.Vector a) where
  type Model (DVU.Vector a) = [a]
  model = DVU.toList
  unmodel = DVU.fromList

  type EqTest (DVU.Vector a) = Property
  equal x y = property (x == y)

#define id_TestData(ty) \
instance TestData ty where { \
  type Model ty = ty;        \
  model = id;                \
  unmodel = id;              \
                             \
  type EqTest ty = Property; \
  equal x y = property (x == y) }

id_TestData(())
id_TestData(Bool)
id_TestData(Int)
id_TestData(Float)
id_TestData(Double)
id_TestData(Ordering)

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance (Eq a, TestData a) => TestData (Maybe a) where
  type Model (Maybe a) = Maybe (Model a)
  model = fmap model
  unmodel = fmap unmodel

  type EqTest (Maybe a) = Property
  equal x y = property (x == y)

instance (Eq a, TestData a) => TestData [a] where
  type Model [a] = [Model a]
  model = fmap model
  unmodel = fmap unmodel

  type EqTest [a] = Property
  equal x y = property (x == y)

instance (Eq a, TestData a) => TestData (Identity a) where
  type Model (Identity a) = Identity (Model a)
  model = fmap model
  unmodel = fmap unmodel

  type EqTest (Identity a) = Property
  equal = (property .) . on (==) runIdentity

instance (Eq a, TestData a, Monoid a) => TestData (Writer a ()) where
  type Model (Writer a ()) = Writer (Model a) ()
  model = mapWriter model
  unmodel = mapWriter unmodel

  type EqTest (Writer a ()) = Property
  equal = (property .) . on (==) execWriter

instance (Eq a, Eq b, TestData a, TestData b) => TestData (a,b) where
  type Model (a,b) = (Model a, Model b)
  model (a,b) = (model a, model b)
  unmodel (a,b) = (unmodel a, unmodel b)

  type EqTest (a,b) = Property
  equal x y = property (x == y)

instance (Eq a, Eq b, Eq c, TestData a, TestData b, TestData c) => TestData (a,b,c) where
  type Model (a,b,c) = (Model a, Model b, Model c)
  model (a,b,c) = (model a, model b, model c)
  unmodel (a,b,c) = (unmodel a, unmodel b, unmodel c)

  type EqTest (a,b,c) = Property
  equal x y = property (x == y)

instance (Eq a, Eq b, Eq c, Eq d, Eq e, TestData a, TestData b, TestData c, TestData d, TestData e) => TestData (a,b,c,d,e) where
  type Model (a,b,c,d,e) = (Model a, Model b, Model c, Model d, Model e)
  model (a,b,c,d,e) = (model a, model b, model c, model d, model e)
  unmodel (a,b,c,d,e) = (unmodel a, unmodel b, unmodel c, unmodel d, unmodel e)

  type EqTest (a,b,c,d,e) = Property
  equal x y = property (x == y)

instance (Arbitrary a, Show a, TestData a, TestData b) => TestData (a -> b) where
  type Model (a -> b) = Model a -> Model b
  model f = model . f . unmodel
  unmodel f = unmodel . f . model

  type EqTest (a -> b) = a -> EqTest b
  equal f g x = equal (f x) (g x)

newtype P a = P { unP :: EqTest a }

instance TestData a => Testable (P a) where
  property (P a) = property a

infix 4 `eq`
eq :: TestData a => a -> Model a -> P a
eq x y = P (equal x (unmodel y))

class Conclusion p where
  type Predicate p

  predicate :: Predicate p -> p -> p

instance Conclusion Property where
  type Predicate Property = Bool

  predicate = (==>)

instance Conclusion p => Conclusion (a -> p) where
  type Predicate (a -> p) = a -> Predicate p

  predicate f p = \x -> predicate (f x) (p x)

