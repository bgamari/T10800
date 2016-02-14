{-# LANGUAGE CPP, RankNTypes, TemplateHaskell, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module Tests.Vector (tests) where

import Boilerplater
import Utilities as Util

import qualified Data.Vector.Generic as V
import qualified Data.Vector
import qualified Data.Vector.Primitive
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Fusion.Bundle as S

import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Text.Show.Functions ()
import Data.List
import Data.Monoid
import qualified Control.Applicative as Applicative
import System.Random       (Random)

import Data.Functor.Identity
import Control.Monad.Trans.Writer

#define COMMON_CONTEXT(a, v) \
 VANILLA_CONTEXT(a, v), VECTOR_CONTEXT(a, v)

#define VANILLA_CONTEXT(a, v) \
  Eq a,     Show a,     Arbitrary a,     CoArbitrary a,     TestData a,     Model a ~ a,        EqTest a ~ Property

#define VECTOR_CONTEXT(a, v) \
  Eq (v a), Show (v a), Arbitrary (v a), CoArbitrary (v a), TestData (v a), Model (v a) ~ [a],  EqTest (v a) ~ Property, V.Vector v a

testSanity :: forall a v. (COMMON_CONTEXT(a, v)) => v a -> [Test]
testSanity _ = []

testPolymorphicFunctions :: forall a v. (COMMON_CONTEXT(a, v), VECTOR_CONTEXT(Int, v)) => v a -> [Test]
testPolymorphicFunctions _ = []

testTuplyFunctions:: forall a v. (COMMON_CONTEXT(a, v), VECTOR_CONTEXT((a, a), v), VECTOR_CONTEXT((a, a, a), v)) => v a -> [Test]
testTuplyFunctions _ = $(testProperties ['prop_zip, 'prop_zip3, 'prop_unzip, 'prop_unzip3])
  where
    prop_zip    :: P (v a -> v a -> v (a, a))           = V.zip `eq` zip
    prop_zip3   :: P (v a -> v a -> v a -> v (a, a, a)) = V.zip3 `eq` zip3
    prop_unzip  :: P (v (a, a) -> (v a, v a))           = V.unzip `eq` unzip
    prop_unzip3 :: P (v (a, a, a) -> (v a, v a, v a))   = V.unzip3 `eq` unzip3

testOrdFunctions :: forall a v. (COMMON_CONTEXT(a, v), Ord a, Ord (v a)) => v a -> [Test]
testOrdFunctions _ = []

testEnumFunctions :: forall a v. (COMMON_CONTEXT(a, v), Enum a, Ord a, Num a, Random a) => v a -> [Test]
testEnumFunctions _ = []

testMonoidFunctions :: forall a v. (COMMON_CONTEXT(a, v), Monoid (v a)) => v a -> [Test]
testMonoidFunctions _ = []

testFunctorFunctions :: forall a v. (COMMON_CONTEXT(a, v), Functor v) => v a -> [Test]
testFunctorFunctions _ = []

testMonadFunctions :: forall a v. (COMMON_CONTEXT(a, v), Monad v) => v a -> [Test]
testMonadFunctions _ = []

testApplicativeFunctions :: forall a v. (COMMON_CONTEXT(a, v), V.Vector v (a -> a), Applicative.Applicative v) => v a -> [Test]
testApplicativeFunctions _ = []

testAlternativeFunctions :: forall a v. (COMMON_CONTEXT(a, v), Applicative.Alternative v) => v a -> [Test]
testAlternativeFunctions _ = []

testBoolFunctions :: forall v. (COMMON_CONTEXT(Bool, v)) => v Bool -> [Test]
testBoolFunctions _ = []

testNumFunctions :: forall a v. (COMMON_CONTEXT(a, v), Num a) => v a -> [Test]
testNumFunctions _ = []

testNestedVectorFunctions :: forall a v. (COMMON_CONTEXT(a, v)) => v a -> [Test]
testNestedVectorFunctions _ = []

testGeneralBoxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Vector), Ord a) => Data.Vector.Vector a -> [Test]
testGeneralBoxedVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testTuplyFunctions,
        testNestedVectorFunctions,
        testMonoidFunctions,
        testFunctorFunctions,
        testMonadFunctions,
        testApplicativeFunctions,
        testAlternativeFunctions
    ]

testBoolBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testBoolFunctions
  ]

testNumericBoxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Vector), Ord a, Num a, Enum a, Random a) => Data.Vector.Vector a -> [Test]
testNumericBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testNumFunctions
  , testEnumFunctions
  ]


testGeneralPrimitiveVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Primitive.Vector), Data.Vector.Primitive.Prim a, Ord a) => Data.Vector.Primitive.Vector a -> [Test]
testGeneralPrimitiveVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testMonoidFunctions
    ]

testNumericPrimitiveVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Primitive.Vector), Data.Vector.Primitive.Prim a, Ord a, Num a, Enum a, Random a) => Data.Vector.Primitive.Vector a -> [Test]
testNumericPrimitiveVector dummy = concatMap ($ dummy)
 [
   testGeneralPrimitiveVector
 , testNumFunctions
 , testEnumFunctions
 ]


testGeneralStorableVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Storable.Vector), Data.Vector.Storable.Storable a, Ord a) => Data.Vector.Storable.Vector a -> [Test]
testGeneralStorableVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testMonoidFunctions
    ]

testNumericStorableVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Storable.Vector), Data.Vector.Storable.Storable a, Ord a, Num a, Enum a, Random a) => Data.Vector.Storable.Vector a -> [Test]
testNumericStorableVector dummy = concatMap ($ dummy)
  [
    testGeneralStorableVector
  , testNumFunctions
  , testEnumFunctions
  ]


testGeneralUnboxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Unboxed.Vector), Data.Vector.Unboxed.Unbox a, Ord a) => Data.Vector.Unboxed.Vector a -> [Test]
testGeneralUnboxedVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testMonoidFunctions
    ]

testUnitUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

testBoolUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testBoolFunctions
  ]

testNumericUnboxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Unboxed.Vector), Data.Vector.Unboxed.Unbox a, Ord a, Num a, Enum a, Random a) => Data.Vector.Unboxed.Vector a -> [Test]
testNumericUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

testTupleUnboxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Unboxed.Vector), Data.Vector.Unboxed.Unbox a, Ord a) => Data.Vector.Unboxed.Vector a -> [Test]
testTupleUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

tests = [
        testGroup "Data.Vector.Vector (Bool)"           (testBoolBoxedVector      (undefined :: Data.Vector.Vector Bool)),
        testGroup "Data.Vector.Vector (Int)"            (testNumericBoxedVector   (undefined :: Data.Vector.Vector Int)),

        testGroup "Data.Vector.Primitive.Vector (Int)"    (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Int)),
        testGroup "Data.Vector.Primitive.Vector (Double)" (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Double)),

        testGroup "Data.Vector.Storable.Vector (Int)"    (testNumericStorableVector (undefined :: Data.Vector.Storable.Vector Int)),
        testGroup "Data.Vector.Storable.Vector (Double)" (testNumericStorableVector (undefined :: Data.Vector.Storable.Vector Double)),

        testGroup "Data.Vector.Unboxed.Vector ()"       (testUnitUnboxedVector (undefined :: Data.Vector.Unboxed.Vector ())),
        testGroup "Data.Vector.Unboxed.Vector (Int)"    (testNumericUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Int)),
        testGroup "Data.Vector.Unboxed.Vector (Double)" (testNumericUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Double)),
       testGroup "Data.Vector.Unboxed.Vector (Int,Bool)" (testTupleUnboxedVector (undefined :: Data.Vector.Unboxed.Vector (Int,Bool))),
         testGroup "Data.Vector.Unboxed.Vector (Int,Bool,Int)" (testTupleUnboxedVector (undefined :: Data.Vector.Unboxed.Vector (Int,Bool,Int)))

    ]

