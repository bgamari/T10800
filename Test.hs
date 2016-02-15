{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module Tests.Vector (test1) where

import Utilities as Util

import qualified Data.Vector.Generic as V
import qualified Data.Vector
import Data.List (zip5, unzip5)

import Test.QuickCheck

import Text.Show.Functions ()
import System.Random       (Random)

#define COMMON_CONTEXT(a, v) \
 VANILLA_CONTEXT(a, v), VECTOR_CONTEXT(a, v)

#define VANILLA_CONTEXT(a, v) \
  Eq a,     Show a,     Arbitrary a,     CoArbitrary a,     TestData a,     Model a ~ a,        EqTest a ~ Property

#define VECTOR_CONTEXT(a, v) \
  Eq (v a), Show (v a), Arbitrary (v a), CoArbitrary (v a), TestData (v a), Model (v a) ~ [a],  EqTest (v a) ~ Property, V.Vector v a

testTuplyFunctions :: forall a v. (v ~ Data.Vector.Vector, COMMON_CONTEXT(a, v), VECTOR_CONTEXT((a, a), v), VECTOR_CONTEXT((a, a, a), v), VECTOR_CONTEXT((a,a,a,a,a), v))
                   => v a -> [Property]
testTuplyFunctions _ = [ property prop_zip
                       -- , property prop_zip3
                       -- , property prop_zip5
                       -- , property prop_unzip
                       -- , property prop_unzip3
                       ]
  where
    prop_zip    :: P (v a -> v a -> v (a, a))           = V.zip `eq` zip
    prop_zip3   :: P (v a -> v a -> v a -> v (a, a, a)) = V.zip3 `eq` zip3
    prop_zip5   :: P (v a -> v a -> v a -> v a -> v a -> v (a, a, a, a, a)) = V.zip5 `eq` zip5
    prop_unzip  :: P (v (a, a) -> (v a, v a))           = V.unzip `eq` unzip
    prop_unzip3 :: P (v (a, a, a) -> (v a, v a, v a))   = V.unzip3 `eq` unzip3
    prop_unzip5 :: P (v (a, a, a, a, a) -> (v a, v a, v a, v a, v a))   = V.unzip5 `eq` unzip5

testGeneralBoxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Vector), Ord a) => Data.Vector.Vector a -> [Property]
testGeneralBoxedVector dummy = concatMap ($ dummy) [testTuplyFunctions]

testBoolBoxedVector dummy = concatMap ($ dummy) [testGeneralBoxedVector]

--testNumericBoxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Vector), Ord a, Num a, Enum a, Random a) => Data.Vector.Vector a -> [Property]
--testNumericBoxedVector dummy = concatMap ($ dummy) [testGeneralBoxedVector]


test1 = testBoolBoxedVector      (undefined :: Data.Vector.Vector Bool) -- 5 seconds
--test2 = testNumericBoxedVector   (undefined :: Data.Vector.Vector Int) -- 4.5 seconds
