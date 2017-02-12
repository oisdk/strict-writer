{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DefaultSignatures #-}

module Main (main) where

import           Test.DocTest
import           Test.QuickCheck

import           Control.Monad.Writer.Stricter

import           Data.Functor.Identity
import           Control.Monad

import           Numeric.Natural
import           Data.Monoid



class Arbitrary1 f where
  liftArbitrary :: Gen a -> Gen (f a)
  default liftArbitrary :: Applicative f => Gen a -> Gen (f a)
  liftArbitrary = fmap pure

instance Arbitrary1 Identity
instance Arbitrary1 [] where
  liftArbitrary = sized . flip replicateM

instance (Arbitrary1 m, Monoid w, Arbitrary w, Arbitrary a, Functor m) =>
         Arbitrary (WriterT w m a) where
    arbitrary = fmap WriterT (liftArbitrary arbitrary)

{-# ANN fmapLaw "HLint: ignore Functor law" #-}
fmapLaw :: WriterT String [] Integer -> Property
fmapLaw xs = fmap id xs === xs

{-# ANN fmapCompLaw "HLint: ignore Functor law" #-}
fmapCompLaw
    :: Blind (Integer -> Integer)
    -> Blind (Integer -> Integer)
    -> WriterT String [] Integer
    -> Property
fmapCompLaw (Blind f) (Blind g) xs = fmap (f . g) xs === (fmap f . fmap g) xs

appIdLaw :: WriterT String [] Integer -> Property
appIdLaw xs = (pure id <*> xs) === xs

appCompLaw
    :: Blind (WriterT String [] (Natural -> Integer))
    -> Blind (WriterT String [] (Word -> Natural))
    -> WriterT String [] Word
    -> Property
appCompLaw (Blind u) (Blind v) w
  = (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

homomorphismLaw :: Blind (Natural -> Integer) -> Natural -> Property
homomorphismLaw (Blind f) x
  = (pure f <*> pure x) === (pure (f x) :: WriterT String [] Integer)

interchangeLaw :: Blind (WriterT String [] (Natural -> Integer))
               -> Natural
               -> Property
interchangeLaw (Blind u) y = (u <*> pure y) === (pure ($y) <*> u)

monadLawOne :: Blind (Natural -> WriterT String [] Integer)
            -> Natural
            -> Property
monadLawOne (Blind k) a = (pure a >>= k) === k a

monadLawTwo :: WriterT String [] Integer -> Property
monadLawTwo xs = (xs >>= pure) === xs

monadLawThree :: WriterT String [] Word
              -> Blind (Word -> WriterT String [] Natural)
              -> Blind (Natural -> WriterT String [] Integer)
              -> Property
monadLawThree m (Blind k) (Blind h) =
  (m >>= (k Control.Monad.>=> h)) === ((m >>= k) >>= h)

reflexiveEq :: WriterT String [] Integer -> Property
reflexiveEq xs = xs === xs

longSum :: Property
longSum =
    once $
    getSum
        (execWriter
             (traverse
                  (\x ->
                        Writer ((), Sum x))
                  [1 .. 100000000])) ===
    (5000000050000000 :: Integer)

main :: IO ()
main = do
  quickCheck longSum
  quickCheck fmapLaw
  quickCheck fmapCompLaw
  quickCheck appIdLaw
  quickCheckWith stdArgs { maxSize = 25 } appCompLaw
  quickCheck homomorphismLaw
  quickCheck interchangeLaw
  quickCheck monadLawOne
  quickCheck monadLawTwo
  quickCheckWith stdArgs { maxSize = 25 } monadLawThree
  quickCheck reflexiveEq
  doctest ["-isrc", "src/Control/Monad/Writer/Stricter.hs"]
