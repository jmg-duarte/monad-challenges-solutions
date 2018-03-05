{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = let (i0, s0) = rand (mkSeed 1) in
            let (i1, s1) = rand s0 in
            let (i2, s2) = rand s1 in
            let (i3, s3) = rand s2 in
            let (i4, _) = rand s3 in
                [i4, i3, i2, i1, i0]

randLetter :: Gen Char
randLetter = generalA toLetter rand
--randLetter seed = let (i, s) = rand seed in ((toLetter i), s)

randString3 :: String

randString3 = let (l0, s0) = randLetter $ mkSeed 1 in
              let (l1, s1) = randLetter s0 in
              let (l2, _) = randLetter s1 in
                [l0, l1, l2]

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (* 2) rand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA (\x -> x * 2 + 1) rand

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (* 10) rand

randTwenty :: Gen Integer
randTwenty = generalA (* 20) rand

randPair :: Gen (Char, Integer)
randPair seed = ((c, i), s1)
  where
    (c, s0) = randLetter seed
    (i, s1) = rand s0

repRandom :: [Gen a] -> Gen [a]
repRandom [] s = mkGen [] s
repRandom (x:xs) s = (v0 : v1, s1)
  where 
    (v0, s0) = x s
    (v1, s1) = repRandom xs s0

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gen f = (\(x, y) -> f x y) . gen

test :: Integer -> Gen [Integer]
test v = mkGen [v]

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair f g = pairValues . applyOnSeed g . f

generalPair' :: Gen a -> Gen b -> Gen (a, b)
generalPair' f g = generalB (,) f g

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g = applyOnVal f . g

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB x g1 g2 s = (x v1 v2, s2) 
  where 
    (v1, s1) = g1 s
    (v2, s2) = g2 s1

mkGen :: a -> Gen a
mkGen a = (\x -> (a, x))

pairValues :: (a, (b, c)) -> ((a, b), c)
pairValues (x, (y, z)) = ((x, y), z) 

applyOnSeed :: Gen b -> (a, Seed) -> (a, (b, Seed))
applyOnSeed f (x, y) = (x, f y)

applyOnVal :: (a -> b) -> (a, Seed) -> (b, Seed)
applyOnVal f (x, y) = (f x, y)