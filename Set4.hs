{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

newtype Gen a = Gen {runGen :: Seed -> (a, Seed)}

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) 
  where show Nothing = "Nothing"
        show (Just a) = "Just " ++ show a

class Monad m where
  return :: a -> m a
  bind :: m a -> (a -> m b) -> m b

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind
  
join :: Monad m => m (m a) -> m a
join = (=<<) id 

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f = (=<<) (return . f)
  
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ap ((return . f) =<< ma) mb

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = (\x -> liftM2 x mb mc) =<< ((return . f) =<< ma)

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = (\x -> (return . x) =<< ma) =<< mf

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = liftM2 (++) ((return . (:[])) =<< x) (sequence xs)

instance Monad [] where
  return = (:[])
  bind = flip concatMap

instance Monad Maybe where
  return = Just
  bind = link

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f (Just x) = f x
chain _ _ = Nothing

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay k (x:xs) = 
  if k == fst x 
    then Just (snd x)
  else
    lookupMay k xs 

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just (foldr max x xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just (foldr min x xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd str = join $ liftM2 divMay (liftM fromInteger h) (liftM fromInteger m)
  where
    l = lookupMay str gd
    m = maximumMay =<< l
    h = headMay =<< l


instance Monad Gen where
  return = mkGen
  bind = genTwo

evalGen :: Gen a -> Seed -> a
evalGen g = fst . runGen g

mkGen :: a -> Gen a
mkGen a = Gen ((,) a)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gen f = Gen (uncurry (runGen . f) . runGen gen)

randFive :: [Integer]
randFive = evalGen (sequence (replicate 5 (Gen rand))) (mkSeed 1)

randLetter :: Gen Char
randLetter = liftM toLetter (Gen rand)

randString3 :: String
randString3 = evalGen (sequence (replicate 3 (randLetter))) (mkSeed 1)

randEven :: Gen Integer -- the output of rand * 2
randEven = liftM (*2) (Gen rand)

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = liftM (\x -> x * 2 + 1) (Gen rand)

randTen :: Gen Integer -- the output of rand * 10
randTen = liftM (*10) (Gen rand)

randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter (Gen rand)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair = liftM2 (,)

generalA :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalA = liftM2

generalB :: (a -> b -> c -> d) -> Gen a -> Gen b -> Gen c -> Gen d
generalB = liftM3

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence