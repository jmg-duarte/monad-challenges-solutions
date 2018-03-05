{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) 
  where show Nothing = "Nothing"
        show (Just a) = "Just " ++ show a


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
queryGreek gd str = let nums = foldr (++) [] $ map snd $ filter (\x -> str == fst x) gd in
  case tailMay nums of
    Nothing -> Nothing
    Just x -> 
      case maximumMay x of
        Nothing -> Nothing
        Just y -> 
          case headMay nums of
            Nothing -> Nothing
            Just z -> divMay (fromIntegral y :: Double) (fromIntegral z :: Double)

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd str = squish ((Just divMay) `crazy` t `crazy` h)
  where
    nums = foldr ((++) . snd) [] $ filter (\x -> str == fst x) gd
    t = intMaybeDouble `chain` (maximumMay `chain` (tailMay nums))
    h = intMaybeDouble `chain` (headMay nums)

squish :: Maybe (Maybe a) -> Maybe a
squish (Just x) = x
squish _ = Nothing

crazy :: Maybe (a -> b) -> Maybe a -> Maybe b
crazy (Just f) (Just a) = Just (f a) 
crazy _ _ = Nothing

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries [] _ _ = Nothing
addSalaries m p1 p2
  | p1 == p2 = Nothing
  | p1 /= p2 =
    case (filter (\x -> p1 == fst x || p2 == fst x) m) of 
      [] -> Nothing
      [x] -> Nothing
      x -> Just $ foldr ((+) . snd) 0 x

tailProd :: Num a => [a] -> Maybe a
tailProd [] = Nothing
tailProd [x] = Just x
tailProd l = maybeMap (foldr (*) 1) (tailMay l)

tailSum :: Num a => [a] -> Maybe a
tailSum [] = Nothing
tailSum [x] = Just x
tailSum l = maybeMap (foldr (+) 0) (tailMay l)

-- According to Monad challenges the signature should be
-- tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax :: Ord a => [a] -> Maybe a
tailMax [] = Nothing
tailMax [x] = Just x
tailMax l = squish $ maybeMap maximumMay (tailMay l)

tailMin :: Ord a => [a] -> Maybe a
tailMin [] = Nothing
tailMin [x] = Just x
tailMin l = squish $ maybeMap minimumMay (tailMay l)

maybeMap :: (a -> b) -> Maybe a -> Maybe b 
maybeMap f (Just x) = Just (f x)
maybeMap _ Nothing = Nothing

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f (Just x) (Just y) = (Just $ f x y)
yLink f _ _ = Nothing 

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f m = 
  case m of
    Nothing -> Nothing
    Just x -> f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

intMaybeDouble :: Integer -> Maybe Double
intMaybeDouble = (Just . fromIntegral)