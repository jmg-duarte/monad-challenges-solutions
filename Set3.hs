{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

data Card = Card Integer String 

instance Show Card
  where show (Card i s) = (show i :: String) ++ s

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) l =
  (map ((,) x) l) ++ (allPairs xs l)

allCards :: [Integer] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) l =
  (map (Card x) l) ++ (allCards xs l)

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (x:xs) l1 = 
  (map (f x) l1) ++ (allCombs f xs l1)

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' _ [] _ = []
allCombs' _ _ [] = []
allCombs' f l0@(x:xs) l1@(y:ys) =
  combStep (map f l0) l1

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 _ _ [] _ = []
allCombs3 _ _ _ [] = []
allCombs3 f l0@(h:t) l1 l2 = (auxY l1) ++ (allCombs3 f t l1 l2)
  where 
    auxY [] = []
    auxY (y:ys) = 
      (map (f h y) l2) ++ (auxY ys)

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' _ [] _ _ = []
allCombs3' _ _ [] _ = []
allCombs3' _ _ _ [] = []
allCombs3' f x y z = 
  combStep (combStep (map f x) y) z

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep _ [] = []
combStep fl@(fh:ft) l =
  (map fh l) ++ (combStep ft l)