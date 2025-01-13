module Utils.Conversions
  ( eitherToMaybe,
    maybeToEither,
    maximumByMay,
    safeHead,
    safeLast,
    mapMaybe,
    isRight,
    isLeft,
    fromRight,
    fromLeft,
    onJust,
    listToMaybe,
    maybeToList,
  )
where

import Data.List (maximumBy)

-- Convert Either to Maybe
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _ = Nothing

-- Convert Maybe to Either
maybeToEither :: l -> Maybe r -> Either l r
maybeToEither leftValue Nothing = Left leftValue
maybeToEither _ (Just rightValue) = Right rightValue

-- Safely get the maximum element by a comparator
maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay _ [] = Nothing
maximumByMay cmp xs = Just (maximumBy cmp xs)

-- Safely get the first element of a list
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- Safely get the last element of a list
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- Map over a list, filtering out Nothing values
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs

-- Check if an Either value is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- Check if an Either value is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Extract the value from a Right, or return a default value
fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight defaultValue _ = defaultValue

-- Extract the value from a Left, or return a default value
fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft defaultValue _ = defaultValue

-- Apply a function to a Maybe value if it exists
onJust :: Maybe a -> (a -> b) -> Maybe b
onJust Nothing _ = Nothing
onJust (Just x) f = Just (f x)

-- Convert a list to a Maybe (first element or Nothing)
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

-- Convert a Maybe to a list (Just becomes a singleton list)
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
