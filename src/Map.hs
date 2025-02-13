-- Map - functional Map implementation
-- Authors: Samuel Roland and Timothée Van Hove
-- Description:
--   This module provides a functional implementation of a Map (key-value store).
--   The Map is implemented as a function that maps a character to an optional integer (Maybe Int),
--   where a character not present in the map returns Nothing.

-- Goal:
--   This implementation aims to provide an immutable map that supports standard
--   set operations (such as union, intersection, and difference) and map operations
--   (insert, delete, find, etc.). Set operations on the Map require both the key and value
--   to match between maps for entries to be considered equal.

-- Implementation choices:
-- 1. Map keys order:
--   - When iterating over the map (such as when using keys, values, or displaying with Show),
--     the keys follow the ASCII range order of characters (['\x00' .. '\x7F']).
--     This order is independent of the insertion order of the keys.
--   - This impacts functions like Show, keys, and values, which display or return keys
--     in ASCII order, not based on the order in which keys were inserted into the map.
-- 2. Set operations like union, intersection, and difference:
--   - Union: When combining two maps with union, the resulting map contains all key-value pairs from both maps.
--          If the same key exists in both maps, the value from the first map is retained in the result.
--   - Intersection: The intersection of two maps returns a map containing only the key-value pairs
--          that are present in both maps with the same value. If a key exists in both maps but the values differ,
--          that key is excluded from the intersection.
--   - Difference: The difference between two maps returns the key-value pairs that are in the first map but not
--          in the second map. If a key exists in both maps but has the same value, it is excluded from the result.
--          If the values differ, the key-value pair from the first map is retained.

-- Tests: See MapTests.hs.
--      First install HUnit: `cabal install --lib HUnit`
--      Then run tests: `ghc --run MapTests.hs`

module Map where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe

type Map = Char -> Maybe Int

-- The range of supported chars in our Map keys, the first 128 code points
asciiRange :: [Char]
asciiRange = ['\x00' .. '\x7F']

-- Implement Show for Map
-- It will prints the map in "{'a' -> 1, 'b' -> 2}"
instance Show Map where
    show :: Map -> String
    show map =
        "{"
            ++ intercalate ", " ["'" ++ [key] ++ "' -> " ++ show value | (key, value) <- toList map]
            ++ "}"

-- Implement Eq for Map
-- Maps are equal when their string representation are equal
instance Eq Map where
    (==) :: Map -> Map -> Bool
    x == y = show x == show y

-- 1 | Empty map
empty :: Map
empty _ = Nothing

-- 2 | Singleton map
singleton :: Char -> Int -> Map
singleton key value k
    | k == key = Just value
    | otherwise = Nothing

-- 3 | Insert a key-value pair into the map
insert :: Char -> Int -> Map -> Map
insert key value map x
    | key == x = Just value
    | otherwise = map x

-- 4 | Find the value associated with a key in the map
find :: Char -> Map -> Maybe Int
find key map = map key

-- 5 | Find the value associated with a key in the map or return a default value
findOrDefault :: Char -> Int -> Map -> Int
findOrDefault key def map = fromMaybe def (map key)

-- 6 | Delete a key from the map
delete :: Char -> Map -> Map
delete key map k
    | k == key = Nothing
    | otherwise = map k

-- 7 | Update the value associated with a key in the map
update :: Char -> (Int -> Int) -> Map -> Map
update key updater map x = case (key == x, map key) of
    (True, Just v) -> Just (updater v)
    _ -> map x

-- 8 | Check if a key exists in the map
member :: Char -> Map -> Bool
member key map = isJust (map key)

-- 9 | Get the size of the map
size :: Map -> Int
size map = length [key | key <- asciiRange, isJust (map key)]

-- 10 | Check if the map is empty
isEmpty :: Map -> Bool
isEmpty map = size map == 0

-- 11 | Get all keys in the map
keys :: Map -> [Char]
keys map = [key | key <- asciiRange, isJust (map key)]

-- 12 | Get all values in the map
values :: Map -> [Int]
values map = catMaybes [map key | key <- asciiRange]

-- 13 | Get the keys associated with a given value
keysWithValue :: Int -> Map -> [Char]
keysWithValue value map = [key | key <- asciiRange, map key == Just value]

-- 14 | Union of two maps
union :: Map -> Map -> Map
union map1 map2 key = case map1 key of
    Just val -> Just val
    _ -> map2 key

-- 15 | Intersection of two maps
intersection :: Map -> Map -> Map
intersection map1 map2 key = case (map1 key, map2 key) of
    (Just v1, Just v2) | v1 == v2 -> Just v1
    _ -> Nothing

-- 16 | Difference of two maps
difference :: Map -> Map -> Map
difference map1 map2 key
    | isNothing (map1 key) = Nothing
    | map1 key == map2 key = Nothing
    | otherwise = map1 key

-- 17 | Apply a function to all values in the map
mapValues :: (Int -> Int) -> Map -> Map
mapValues updater map key = case map key of
    Just v -> Just (updater v)
    _ -> Nothing

-- 18 | Apply a function to all keys in the map
mapKeys :: (Char -> Char) -> Map -> Map
mapKeys f map = fromList [(f key, value) | key <- keys map, Just value <- [map key]]

-- 19 | Filter the map by a predicate
filterMap :: (Char -> Int -> Bool) -> Map -> Map
filterMap pred map key = case map key of
    Just v -> if pred key v then Just v else Nothing
    _ -> Nothing

-- 20 | Check if any entry in the map satisfies a predicate
anyEntry :: (Char -> Int -> Bool) -> Map -> Bool
anyEntry p = not . isEmpty . filterMap p

-- 21 | Check if all entries in the map satisfy a predicate
allEntries :: (Char -> Int -> Bool) -> Map -> Bool
allEntries p map = size map == size (filterMap p map)

-- 22 | Partition the map into two maps based on a predicate
partition :: (Char -> Int -> Bool) -> Map -> (Map, Map)
partition p map = (filterMap p map, filterMap (\key val -> not (p key val)) map)

-- 23 | Split the map into two maps at a given key
split :: Char -> Map -> (Map, Map)
split c map = (filterMap (\key _ -> key <= c) map, filterMap (\key _ -> key > c) map)

-- 24 | Get the maximum value in the map
findMax :: Map -> Maybe Int
findMax map = case values map of
    [] -> Nothing
    values -> Just (maximum values)

-- 25 | Get the minimum value in the map
findMin :: Map -> Maybe Int
findMin map = case values map of
    [] -> Nothing
    values -> Just (minimum values)

-- 26 | Convert a list of key-value pairs to a map
fromList :: [(Char, Int)] -> Map
fromList = foldr (uncurry insert) empty

-- 27 | Convert the map to a list of key-value pairs
toList :: Map -> [(Char, Int)]
toList map = [(key, value) | key <- asciiRange, Just value <- [map key]]

-- 28 | Deserialize a map from a string
fromString :: String -> Map
fromString "" = error "Invalid format: cannot parse empty string"
fromString xs
    | head xs /= '{' || last xs /= '}' = error "Invalid format: missing braces"
    | otherwise = fromString' (tail (init xs)) empty -- Skip the outer braces '{' and '}'
  where
    fromString' :: String -> Map -> Map
    fromString' [] map = map -- End of string
    fromString' (x : xs) map
        | x == ',' = fromString' xs map -- Skip comma and move to the next entry
        | x /= ' ' = extractElement (x : xs) map -- Process the next key-value pair
        | otherwise = fromString' xs map -- Skip spaces
    extractElement :: String -> Map -> Map
    extractElement ('\'' : k : '\'' : ' ' : '-' : '>' : ' ' : v) map =
        insert k (read (takeWhile (/= ',') v) :: Int) (extractElement (dropWhile isDigit v) map)
    extractElement "" map = map
    extractElement (',' : ' ' : ',' : xs) map = error ("Invalid format in between entries " ++ xs)
    extractElement (',' : ' ' : xs) map = extractElement xs map
    extractElement _ map = error ("Invalid format in " ++ xs)
