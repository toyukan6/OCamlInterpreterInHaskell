module Environment where

import qualified Data.Map as Map (Map, empty, insert, lookup, map, foldr)

type Environment = Map.Map String

empty :: Environment a
empty = Map.empty

insert :: String -> a -> Environment a -> Environment a
insert k v m = Map.insert k v m

lookup :: String -> Environment a -> Maybe a
lookup k m = Map.lookup k m

map :: (a -> b) -> Environment a -> Environment b
map f m = Map.map f m

foldr :: (a -> b -> b) -> b -> Environment a -> b
foldr f i m = Map.foldr f i m