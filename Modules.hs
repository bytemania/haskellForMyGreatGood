module Modules where

import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

search :: Eq a => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)


encode :: Int -> String -> String
encode shift = map (chr . (+) shift . ord)

decode :: Int -> String -> String
decode shift = encode (negate shift)

phoneBook :: [(String, String)]
phoneBook =   [("betty","555-2938")
                ,("bonnie","452-2928")
                ,("patsy","493-2928")
                ,("lucille","205-2928")
                ,("wendy","939-8282")
                ,("penny","853-2492")
                ]

findKey :: Eq k => k -> [(k, v)] -> Maybe v
findKey k = fmap snd . find ((==) k . fst)

fromList' :: Ord k => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

phoneBookMultiple =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: Ord k => [(k, a)] -> Map.Map k [a]
phoneBookToMap = Map.fromListWith  (++) . map (\(k, v) -> (k, [v]))

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

