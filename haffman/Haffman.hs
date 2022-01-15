{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Map (fromListWith, toList)
import Data.ByteString (intercalate)
import System.Directory (doesFileExist)
import Data.ByteString.Char8 (unpack, pack)

import qualified Data.ByteString.Char8 as BSC
import qualified System.Directory.Internal.Prelude as P
import Data.List (sortBy, find, isPrefixOf)


frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- Map:

type MapToCode  = [(Char,String)]
type MapToChar  = [(String,Char)]

getByKey :: (Eq a, Read b) => a  -> [(a,b)] -> Maybe b
getByKey key mapToChar = case find (\x -> fst x == key) mapToChar of
  Nothing -> Nothing
  Just pair -> Just (snd pair)

expandMayBe:: Read a => Maybe a -> a
expandMayBe (Just a) = a
expandMayBe Nothing = error "Sorry, can't find key, something goes wrong!!!"

reversePair:: [(Char,String)] -> [(String, Char)]
reversePair = map (\(a, b) -> (b,a) )


-- Tree
data MyTree a = MyNode Char Int
              | MyFilledNode Int (MyTree a) (MyTree a)
              deriving (Eq,Ord,Show,Read)

-- Merge tree nodes
(+++) :: MyTree a -> MyTree a -> MyTree a
tree1@(MyFilledNode x1 l1 r1) +++ tree2@(MyFilledNode x2 l2 r2) = MyFilledNode (x1 + x2) tree1 tree2
tree1@(MyNode c1 n1) +++ tree2@(MyNode c2 n2 ) = MyFilledNode (n1 + n2) tree1 tree2
tree1@(MyFilledNode n1 l1 r1) +++ tree2@(MyNode c2 n2 ) = MyFilledNode (n1 + n2) tree1 tree2
tree1@(MyNode c2 n2 ) +++ tree2@(MyFilledNode n1 l1 r1) = MyFilledNode (n1 + n2) tree1 tree2

buildHaffmanTree :: [MyTree a] -> MyTree a
buildHaffmanTree (t:[]) = t
buildHaffmanTree (a:b:cs) = buildHaffmanTree ( sortBy (compare `P.on` getFrequencyOfNode) ( (a +++ b ) : cs))

getFrequencyOfNode :: MyTree a  -> Int
getFrequencyOfNode (MyNode c n ) = n
getFrequencyOfNode (MyFilledNode n leftNode rightNode) = n

flattenThree :: MyTree a -> [(Char, String)]
flattenThree a = flattenThreeHelper a ""

flattenThreeHelper :: MyTree a -> String -> [(Char, String)]
flattenThreeHelper (MyNode c n) path = [(c, path)]
flattenThreeHelper (MyFilledNode n leftNode rightNode) path = flattenThreeHelper leftNode (path ++ "0")  ++ flattenThreeHelper rightNode (path ++ "1")

-- Encode and Decode 

decodeArray:: [Char] -> MapToChar ->  [Char]
decodeArray  [] _ = []
decodeArray array toChar = symbol : decodeArray rest toChar
    where (symbol, rest ) = decodeArrayhelper array toChar

decodeArrayhelper :: [Char] -> MapToChar ->  (Char, [Char])
decodeArrayhelper _ [] = error "incorrect "
decodeArrayhelper array (a:cs) = if isPrefixOf (fst a) array
    then (snd a, drop l array)
    else decodeArrayhelper array cs
    where l = length (fst a)



haffman :: IO ()
haffman  = do
            putStr "Enter file name: "
            fileName <- getLine
            fileExists <- doesFileExist fileName
            if fileExists
                then do
                        inputText <- BSC.readFile fileName
                        let resultList = frequency (unpack inputText)
                        let mapList = map (uncurry MyNode ) resultList
                        let tree = buildHaffmanTree mapList
                        let flatten = flattenThree tree
                        let reverseFlatten = reversePair flatten
                        let a = map ( pack . expandMayBe . (`getByKey` flatten )) (unpack inputText)
                        let result = intercalate (pack "") a;

                        let b = decodeArray (unpack result) reverseFlatten
                        print result
                        print b



                        P.putStrLn "End of programm"
                else do P.putStrLn "The file doesn't exist!"