{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Map (fromListWith, toList)
import Data.ByteString (intercalate)
import System.Directory (doesFileExist)
import Data.ByteString.Char8 (unpack, pack)

import qualified Data.ByteString.Char8 as BSC
import qualified System.Directory.Internal.Prelude as P
import Data.List (sortBy)


frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

showPair :: Char  ->  Int  -> String
showPair a b = show a ++ " : " ++ show b


data MyTree a = MyNode Char Int
              | MyFilledNode Int (MyTree a) (MyTree a)
              deriving (Eq,Ord,Show,Read)

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
 
haffman :: IO ()
haffman  = do
            putStr "Enter file name: "
            fileName <- getLine
            fileExists <- doesFileExist fileName
            if fileExists
                then do
                        inputText <- BSC.readFile fileName
                        let resultList = frequency (unpack inputText)
                        let mapList = map (uncurry MyNode ) [('a', 2), ('b', 5), ('c', 7), ('d', 3)]
                        let tree = buildHaffmanTree mapList
                        let flatten = flattenThree tree
                        print flatten



                        P.putStrLn "End of programm"
                else do P.putStrLn "The file doesn't exist!"