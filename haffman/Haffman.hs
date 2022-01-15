
import Data.Map (fromListWith, toList)
import Data.ByteString (intercalate)
import Data.Bifunctor (bimap)
import System.Directory (doesFileExist)
import Data.ByteString.Char8 (unpack, pack, head)

import qualified Data.ByteString.Char8 as BSC
import qualified System.Directory.Internal.Prelude as P
import Data.List (sortBy, find, isPrefixOf)
import qualified Data.List.Split(wordsBy)

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
encodeArray:: [Char] -> [(Char, String)] -> [String]
encodeArray inputText charToString = map ( expandMayBe . (`getByKey` charToString ))  inputText

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

-- Read/Write 
writeMapToCode:: MapToCode -> BSC.ByteString
writeMapToCode toChar = intercalate (pack "\n") ( map (\(a,b)-> pack ( [a] ++ " " ++  b) ) toChar );

writeMapToChar:: MapToChar -> BSC.ByteString
writeMapToChar toChar = intercalate (pack "\n") ( map (\(a,b)-> pack ( a ++ " " ++  [b]) ) toChar );

readFileToArray:: BSC.ByteString -> [(BSC.ByteString,BSC.ByteString)]
readFileToArray inputString = readFileToArrayHelper (BSC.words inputString)

readFileToArrayHelper:: [BSC.ByteString]-> [(BSC.ByteString,BSC.ByteString)]
readFileToArrayHelper [] = []
readFileToArrayHelper (a:b:cs) = (a, b) : readFileToArrayHelper cs
readFileToArrayHelper [ a ] = error "May be delete special symbols from source file"

parseToMapToCode:: [(BSC.ByteString,BSC.ByteString)] -> MapToCode
parseToMapToCode = map (Data.Bifunctor.bimap BSC.head BSC.unpack )

parseToMapToChar:: [(BSC.ByteString,BSC.ByteString)] -> MapToChar
parseToMapToChar = map (Data.Bifunctor.bimap BSC.unpack BSC.head )

removeFromInputString :: BSC.ByteString -> BSC.ByteString
removeFromInputString line = BSC.pack (removePunc (BSC.unpack line))

removePunc :: [Char] -> [Char]
removePunc xs = [ x | x <- xs, x `notElem` " \n" ]

-- main 
haffman :: IO ()
haffman  = do
            putStr "Enter file name: "
            fileName <- getLine
            fileExists <- doesFileExist fileName
            if fileExists
                then do
                        inputText <- BSC.readFile fileName
                        let frequencyList = frequency (unpack (removeFromInputString inputText))
                        let tree = buildHaffmanTree (map (uncurry MyNode ) frequencyList)

                        let charToString = flattenThree tree
                        let stringToChar = reversePair charToString

                        let encodedText = encodeArray (unpack (removeFromInputString inputText)) charToString
                        let encodedTextToOutput = intercalate (pack "") ( map pack encodedText);

                        BSC.writeFile "toChar.out" (writeMapToChar stringToChar)
                        BSC.writeFile "toCode.out" (writeMapToCode charToString)
                        BSC.writeFile "encoded.out" encodedTextToOutput

                        readedToCharString <- BSC.readFile "toChar.out"
                        encodedByteString <- BSC.readFile "encoded.out"

                        let readedToChar =  parseToMapToChar (readFileToArray readedToCharString)
                        let decodedText = decodeArray (unpack encodedByteString) readedToChar
                        let decodedTextToOutput = pack decodedText

                        BSC.writeFile "decoded.out" decodedTextToOutput
                        P.putStrLn "End of programm"
                else do P.putStrLn "The file doesn't exist!"