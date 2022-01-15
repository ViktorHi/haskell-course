import Data.Map (fromListWith, toList)
import Data.ByteString (intercalate)
import System.Directory (doesFileExist)
import Data.ByteString.Char8 (unpack, pack)

import qualified Data.ByteString.Char8 as BSC
import qualified System.Directory.Internal.Prelude as P

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

showPair :: Char  ->  Int  -> String
showPair a b = show a ++ " : " ++ show b

histogramm  = do 
            putStr "Enter file name: " 
            fileName <- getLine 
            fileExists <- doesFileExist fileName  
            if fileExists  
                then do 
                        inputText <- BSC.readFile fileName 
                        let resultList = frequency (unpack inputText)

                        let output = intercalate (pack "\n") (map (pack . uncurry showPair ) resultList);

                        BSC.putStrLn output
                        BSC.writeFile "out" output
                        P.putStrLn "End of programm"
                else do P.putStrLn "The file doesn't exist!" 