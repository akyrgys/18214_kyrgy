import System.IO
import Data.List

main = do
		 handleI <- openFile "inputin.txt" ReadMode
		 handleO <- openFile "outputin.txt" WriteMode
		 text <- hGetContents handleI
		 let rjomba xs | xs == []  = []
					   | otherwise =  map (\ys -> ([head ys] ++ " - " ++ show(length ys) ++ " times")) $ group $ sort xs
		 
		 mapM_ (hPutStrLn handleO) $ rjomba $ text
		 hClose handleI
		 hClose handleO
