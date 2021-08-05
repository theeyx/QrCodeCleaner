-- Function that cleans up a QR code from a bmp file
-- ******INSTRUCTIONS******
-- run cabal update in local folder
-- then run cabal install --lib bmp
-- load this app on ghci
-- then run loadBitmapIntoIt "sampleQR.bmp"
-- finally run showAsASCIIArt (cleanQR(convertToList it)) 
-- all the monochromatic noise is cleaned up






import Codec.BMP
import GHC.Word
import GHC.List as List
import Data.ByteString




-- loadBitmapIntoIt is the function I have written to turn the bmp file into the form ([(Int, Int, Int)], Int, Int) to prepare for cleanup
loadBitmapIntoIt :: FilePath -> IO ([(Int, Int, Int)], Int, Int)
loadBitmapIntoIt filename = do
  (Right bmp) <- readBMP filename
  return ((parseIntoRGBVals (convertToIntList (unpack (unpackBMPToRGBA32 bmp)))), (fst (bmpDimensions bmp)), (snd (bmpDimensions bmp)))
-- helper function to turn the file into list of RGB Int values
convertToIntList :: [Word8] -> [Int]
convertToIntList [] = []
convertToIntList (h:t) = (fromIntegral (toInteger h)) : (convertToIntList t)
-- helper function to turn list of Int into a list of triple tuples
parseIntoRGBVals :: [Int] -> [(Int, Int, Int)]
parseIntoRGBVals [] = []
parseIntoRGBVals (h:i:j:_:t) = (h,i,j) : (parseIntoRGBVals t)


-- showAsASCIIArt is the function I have written to display the cleaned up QR code
showAsASCIIArt :: [[(Int, Int, Int)]] -> IO ()
showAsASCIIArt pixels = Prelude.putStr (unlines (showAsASCIIArt' pixels) )






-- convertToList is the function I have written to turn the result from "it" into the form [[(Int, Int, Int)]]
convertToList :: ([(Int, Int, Int)], Int, Int) -> [[(Int, Int, Int)]]
convertToList (_, _, 0) = []
convertToList (r,g,b) = convertToList (List.drop g r , g, b-1) ++ [List.take g r]


-- cleanQR is function I have written to turn the list into black (0,0,0) or white (255,255,255)
cleanQR :: [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
cleanQR [] = []
cleanQR (x: xs) = cleanSec x : cleanQR xs
-- helper function that changes every tuple in each list to white or black
cleanSec :: [(Int, Int, Int)] -> [(Int, Int, Int)]
cleanSec [] = []
cleanSec ((0, 0, 0) : s) = (0, 0, 0) : cleanSec s
cleanSec ((_) : s) = (255,255,255) : cleanSec s


-- showAsASCIIArt' is helper function to showAsASCIIArt to turn our tuples into a character for display
showAsASCIIArt' :: [[(Int, Int, Int)]] -> [[Char]]
showAsASCIIArt' [] = []
showAsASCIIArt' (x: xs) = switchToCharCol x : showAsASCIIArt' xs
-- helper function that changes every tuple in each list to a white or black character 
switchToCharCol :: [(Int, Int, Int)] -> [Char]
switchToCharCol [] = []
switchToCharCol ((0, 0, 0) : s) = '░' : switchToCharCol s 
switchToCharCol ((_) : s) = '▓' : switchToCharCol s