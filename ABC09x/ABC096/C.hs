import qualified Data.ByteString.Char8 as BS
import Control.Monad (replicateM)
import qualified Data.Vector as V

main = do
    [h,w] <- map (read::String->Int) . words <$> getLine
    cm <- V.replicateM h $ BS.filter (/= '\r') <$> BS.getLine
    let emptyRow = BS.replicate (w + 2) '.'
        xtdRow r = BS.cons '.' $ BS.snoc r '.'
        cm' = V.cons emptyRow $ V.snoc (V.map xtdRow cm) emptyRow
    putStrLn $ solve h w cm'

solve :: Int -> Int -> V.Vector BS.ByteString -> String
solve h w cm
    | all (checkLine w cm) [1..h] = "Yes"
    | otherwise = "No"
    where checkLine :: Int -> V.Vector BS.ByteString -> Int -> Bool
          checkLine w cm j = all (\ i -> notToBePainted i j cm || paintable i j cm) [1..w]
          notToBePainted :: Int -> Int -> V.Vector BS.ByteString -> Bool
          notToBePainted i j cm = BS.index ((V.!) cm j) i == '.'
          paintable :: Int -> Int -> V.Vector BS.ByteString -> Bool
          paintable i j cm
            | BS.index (cm V.! (j - 1)) i       == '#' = True
            | BS.index (cm V.! j      ) (i - 1) == '#' = True
            | BS.index (cm V.! j      ) (i + 1) == '#' = True
            | BS.index (cm V.! (j + 1)) i       == '#' = True
            | otherwise = False