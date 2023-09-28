import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine

main = do
    k <- getInt
    print . flip (!!) (fromIntegral k) . sort . map (read :: [Char] -> Int) . tail $ subsequences ['9','8'..'0']