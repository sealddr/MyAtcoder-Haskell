import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Text.Printf

readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine

main = do
    l <- getInt
    Text.Printf.printf "%.10f" $ solve l

solve l = l' l * l' l* l' l
    where l' :: Integer -> Double
          l' l = fromIntegral l / 3.0