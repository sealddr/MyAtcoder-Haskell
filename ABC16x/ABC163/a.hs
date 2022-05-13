import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Text.Printf

readInt = fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

main = getInt >>= Text.Printf.printf "%.10f\n" . (*) (pi * 2) . (fromIntegral :: Integer -> Double)