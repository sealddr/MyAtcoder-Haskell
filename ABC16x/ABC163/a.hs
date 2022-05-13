import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Text.Printf 

readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine

main = do
    r <- getInt
    Text.Printf.printf "%.10f\n" (2 * pi * (fromIntegral::Integer->Double) r)