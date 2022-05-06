import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine

main = do
    k <- getInt
    print $ solve k

solve k =sum  [gcd a $ gcd b c | a<-[1..k], b<-[1..k], c<-[1..k]]
