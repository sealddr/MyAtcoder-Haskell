import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    n <- getInt
    a_s <- map fromIntegral <$> getIntList

    cnt <- MUV.new (fromIntegral n)
    MUV.set cnt (0::Int)
    forM_ a_s $ \x -> do
        MUV.modify cnt (+1) (x-1)  

    cnt' <- UV.freeze cnt
    let ans = UV.toList cnt'
    mapM_ print ans