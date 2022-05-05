import qualified Data.ByteString.Char8 as BS

main = do
    _ <- getLine
    a_s <- map read . words <$> getLine
    BS.putStr $ solve a_s

solve a_s = BS.unlines (map ((BS.pack. show) . ( \ (_, _ ,d) -> cost a_s - d )) (diffs a_s))
    where cost a_s = snd $ foldl f (0, 0) (a_s ++ [0])
          f (now, acc) next = (next, acc + abs (now - next))
          diffs a_s = drop 1 $ scanl fs (0, head a_s, 0) (tail a_s ++ [0])
          fs (prev, now, diff) next = (now, next, abs(now - prev) + abs(next - now) - abs(next - prev))
