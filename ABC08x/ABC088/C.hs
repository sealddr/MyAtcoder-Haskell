import Control.Monad
main = do
    cs <- replicateM 3 $ map read .words <$> getLine 
    putStrLn . solve $ concat cs

solve cs@[c11, c12, c13, c21, c22, c23, c31, c32, c33]
    | all (\ chk -> chk cs) chks = "Yes"
    | otherwise = "No"
    where chks = [chk22, chk23, chk32, chk33]
          chk22 cs = c21 - c11 + c12 == c22
          chk23 cs = c21 - c11 + c13 == c23
          chk32 cs = c31 - c11 + c12 == c32
          chk33 cs = c31 - c11 + c13 == c33
