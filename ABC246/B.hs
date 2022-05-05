import Text.Printf (printf)
main = do
    v <- map read . words <$> getLine
    printf "%.10f %.10f\n" (x v) (y v)

x :: [Int] -> Double
x v@[x0, _] = fromIntegral x0 / norm v

y :: [Int] -> Double
y v@[_, y0] = fromIntegral y0 / norm v

norm :: [Int] -> Double
norm [x, y] = sqrt (fromIntegral x ^ 2 + fromIntegral y ^ 2)