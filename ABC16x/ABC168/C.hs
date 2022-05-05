import Text.Printf
main = do
    [a,b,h,m] <- map read . words <$> getLine
    printf "%.10f\n" $ solve a b h m

solve ::  Double -> Double -> Double -> Double -> Double
solve a b h m = 
    let attitude = abs ( 11.0 * m - 60.0 * h) * pi / 360.0 
    in sqrt (a ^ 2 + b ^ 2 - 2.0 * a * b * cos attitude)