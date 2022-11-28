main = getLine >>= print . solve
solve str = length $ str ++ filter (=='w') str