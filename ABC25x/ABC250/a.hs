main = do
  [h,w] <- map read . words <$> getLine
  [r,c] <- map read . words <$> getLine
  print $ solve h w r c

solve h w r c = 4 + f 1 r + f h r + f 1 c + f w c

f a b = if a == b then -1 else 0