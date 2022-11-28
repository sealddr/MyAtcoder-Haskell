main = getLine >>= print . sum . map f
f 'w' = 2
f 'v' = 1
f _ = 0