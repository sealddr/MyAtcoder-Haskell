import Data.List
main = do
    _ <- getLine
    s <- getLine
    t <- getLine
    print $ solve s t
            where solve s t
                    | s `isPrefixOf` t && s `isSuffixOf` t = 0
                    | s `isPrefixOf` t = 1
                    | s `isSuffixOf` t = 2
                    | otherwise = 3
