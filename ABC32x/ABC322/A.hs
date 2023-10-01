import Data.List

main :: IO ()
main = do
  _ <- getLine
  s <- getLine
  case elemIndex True . map ("ABC" `isPrefixOf`) $ tails s of
    Just i -> print $ i + 1
    Nothing -> print (-1)
