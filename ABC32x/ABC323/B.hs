import Data.List
import Data.Ord

main :: IO ()
main = do
    n <- readLn :: IO Int
    results <- mapM (const getLine) [1..n]
    let teams = [(i, winRate result) | (i, result) <- zip [1..n] results]
        sortedTeams = sortOn (Down . snd) teams
        teamNumbers = map fst sortedTeams
    putStrLn $ unwords $ map show teamNumbers

winRate :: String -> Double
winRate result = fromIntegral (count 'o' result) / fromIntegral (length result)
count c = length . filter (== c)
