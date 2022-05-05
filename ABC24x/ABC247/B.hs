main = do
    c <- getContents
    let names =  [ (head x, last x) |x <- [words x| x <- tail $ lines c]]
    if uniqueNicknames names then putStrLn "Yes" else putStrLn "No"

uniqueNicknames :: [(String, String)] -> Bool
uniqueNicknames x = uniqueNicknames' x x

uniqueNicknames' :: [(String, String)] -> [(String, String)] -> Bool
uniqueNicknames' _  [] = error "No members"
uniqueNicknames' []  _ = True
uniqueNicknames' ((s1, t1):rests) ((_,_):others) = uniqueNickname s1 t1 others && uniqueNicknames' rests (others ++ [(s1,t1)])

uniqueNickname :: String -> String -> [(String, String)] -> Bool 
uniqueNickname s t xs = check s xs || check t xs

check :: String -> [(String, String)] -> Bool
check a [] = True
check a ((s, t):xs) = a /= s && a /= t && check a xs