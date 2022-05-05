import qualified Data.Vector.Unboxed as UV

binFind :: Ord a => a -> [a] -> Maybe Int
binFind x xs
   | bs == length xs = Nothing
   | otherwise = Just bs
   where bs = binSearch ok ng (pred x xs)
         ok = length xs
         ng = -1
         pred x xs = \ mid ->  x <= (xs !! mid) 

lowerBoundL :: Ord a => a -> [a] -> Int
lowerBoundL key list = binSearch ok ng (pred key list)
  where ok = length list
        ng = -1
        pred key list' mid = key <= (list' !! mid) 

lowerBoundUV ::  Int -> UV.Vector Int -> Int
lowerBoundUV key vectorUV = binSearch ok ng (pred key vectorUV)
  where ok = UV.length vectorUV
        ng = -1
        pred key vec mid = key <= (vec UV.! mid)

binSearch ::  Int -> Int -> (Int -> Bool) -> Int
binSearch ok ng pred
    | abs (ok - ng) == 1 = ok
    | pred mid = binSearch mid ng pred
    | otherwise = binSearch ok mid pred
    where mid = (ok + ng) `div` 2


-------------------------------------------------------------------------------
-- Tests below
-------------------------------------------------------------------------------
test_binSearch :: Bool
test_binSearch = expected == binSearch ok ng pred
  where expected = 31
        ok = 100
        ng = 0
        pred mid = mid > 30

test_lowerBoundL :: Bool
test_lowerBoundL = expected == lowerBoundL key list
  where expected = 2
        key = 5
        list = [1,3,5,7,9]

test_lowerBoundL' :: Bool
test_lowerBoundL' = expected == lowerBoundL key list
  where expected = 2
        key = 4
        list = [1,3,5,7,9]

test_lowerBoundL'' :: Bool
test_lowerBoundL'' = expected == lowerBoundL key list
  where expected = 3
        key = 6
        list = [1,3,5,7,9]

test_lowerBoundUV :: Bool
test_lowerBoundUV = expected == lowerBoundUV key vec
  where expected = 2
        key = 5
        vec = UV.fromList [1,3,5,7,9]

test_lowerBoundUV' :: Bool
test_lowerBoundUV' = expected == lowerBoundUV key vec
  where expected = 2
        key = 4
        vec = UV.fromList [1,3,5,7,9]

test_lowerBoundUV'' :: Bool
test_lowerBoundUV'' = expected == lowerBoundUV key vec
  where expected = 3
        key = 6
        vec = UV.fromList [1,3,5,7,9]