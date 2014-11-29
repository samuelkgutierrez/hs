-- A few simple tests for function implementation given some of the contraints
-- of the STLC.

iszero :: Int -> Bool
iszero n = n == 0

succ' :: Int -> Int
succ' n = n + 1

pred' :: Int -> Int
pred' n = if n == 0 then n else n - 1

leq :: Int -> Int -> Bool
leq a b = if iszero a
          then True
          else if iszero b
               then False
               else leq (pred' a) (pred' b)

equal :: Int -> Int -> Bool
equal a b = if iszero a
            then if iszero b then True else False
            else equal (pred' a) (pred' b)

main :: IO ()
main = do
    let testList = [(True,  leq 2 3),
                    (True,  leq 2 2),
                    (False, leq 3 2),
                    (True,  equal 55 55),
                    (False, equal 3 2),
                    (False, equal 2 3)]
    putStrLn "(Expected, Actual)"
    mapM_ (putStrLn . show) testList
