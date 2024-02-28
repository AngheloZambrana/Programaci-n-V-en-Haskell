module  Homework where
    -- 2^3*4
expression1 = (2 ^ 3) * 4

-- 2*3+4*5
expression2 = (2 * 3) + (4 * 5)

-- 2+3*4^5
expression3 = 2 + (3 * (4 ^ 5))


n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

    --No se porque da error en este lastt
    solucion2 :: [a] -> a
    solucion2 xs = head (reverse xs)

    init :: [a] -> [a]
    init xs = reverse (tail (reverse xs))





