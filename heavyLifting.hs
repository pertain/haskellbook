-- Heavy Lifting exercises (ch 16)

-- Add fmap, parentheses, and function composition to
-- the expression as needed for the expression to
-- typecheck and produce the expected result

-- 1) expected result: a = [2]
-- a = (+1) $ read "[1]" :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]

-- 2) expected result: b = Just ["Hi,lol", "Hellolol"]
--b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3) expected result: c 1 = -2
--c = (*2) (\x -> x - 2)
c = fmap (*2) (\x -> x - 2)

-- 4) expected result: d 0 = "1[0,1,2,3]"
--d = ((return '1' ++) . show) (\x -> [x, 1..3])
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5) expected result: e = 3693 -- UNFINISHED
e :: IO Integer
e = let ioi     = readIO "1" :: IO Integer
        changed = read ("123" ++) (fmap show ioi)
    in fmap (*3) changed
