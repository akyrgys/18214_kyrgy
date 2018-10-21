import Data.Char
sc = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
toDecimal :: Int -> String -> String
toDecimal _ [] = error "Empty list"
toDecimal base snumber = show (helper (base) (snumber) (length snumber - 1))
                        where helper :: Int -> String -> Int -> Int
                              helper base (x:snumber) n | (base == 1) = if (n==0) then 0 else (1 + (helper (base) (snumber) (n-1)))
                                                        | ((base < 1) || (base > 61)) = error "Not correct base"
                                                        | ((digit x) >= base) = error "Not correct numb"
                                                        | n == 0 = (digit x)
                                                        | otherwise = (digit x) * (base^n) + (helper (base) (snumber) (n-1))
                                                        where digit :: Char -> Int
                                                              digit k = numb (k) (sc) (0)
                                                                  where numb :: Char -> String -> Int -> Int
                                                                        numb k (y:xs) n  | n > 61 = 62
                                                                                         | k==y = n
																						 | xs == [] = error "Not correct numb"
                                                                                         | otherwise = numb k xs (n+1)

fromDecimal :: Int -> String -> String
fromDecimal _ [] = error "Empty list"
fromDecimal toBase snumber = reverse (helper1 (toBase) (toDecimal1 snumber (length snumber - 1)))
                                where helper1 :: Int -> Int -> String
                                      helper1 toBase num | (toBase == 1 && num >=0) = if (num==0) then "1" else ("1" ++ (helper1 (toBase) (num-1)))
                                                         | ((toBase < 1) || (toBase > 61)) = error "Not correct base"
                                                         | num < toBase = symbol num
                                                         | otherwise = (symbol (num `mod` toBase)) ++ (helper1 (toBase) (num `div` toBase))
                                                         where symbol :: Int -> String
                                                               symbol k = [sc !! k] 
                                      toDecimal1 :: String -> Int -> Int
                                      toDecimal1 (x:snumber) n | n == 0 = (digit x)
                                                               | otherwise = (digit x) * (10^n) + (toDecimal1 (snumber) (n-1))
                                                               where digit :: Char -> Int
                                                                     digit k = numb (k) (sc) (0)
                                                                         where numb :: Char -> String -> Int -> Int
                                                                               numb k (y:xs) n  | (n >= 10 || xs == [])  = error "Not correct numb"
                                                                                                | k==y = n
                                                                                                | otherwise = numb k xs (n+1)
convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)