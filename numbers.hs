import Data.Char
sc = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
toDecimal :: Integer -> String -> String
toDecimal _ [] = error "Empty list"
toDecimal base snumber = show (helper (base) (snumber) (toInteger (length snumber - 1)))
                        where helper :: Integer -> String -> Integer -> Integer
                              helper base (x:snumber) n | (base == 1) = if (n==0) then 0 else (1 + (helper (base) (snumber) (n-1)))
                                                        | ((base < 1) || (base > 61)) = error "Not correct base"
                                                        | ((digit x) >= base) = error "Not correct numb"
                                                        | n == 0 = (digit x)
                                                        | otherwise = (digit x) * (base^n) + (helper (base) (snumber) (n-1))
                                                        where digit :: Char -> Integer
                                                              digit k = numb (k) (sc) (0)
                                                                  where numb :: Char -> String -> Integer -> Integer
                                                                        numb k (y:xs) n  | n > 61 = 62
                                                                                         | k==y = n
																						 | xs == [] = error "Not correct numb"
                                                                                         | otherwise = numb k xs (n+1)

fromDecimal :: Integer -> String -> String
fromDecimal _ [] = error "Empty list"
fromDecimal toBase snumber = reverse (helper1 (toBase) (toDecimal1 snumber (toInteger (length snumber - 1))))
                                where helper1 :: Integer -> Integer -> String
                                      helper1 toBase num | (toBase == 1 && num >=0) = if (num==0) then "1" else ("1" ++ (helper1 (toBase) (num-1)))
                                                         | ((toBase < 1) || (toBase > 61)) = error "Not correct base"
                                                         | num < toBase = symbol num
                                                         | otherwise = (symbol (num `mod` toBase)) ++ (helper1 (toBase) (num `div` toBase))
                                                        where symbol :: Integer -> String
                                                              symbol k = symbnum (k) (sc) (0)
                                                                  where symbnum :: Integer -> String -> Integer -> String
                                                                        symbnum k (y:xs) n  | n > 61 = error "Not correct numb"
                                                                                            | k==n = y:[]
																						    | xs == [] = error "Not correct numb"
                                                                                            | otherwise = symbnum k xs (n+1)
                                      toDecimal1 :: String -> Integer -> Integer
                                      toDecimal1 (x:snumber) n | n == 0 = (digit x)
                                                               | otherwise = (digit x) * (10^n) + (toDecimal1 (snumber) (n-1))
                                                               where digit :: Char -> Integer
                                                                     digit k = numb (k) (sc) (0)
                                                                         where numb :: Char -> String -> Integer -> Integer
                                                                               numb k (y:xs) n  | (n >= 10 || xs == [])  = error "Not correct numb"
                                                                                                | k==y = n
                                                                                                | otherwise = numb k xs (n+1)
convertFromTo :: Integer -> Integer -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)