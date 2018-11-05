import Data.Char
toDecimal :: Integer -> String -> String
toDecimal _ []         =  error "Empty list"
toDecimal 1 xs         =  show (length(xs) - 1)
toDecimal base snumber =  show (helper (base) (snumber) (toInteger (length snumber - 1)))
                          where helper :: Integer -> String -> Integer -> Integer
                                helper base (x:snumber) n | ((base < 1) || (base > 62)) = error "Not correct base"
                                                          |     ((digit x) >= base)     = error "Not correct numb"
                                                          |           n == 0            = (digit x)
                                                          | otherwise = (digit x) * (base^n) + (helper (base) (snumber) (n-1))
                                                          where digit :: Char -> Integer
                                                                digit k = numb (k) (['0'..'9']++['a'..'z']++['A'..'Z']) (0)
                                                                    where numb :: Char -> String -> Integer -> Integer
                                                                          numb k (y:xs) n  | n > 62    = 63
                                                                                           | k==y      = n
																						   | xs == []  = error "Not correct numb"
                                                                                           | otherwise = numb k xs (n+1)

fromDecimal :: Integer -> String -> String
fromDecimal _ []           = error "Empty list"
fromDecimal 1 xs           =  replicate (read xs + 1) '1'
fromDecimal toBase snumber =  helper1 (toBase) (toDecimal1 snumber (toInteger (length snumber - 1)))
                                where helper1 :: Integer -> Integer -> String
                                      helper1 toBase num | ((toBase < 1) || (toBase > 62)) = error "Not correct base"
                                                         |           num < toBase          = symbol num
                                                         |           otherwise             = ((helper1 (toBase) (num `div` toBase) ++ symbol (num `mod` toBase)))
                                                        where symbol :: Integer -> String
                                                              symbol k = symbnum (k) (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']) (0)
                                                                  where symbnum :: Integer -> String -> Integer -> String
                                                                        symbnum k (y:xs) n  | n > 62    = error "Not correct numb"
                                                                                            | k == n    = y:[]
																						    | xs == []  = error "Not correct numb"
                                                                                            | otherwise = symbnum k xs (n+1)
                                      toDecimal1 :: String -> Integer -> Integer
                                      toDecimal1 (x:snumber) n | n == 0 = (digit x)
                                                               | otherwise = (digit x) * (10^n) + (toDecimal1 (snumber) (n-1))
                                                               where digit :: Char -> Integer
                                                                     digit k = numb (k) (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']) (0)
                                                                         where numb :: Char -> String -> Integer -> Integer
                                                                               numb k (y:xs) n  | (n >= 10 || xs == [])  = error "Not correct numb"
                                                                                                | k == y                 = n
                                                                                                | otherwise              = numb k xs (n+1)
convertFromTo :: Integer -> Integer -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)