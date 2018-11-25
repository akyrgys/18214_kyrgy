import Data.List
--Task 1
myget :: [a] -> Int -> a
myget [] _   =   error "empty list"
myget (x:xs) n | (n < 1)    =  error "Not correct"
               | (n == 1)   =  x
               | otherwise  =  myget xs (n - 1)

--Task 2
myinit :: [a] -> [a]
myinit []     =  error "empty list"
myinit [x]    =  []
myinit (x:xs) =  x:(myinit xs)

--Task 3
myreverse :: [a] -> [a]
myreverse []   =   []
myreverse xs   =   [myget (xs) (n) | n <- helper xs]
           where helper xs  =  [(mylength xs), (mylength xs - 1) .. 1]
                            where mylength xs = helper1 xs 0
                                          where helper1 [] n      = n
                                                helper1 (x:xs) n  = helper1 xs (n+1)
--Task 4
myconcat :: [a] -> [a] -> [a]
myconcat [] ys     =  ys
myconcat (x:xs) ys =  x: (myconcat xs ys)

--Task 5
mycycle :: [a] -> [a]
mycycle [] = []
mycycle xs = myconcat xs (mycycle xs)
--Task 6
mytake :: Int -> [a] -> [a]
mytake n xs  =  [ myget (xs) (k) | k  <-  myfilter ( <= n) [ 1, 2 .. (mylength xs) ]]
                      where myfilter :: (a -> Bool) -> [a] -> [a]
                            myfilter _ []  =  []
                            myfilter test (x:xs) | (test x == True)  =  x:(myfilter test xs)
                                                 | otherwise         =  myfilter test xs
                            mylength xs = helper1 xs 0
                                          where helper1 [] n      = n
                                                helper1 (x:xs) n  = helper1 xs (n+1)
--Task 7
myinits :: [a] -> [[a]]
myinits [] = []
myinits (x:xs) = myinit(x:xs):(myinits xs)

mytails :: [a] -> [[a]]
mytails [] = []
mytails (x:xs) = mytail(x:xs):(mytails xs)
			where mytail (_:xs) = xs
--Task 8
myelem :: Eq a => [a] -> a -> Bool
myelem [] _    =   False
myelem (k:xs) x  |  (k == x )  =  True
                 |  otherwise  =  myelem xs x

--Task 9
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:xs) = helper xs [x]
           where helper :: Eq a => [a] -> [a] -> [a]
                 helper [] ys = ys
                 helper (x:xs) (ys) | (myelem ys x) == False =  helper (xs) (x:ys)
                                    | otherwise = helper (xs) (ys)

--Task 10
mypermutations :: [a] -> [[a]]
mypermutations [] = [[]]
mypermutations xs = [ y:zs | (y:ys) <- myrotater xs, zs <- mypermutations ys ] --myrotarer - Task 18

--Task 11


--Task 12
cubsumr :: [Integer] -> Integer
cubsumr xs = foldr (+) 0 (foldr (\x ys -> (^3) x:ys) [] xs)

--Task 13
cubsuml :: [Integer] -> Integer
cubsuml xs = foldl (+) 0 (foldl (\ys x -> ys ++ [(^3) x]) [] xs)

--Task 14
fact :: Int -> Int
fact 0 = 1
fact n = foldr (*) 1 [1..n]

expT :: Double -> Int -> Double
expT pow n = foldr (+) 1 (foldr (\x ys -> (f) x:ys) [] [1..n])
							where f x = (pow^x)/(fromIntegral(fact x))
--Task 15
howmany :: (Eq a) => a -> [a] -> Int
howmany _ [] = 0
howmany a xs = mylength (foldr(\x ys -> if (x == a) then (x:ys) else ys) [] xs)
			where mylength xs = helper1 xs 0
                                          where helper1 [] n      = n
                                                helper1 (x:xs) n  = helper1 xs (n+1)
--Task 16
howmany_g_b_letters :: [Char] -> (Int, Int)
g = "aeiou"
b = "tnrsh"
howmany_g_b_letters xs = ((mylength (foldr(\x xs -> if (myelem g x) then (x:xs) else xs) [] xs)), (mylength (foldr(\x xs -> if (myelem b x) then (x:xs) else xs) [] xs)))
			where mylength xs = helper1 xs 0
                                          where helper1 [] n      = n
                                                helper1 (x:xs) n  = helper1 xs (n+1)
--Task 17
myintersperse :: a -> [a] -> [a]
myintersperse _ []     = []
myintersperse k (x:xs) = x : foldr(\x xs -> k:(x:xs)) [] xs

--Task 18
cycleshift (x:xs) = xs ++ [x]
myrotater :: [a] -> [[a]]
myrotater [] = []
myrotater xs = mytail (foldr (\ys (x:xs) -> (cycleshift x):x:xs) [xs] xs)
			where mytail (_:xs)  =  xs
			
myrotatel :: [a] -> [[a]]
myrotatel [] = []
myrotatel xs =  mytail(foldl (\(x:xs) ys -> (cycleshift x):x:xs) [xs] xs)
			where mytail (_:xs)  =  xs
