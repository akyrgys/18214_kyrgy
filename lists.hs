--Task1
myget :: [a] -> Int -> a
myget [] _   =   error "empty list"
myget (x:xs) n | (n < 1)    =  error "Not correct"
               | (n == 1)   =  x
               | otherwise  =  myget xs (n - 1)


--Task2
myhead :: [a] -> a
myhead []     =  error "empty list"
myhead (x:_)  =  x


--Task3
mylast :: [a] -> a
mylast []     =  error "empty list"
mylast [x]    =  x
mylast (_:xs) =  mylast xs


--Task4
mytail :: [a] -> [a]
mytail []      =  error "empty list"
mytail (_:xs)  =  xs


--Task5
myinit :: [a] -> [a]
myinit []     =  error "empty list"
myinit [x]    =  []
myinit (x:xs) =  x:(myinit xs)


--Task7
mylength :: Eq b => [b] -> Int
mylength []  =  0
mylength xs  =  mylength' xs 0
              where mylength' (_:xs) n | (xs == [])  =  n + 1
                                       | otherwise   =  mylength' xs (n + 1)


--Task6
myreverse :: Eq a => [a] -> [a]
myreverse []   =   []
myreverse xs   =   [myget (xs) (n) | n <- helper xs]
             where helper xs  =  [(mylength xs), (mylength xs - 1) .. 1]


--Task8
myappend :: [a] -> a -> [a]
myappend [] x      =  x:[]
myappend (y:xs) x  =  y:(myappend xs x)


--Task 9
myconcat :: [a] -> [a] -> [a]
myconcat [] []      =    []
myconcat [] sys     =    sys
myconcat xs []      =    xs
myconcat xs (x:sys) =    myconcat (myappend xs x) sys


--Task 10
mydrop :: Int -> [a] -> [a]
mydrop 0 xs   =   xs
mydrop _ []   =   []
mydrop n (_:xs) | (n > 0)    =   mydrop (n-1) xs 
                | otherwise  =   error "Not correct"


--Task 11
mytake :: Eq a => Int -> [a] -> [a]
mytake n xs  =  [ myget (xs) (k) | k  <-  myhelper xs]
		where myhelper xs = myfilter ( <= n) [ 1, 2 .. (mylength xs) ]

		
--Task 12
mysplitAt :: Eq a => Int -> [a] -> ([a], [a])
--mysplitAt _ []  =  error "There is an empty list"
mysplitAt n xs  | (n <= 0)           =  ([], xs)
                |  otherwise         =  (mytake (n-1) (xs), mydrop (n-1) (xs))


--Task 13
mynull :: [a] -> Bool
mynull []     =  False
mynull (_:_)  =  True


--Task 14
myelem :: Eq a => [a] -> a -> Bool
myelem [] _    =   False
myelem (k:xs) x  |  (k == x )  =  True
                 |  otherwise  =  myelem xs x


--Task 15
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ []  =  []
myfilter test (x:xs) | (test x == True)  =  x:(myfilter test xs)
                     | otherwise         =  myfilter test xs


--Task 16
mymap :: (a -> b) -> [a] -> [b]
mymap f []      =  []
mymap f (x:xs)  =  (f x):(mymap f xs)   


--Task 17
myzip :: [a] -> [b] -> [(a, b)]
myzip  []      _   =  []
myzip  _      []   =  [] 
myzip (x:xs)(y:ys) =  (x,y) : myzip xs ys





