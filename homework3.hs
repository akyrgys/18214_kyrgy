myfoldrmap :: (a -> b) -> [a] -> [b]
myfoldrmap f xs = foldr (\x xs -> f x:xs) [] xs

myfoldlmap :: (a -> b) -> [a] -> [b]
myfoldlmap f xs = foldl (\xs x -> xs ++ [f x]) [] xs

--foldr в данном случае более эффективен, т.к. в нем испольхуется операция (:) - эффективней, чем (++), использующаяся в foldl
