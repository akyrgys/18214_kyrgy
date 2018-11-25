heaPsort :: Ord a => [a] -> [a]
heaPsort xs = heaper xs (length xs - 1) 0
	where
		heaper ys len n | n <= len = heaper ((take n ys) ++ createHeap (drop n ys)) len (n + 1)
                        | otherwise = ys
			where 
				createHeap ys = helperCreater ys (length ys - 1) 0
					where
						helperCreater ys len n | n <= len = helperCreater (heaperScope ys n) len (n + 1) 
						                       | otherwise = ys
							where
							  heaperScope ys 0 = ys
							  heaperScope ys ind | ys !! ind < ys !! (div (ind - 1) 2) = heaperScope (swap ys ind (div (ind - 1) 2)) (div (ind - 1) 2)
											 | otherwise = ys
								where 
									 swap ys a b = (take (min a b) ys) ++ [ys !! (max a b)] ++ (take ((max a b) - (min a b) - 1) (drop ((min a b) + 1) ys)) ++ [ys !! (min a b)] ++ (drop ((max a b) + 1) ys)

									 
