import Data.Char


hashFunc :: (Show k, Eq k) => k -> Int -> Int
hashFunc key size = (abs $ foldl (\res ch -> res + ord ch) 0 (show key)) `mod` size


data HashTable k v = HashTable {
    table :: [[(k, v)]],
    cap   :: Int,
    cnt   :: Int
} deriving (Show)


defaultHashTable :: (Show k, Eq k) => Int -> HashTable k v
defaultHashTable size = HashTable {
                            table = replicate size [],
                            cap   = size,
                            cnt   = 0
                        }


from_list :: (Show k, Eq k) => [(k, v)] -> HashTable k v
from_list list   =	let 
						get_size :: Int -> [Int] -> Int
						get_size len (lol:lols) | len > lol = get_size len lols
												| otherwise = lol
						hehi :: [Int]
						hehi = rjomba [1]
							where
								rjomba (kek:keks) = kek:(rjomba[2*kek])
					in foldl insert (defaultHashTable (get_size (length list) hehi)) list
    
        


rehash :: (Show k, Eq k) => HashTable k v -> HashTable k v
rehash hash_table = foldl (\heh_table tuple -> foldl insert heh_table tuple) (defaultHashTable (2 * cap hash_table)) (table hash_table)


insert :: (Show k, Eq k) => HashTable k v -> (k, v) -> HashTable k v
insert hash_table couple | 2 * cnt hash_table == cap hash_table = funkcblya $ rehash hash_table
                       | otherwise                            = funkcblya          hash_table
							where
								funkcblya heh_table = HashTable {
													  table = left ++ (couple:middle) : right,
													  cap   = cap heh_table,
													  cnt   = cnt heh_table + 1
													}
									where
										left   = take (hashFunc (fst couple) (cap heh_table)) (table heh_table)
										middle = (table heh_table)!!(hashFunc (fst couple) (cap heh_table))
										right  = drop ((hashFunc (fst couple) (cap heh_table)) + 1) (table heh_table)


clear :: (Show k, Eq k) => HashTable k v -> HashTable k v
clear hash_table = defaultHashTable (cap hash_table)



erase :: (Show k, Eq k) => HashTable k v -> k -> HashTable k v
erase hash_table key =	let
							left   = take (hashFunc key (cap hash_table)) (table hash_table)
							middle = (table hash_table)!!(hashFunc key (cap hash_table))
							right  = drop ((hashFunc key (cap hash_table)) + 1) (table hash_table)
							delete_from list = filter (\x -> fst x /= key) list
						in	HashTable {
							table  = left ++ delete_from middle : right,
							cap    = cap hash_table,
							cnt    = cnt hash_table - 1
							}
    



at :: (Show k, Eq k) => HashTable k v -> k -> v
at hash_table key =	let
						list = (table hash_table)!!(hashFunc key (cap hash_table))
						check _   []                          = error "No such kek!"
						check 0   _                           = error "No such kek!"
						check len (heh:hehs) | fst heh == key = snd heh
											 | otherwise      = check (len - 1) hehs

					in check (length list) list
   

-------------------------------------------------------------------------------------------

contains :: (Show k, Eq k) => HashTable k v -> k -> Bool
contains hash_table key  =	let
								list = (table hash_table)!!(hashFunc key (cap hash_table))
								check _   []                                = False
								check len (rjaka:rjakas) | fst rjaka == key = True
														 | len       == 0   = False
														 | otherwise        = check (len - 1) rjakas
							in check (length list) list
   

-------------------------------------------------------------------------------------------

size :: (Show k, Eq k) => HashTable k v -> Int
size hash_table = cnt hash_table

-------------------------------------------------------------------------------------------

empty :: (Show k, Eq k) => HashTable k v -> Bool
empty hash_table = cnt hash_table == 0
