module Tree where



data Tree a = Node a [Tree a]
	deriving Show

--maptree f (Node a xs) = Node (f a) (map (maptree f) xs)
--
data RBColor = R | B
	deriving Show

data RB a= RB RBColor (RB a) a (RB a) | E
	deriving Show

--data VTree a = Node a [VTree a]
--      deriving Show
        
mapVTree f (Node a branches) = Node (f a) (map (mapVTree f) branches)
                
--maptree
nodes (Node a branches) = (a:concat (map nodes branches))


