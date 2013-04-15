module Thtools where

import Data.List

sign 0 = 0
sign n
	| n<0 = -1
	| True = 1

image _ [] = []
image [] _ = []
image (True:xs) (y:ys) = (y:image xs ys)
image (False:xs) (y:ys) = image xs ys

interleave [x] c = [x]
interleave (x:xs) c = (x:interleave' xs)
        where interleave' x= (c:interleave x c)

leave 0 xs = xs
leave n (x:xs) = leave (n-1) xs

rshift (x:xs) = xs++[x]
lshift xs = (last xs:init xs)

segment _ [] = []
segment n x = (take n x:segment n (leave n x))

applyfst f x (y,z) = (f x y,z)
applysnd f x (y,z) = (y, f x z)


reorder x o = map fst (sortWith snd (zip x o))

addpair (x1,x2) (y1,y2) = (x1+y1,x2+y2)
multpairconst c (x,y) = (x*c,y*c)

set_insert xs y
	| elem y xs = xs
	| True = (y:xs)

set_remove [] y = []
set_remove (x:xs) y
	| x == y = xs
	| True = (x:set_remove xs y)

append x= (++ [x])

apply x f = f x

allpaths g x = tail (allpaths' g x)
	where
		allpaths' g x = map (x:) ([[]]++concat (map (allpaths' g') (map snd verticies)))
			where
				(verticies,g') = partition ((x==).fst) g
	
kleene sigma 0 = [[]]
kleene sigma n = concat (map (\x->map (x:) rest) sigma)
	where
		rest = kleene sigma (n-1)

dot_product x y= sum(zipWith (*) x y)

maxl l = foldr1 max l
minl l = foldr1 min l
mml p l = foldr1 (mm p) l
mmlWith f p l = foldr1 (mmWith f p) l
mm p x y = p*(max(p*x)(p*y))
mmWith f p x y = (maxWith ((p*).f)x y)
maxWith f x y
	| f x > f y = x
	| True = y

qsort[] = []
qsort (l:ls) = qsort ll ++ [l] ++ qsort gl
	where
		(ll,gl) = partition (<l) ls



sortWith f [] = []
sortWith f (x:xs) = sortWith f lower ++ [x] ++ sortWith f upper
	where
		(lower,upper) = partition ((<f x).f) xs

sortHighWith f [] = []
sortHighWith f (x:xs) = sortHighWith f upper ++ [x] ++ sortHighWith f lower
	where
		(lower,upper) = partition ((<f x).f) xs


merge_sort xs = merge_sort' xs
	where
		merge :: Ord t => [t] -> [t] -> [t]
		merge [] l2 = l2
		merge l1 [] = l1
		merge l1@(a:ax) l2@(b:bx)
		  | a < b = a : merge ax l2
		  | otherwise = b : merge l1 bx
		
		
		merge_sort' :: Ord t => [t] -> [t]
		merge_sort' [a] = [a]
		merge_sort' ls@(a:ax) = merge (merge_sort' a) (merge_sort' b)
		  where
		    len_2 :: Int
		    len_2 = (length ls) `div` 2
		    split :: [t] -> ([t],[t])
		    split l = ((take len_2 l),(drop len_2 l))
		    (a,b) = split ls

mergeSortWith f xs = xs--merge_sort' xs
	where
		merge [] l2 = l2
		merge l1 [] = l1
		merge l1@(a:ax) l2@(b:bx)
		  | f a < f b = a : merge ax l2
		  | otherwise = b : merge l1 bx
		
		
		merge_sort' [] = []
		merge_sort' [a] = [a]
		merge_sort' ls = merge (merge_sort' a) (merge_sort' b)
		  where
		    (a,b) = splitAt (div (length ls) 2) ls

sortComp _ [] = []
sortComp f (x:xs) = sortComp f lower ++ [x] ++ sortComp f upper
	where
		(lower,upper) = partition (f x) xs

apply2 x y f = f x y

toposort[] = []
toposort g = (id0:toposort g')
	where
		id0=(fst.head.filter (null.snd))g
		g' = map(\(x,y)->(x,set_remove y id0))(filter((/=id0).fst) g)
test0 = [(1,[]),
	(2,[1,3,4]),
	(3,[1]),
	(4,[6]),
	(5,[3]),
	(6,[5])]
test1 = [(2,[3,4]), 
        (3,[]),
        (4,[6]),
        (5,[3]),
        (6,[5])]
test2 = [(2,[4]), 
        (4,[6]),
        (5,[]),
        (6,[5])]
test3 = [(2,[4]), 
        (4,[6]),
        (6,[])]
test4 = [(2,[4]), 
        (4,[])]
test5 = [(2,[])]

separateSub sub xs = separateSub' [] sub xs
	where
		separateSub' r sub xs
			| take (length sub) xs == sub = (reverse r,drop (length sub)xs)
		separateSub' r sub (x:xs) = separateSub' (x:r) sub xs

chunk _ [] = []
chunk n xs = take n xs:chunk n (leave n xs)

currylist f = (\(x:y:_)-> f x y)

filter1 f (x:xs)
	| f x = x:filter1 f xs
	| True = xs



lowercase a | fromEnum a <= fromEnum 'Z' && fromEnum a >= fromEnum 'A' = toEnum (fromEnum 'a' + (fromEnum a - fromEnum 'A'))
lowercase a = a

doall [x] = do x
doall (x:xs) = do
	x
	doall xs


inrange (min,max) num = num >= min && num <= max

toList (a,b) = [a,b]
toPair [a,b] = (a,b)

fromHexChar '0' = 0
fromHexChar '1' = 1 
fromHexChar '2' = 2 
fromHexChar '3' = 3 
fromHexChar '4' = 4
fromHexChar '5' = 5 
fromHexChar '6' = 6 
fromHexChar '7' = 7 
fromHexChar '8' = 8 
fromHexChar '9' = 9 
fromHexChar 'A' = 10 
fromHexChar 'B' = 11 
fromHexChar 'C' = 12 
fromHexChar 'D' = 13 
fromHexChar 'E' = 14 
fromHexChar 'F' = 15 
fromHexChar 'a' = 10 
fromHexChar 'b' = 11 
fromHexChar 'c' = 12 
fromHexChar 'd' = 13 
fromHexChar 'e' = 14 
fromHexChar 'f' = 15 

shiftLeft1 x = x+x
shiftRight1 x = div x 2

shiftLeft 0 x = x
shiftLeft n x = shiftLeft (n-1)(shiftLeft1 x)

shiftRight 0 x = x
shiftRight n x = shiftRight (n-1)(shiftRight1 x)

bitOr8 x y = bits2num(zipWith((||)) (num2bitsSize 8 x) (num2bitsSize 8 y))
bitAnd8 x y = bits2num(zipWith((&&)) (num2bitsSize 8 x) (num2bitsSize 8 y))

bits2num [] = 0
bits2num (bits) = 2*bits2num (init bits) + fromEnum( last bits)

bit n 0 = odd n
bit n d = bit  (div ( n)2) (d-1)

num2bitsSize l = extend l False . num2bits

num2bits 0 = []
num2bits num = num2bits (div num 2) ++ [odd num]
extend l d xs = (take l xs) ++ take (max(length xs)l - l) (repeat d)


unescape s= fixs --  ++ (if (fromEnum sumb) == 0 then [] else [sumb])
	where
--		sumb = toEnum (foldl fix2 0 fixs)
		fixs = fix s
		fix [] = []
		fix ('%':s:t:ring) = toEnum(( (fromHexChar s)*16)+ (fromHexChar t)) : fix ring
		fix ('+':tring) = ' ':fix tring
		fix (s:tring) = s:fix tring
		
--		fix2 sumb b | bitAnd8 (fromEnum b) 0xc8 == 0x80 = (sumb * 64 )+(bitAnd8 (fromEnum b) 0x3f)
--		fix2 sumb b | bitAnd8 (fromEnum b) 0x80 == 0x00 = fromEnum b
--		fix2 sumb b = bitAnd8 (fromEnum b) 0x1f

repchar fromchar tochar = repchar'
			where
				repchar' [] = []
				repchar' (s:tr)	| s == fromchar = tochar:repchar' tr
						| True = s:repchar' tr

