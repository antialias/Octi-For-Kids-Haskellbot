
module Ofk where

import Minimax
import List
import Thtools

type Ofkboard = (Integer,((Int,Int),[(Int,(Int,Int),[Int])]))

ofktest2::Ofkboard
ofktest2 = (1,((12,12),[(1,(2,0),[1,1,1,1,1,1,1,1]),
	(-1,(2,1),[0,0,0,0,0,0,0,0]),
	(-1,(0,1),[0,0,0,0,0,0,0,0]),
	(-1,(1,1),[0,0,0,0,0,0,0,0]),
	(-1,(1,2),[0,0,0,0,0,0,0,0]),

	(1,(1,5),[0,0,0,0,0,0,0,0]),
	(1,(2,5),[0,0,0,0,0,0,0,0])]))
test::Ofkboard
test = (1,((12,12),[(1,(2,0),[1,1,1,1,1,1,1,1]),
	(-1,(2,1),[0,0,0,0,0,0,0,0]),
	(-1,(3,2),[0,0,0,0,0,0,0,0]),
	(-1,(0,1),[0,0,0,0,0,0,0,0]),
	(-1,(1,1),[0,0,0,0,0,0,0,0]),
	(-1,(1,2),[0,0,0,0,0,0,0,0]),
	(-1,(3,3),[0,0,0,0,0,0,0,0]),

	(1,(1,5),[0,0,0,0,0,0,0,0]),
	(1,(2,5),[0,0,0,0,0,0,0,0]),
	(1,(3,5),[0,0,0,0,0,0,0,0]),
	(1,(4,5),[0,0,0,0,0,0,0,0])]))
winsit::Ofkboard
winsit = (1,((12,12),[(-1,(1,1),[0,0,0,0,0,0,0,0]),
	(-1,(3,1),[0,0,0,0,1,0,0,0]),
	(-1,(4,1),[0,0,1,1,0,1,1,0]),
	(-1,(3,2),[0,0,0,1,0,1,0,0]),
	(1,(5,2),[0,1,0,0,0,0,0,1]),
	(1,(3,3),[1,0,0,0,0,0,0,0]),
	(1,(4,3),[0,1,0,0,0,0,0,1]),
	(1,(1,5),[0,0,0,0,0,0,0,0])]))
ofknewboard::Ofkboard
ofknewboard = (1,((12,12),[(-1,(1,1),[0,0,0,0,0,0,0,0]),
	(-1,(2,1),[0,0,0,0,0,0,0,0]),
	(-1,(3,1),[0,0,0,0,0,0,0,0]),
	(-1,(4,1),[0,0,0,0,0,0,0,0]),

	(1,(1,5),[0,0,0,0,0,0,0,0]),
	(1,(2,5),[0,0,0,0,0,0,0,0]),
	(1,(3,5),[0,0,0,0,0,0,0,0]),
	(1,(4,5),[0,0,0,0,0,0,0,0])]))

ofkcurplayer(p,_) = p

pin (_,_,p) n = p!!n
prongs (_,_,p) = p
loc (_,l,_) = l
ofkplayer (p,_,_) = p


parsemove = parsemove'.map lowercase 
	where
		parsemove' (c:r:move) = (toPair (map fix [c,r]),parse' move)
			where
				parse' ('+':p:move) = (fix p,[])
				parse' (move) = (-1,map orderjump (chunk 3 move))
				orderjump ('x':cr) = (True,order cr)
				orderjump ('-':cr) = (False,order cr)
				order [c,r] | inrange ('a', 'f') c = (fix c,fix r)
				order [c,r] = (fix c,fix r)
				fix c | inrange ('a','h') c = fromEnum c - fromEnum 'a'
				fix c = fromEnum c - fromEnum '1'

ofkunparse ((x,y),(-1,ml)) = (toEnum(fromEnum '1'+x):toEnum(fromEnum '1' + y):rest ml)
	where
		rest [] = ""
		rest ((True,(x,y)):umps) = ('x':toEnum(fromEnum '1'+x):toEnum(fromEnum '1' + y):rest umps)
		rest ((False,(x,y)):umps) = ('-':toEnum(fromEnum '1'+x):toEnum(fromEnum '1' + y):rest umps)
ofkunparse ((x,y),(p,[])) = (toEnum(fromEnum '1'+x):toEnum(fromEnum '1' + y):'+':toEnum(fromEnum 'a'+p):[])


setloc(p,(c,r),prongs) (c',r') = (p,(c',r'),prongs)
setprong(p,(c,r),prongs) n = (p,(c,r),highprongs ++ [1] ++ lowprongs)
	where
		(highprongs,(_:lowprongs)) = splitAt n prongs

tween (c1,r1) (c2,r2) =(c1 + sign((c2-c1)),r1+(sign(r2-r1)))
tz l = zipWith tween l (tail l)
movepiece (p1,p2) piece (-1,jumps) others = ((p1',p2'),setloc piece (snd(last jumps)):others')
	where
		(others',taken) = partition (not.flip elem takes.loc) others
		(p1',p2')	| ofkplayer piece == 1 =	(p1,p2 + (sum (map (sum.prongs) taken)))
				| True =			(p1 + sum (map (sum.prongs) taken),p2)
		(takes) = image (map fst jumps)(tz(loc piece:map snd jumps))
		locs = map loc others
movepiece (p1,p2) piece (pn,jumps) others = ((p1',p2'),setprong piece pn:others)
	where
		(p1',p2')	| ofkplayer piece == 1 = (p1,p2-1)
				| True = (p1-1,p2)

ofkmove b ((-1,-1),(-1,[])) = b
ofkmove (p,(pc,b)) (piece,action) = (-p,movepiece pc thepiece action others)
	where
		
		((thepiece:_),others) = (partition ((piece==).loc) b)

prongdir x	| x > 3 = prongdir7 x
		| True = prongdir3 x
	where
		prongdir7 x	| x > 5 = prongdir7' x
				| True = prongdir5' x
		prongdir7' 7 = (-1,-1)
		prongdir7' 6 = (-1,0)
		prongdir5' 5 = (-1,1)
		prongdir5' 4 = (0,1)
		prongdir3 x	| x > 1 = prongdir3' x
				| True = prongdir1' x
		prongdir3' 3 = (1,1)
		prongdir3' 2 = (1,0)
		prongdir1' 1 = (1,-1)
		prongdir1' 0 = (0,-1)
{-
prongdir 0 = (0,-1)
prongdir 1 = (1,-1)
prongdir 2 = (1,0)
prongdir 3 = (1,1)
prongdir 4 = (0,1)
prongdir 5 = (-1,1)
prongdir 6 = (-1,0)
prongdir 7 = (-1,-1)
-}
steppod p (xs,ys) = setloc p (x+xs,y+ys)
	where
		(x,y) = loc p

--a -> [(b,(Integer,Integer),[Integer])] -> [(b,(Integer,Integer),[Integer])],

--Integer -> [(Integer,(Integer,Integer),[Integer])] -> [((Int,Int),(Int,[(Bool,(Int,Int))]))],

ofknext (pn,(pc,bo))
	| or( map ((/=0).ongoal) bo) = []
	| True = (concat.concat)(map (ofknext' pc) (filter (\x->((fromInteger pn) == (ofkplayer x))) bo))
	where
		ofknext' (p1,p2) p
			| -1 == ofkplayer p && p1 <1 = [jumps,moves]
			| 1 == ofkplayer p && p2 <1 = [jumps,moves]
			| True = [jumps,addprongs,moves]
			where
				(taken',vacant') = (partition((==1).fst)(zip(prongs p)[0..]))
				moves = map(\x->((loc p),(-1,[(False,x)]))) (filter(\x-> (not.occupied bolocs) x && onboard x)(map (addpair (loc p)) prongdirs))
				addprongs = map (\x->((loc p),(x,[]))) vacant
				bolocs = filter1 (/=(loc p))(map loc bo)
				taken = map snd taken'
				vacant = map snd vacant'
				prongdirs = map prongdir taken
				occupied ol = flip elem ol
				onboard (x,y) = x>=0 && x< 6 && y>=0 && y<7
				
				jumps = map ((\x->(loc p,(-1, x))).uncurry zip)
                                    (concat(( map ( uncurry zip. (\x->(kleene [True,False](length (x) - 1),repeat (tail x)))))
                                    rest))
						where
							(_:rest) = filter (not.null)( ((jumps') (loc p,bolocs)))
                                                        jumps' (l,ol) = map(l:)([[]] ++ concat((	map
											jumps'
											(	zip
												jp
												(map (set_remove ol) tj)
											))))

										
									
								where
									tj = map (tween l) jp
                                                                        jp = filter (\x->onboard x && not(occupied ol x) && occupied ol(tween l x))
                                                                            (map (addpair l.multpairconst 2) prongdirs)
					
				



--  \|/
--  -X-
--  /|\

lower x y
	| snd locx < snd locy = True
	| snd locx == snd locy && fst locx < fst locy = True
	| True = False
	where
		locx = loc x
		locy = loc y

sortpiecesloc [] = []
sortpiecesloc (b:bo) = sortpiecesloc low ++ [b] ++ sortpiecesloc high
	where
		(high,low) = partition (lower b) bo

ofkboardstr (curplayer,((p1,p2),bo)) =
	"North: " ++ show p1 ++" prongs\n" ++ 
	concat( replicate 6 "+---")++
	"+\n"++
	(fst (foldl (\(s,x) y -> applyfst(++) s (y x)) ("",sortpiecesloc bo)
	 (map showline [0..6])))
	++ "South: " ++ show p2 ++ " prongs"
	where
		showline y b = foldl1(\(x,y)(z,_)  -> (x++z,y)) (map (showline' 0 ([],b))[0..3])
			where
				showline' _ _ 3 = (concat( replicate 6 "+---")++"+\n",[])
				showline' 6 (r,bs) _= (r++"|\n",bs)
				showline' x (r,[]) yrow = showline' (x+1) (r++shownull,[]) yrow
				showline' x (r,(b:bs)) yrow
					| loc b == (x,y) = showline' (x+1) (r ++ (rowchars b yrow),bs) yrow
					| True = showline' (x+1) (r++shownull,(b:bs)) yrow
		prongorder = [1,2,4,7,6,5,3,0]
		prongchar n = (reorder "|/-\\|/-\\" prongorder)!!n
		prongchars p = map foo (zip ( (reorder (prongs p)prongorder)) [0..])
			where
				foo (1,y) = prongchar y
				foo (0,_) = ' '
		showpod 1 = 'X'
		showpod (-1) = 'O'
		rowchars p r = "|"++ (!!r) (
			segment 3 ((take 3 (prongchars p)) ++
			(prongchars p!! 3 : showpod (ofkplayer p) : prongchars p!!4 : []) ++
			(leave 5 (prongchars p))))
		shownull = "|   "



ofkevalfloat :: (Num a, Integral b, Num c) => (d,((b,b),[(b,(a,b),[b])])) -> c
ofkevalfloat = fromInteger.ofkeval

ofkeval :: (Integral a, Num b) => (c,((a,a),[(a,(b,a),[a])])) -> Integer

ofkeval (_,(pc,b))
	| not (null gmax) = win
	| not (null gmin) = -win
	where
		goals  = map ongoal b
		gmax=filter (1==) goals
		gmin=filter ((-1)==) goals
ofkeval (_,((p1,p2),b)) = toInteger ofkeval'
	where
		ofkeval' = dot_product weights (map (sum.flip map b) funcs ++ [p2-p1])
		funcs = [dg,ofkplayer,sum.prongs]
		weights = [3,40,2,1]
		dg (1,(x,y),_) = (5-abs(1-y))
		dg (-1,(x,y),_) = (-(5-abs(5-y)))

ofkquickeval (_,(pc,b)) = 0::Integer--fromInteger(sum(map (ofkplayer) b))

ongoal (1,(1,1),_) = 1
ongoal (1,(2,1),_) = 1
ongoal (1,(3,1),_) = 1
ongoal (1,(4,1),_) = 1
ongoal (-1,(1,5),_) = -1
ongoal (-1,(2,5),_) = -1
ongoal (-1,(3,5),_) = -1
ongoal (-1,(4,5),_) = -1
ongoal _ = 0

ofkfindwinner (_,(pc,b)) = w(filter (/=0)(map ongoal b))
	where
		w[] = 0
		w(x:_)=x

-- we need a bogus argument here for some goddamn reason. I am not sure why Haskell treats constants different than applied functions

ofk :: [Int]-> (	Bool,
		(Integer,((Int,Int),[(Int,(Int,Int),[Int])])) -> [((Int,Int),(Int,[(Bool,(Int,Int))]))],
		(Integer,((Int,Int),[(Int,(Int,Int),[Int])])) -> ((Int,Int),(Int,[(Bool,(Int,Int))])) -> (Integer,((Int,Int),[(Int,(Int,Int),[Int])])),
		(Integer,((Int,Int),[(Int,(Int,Int),[Int])])) -> Integer,
		(Integer,((Int,Int),[(Int,(Int,Int),[Int])])) -> Integer,
		(Integer,((Int,Int),[(Int,(Int,Int),[Int])])),
		(Integer,((Int,Int),[(Int,(Int,Int),[Int])])) -> Integer,
		(Integer,((Int,Int),[(Int,(Int,Int),[Int])])) -> [Char],
		[Char] -> ((Int,Int),(Int,[(Bool,(Int,Int))])),
		((Int,Int),(Int,[(Bool,(Int,Int))])) -> [Char],
		(Integer,((Int,Int),[(Int,(Int,Int),[Int])])) -> Integer,
		[Char],
		[Char],
		((Int,Int),(Int,[(Bool,(Int,Int))]))
		)

ofk _ = (	False,
		ofknext,
		ofkmove,
		ofkeval,
		ofkfindwinner,
		ofknewboard,--winsit,--ofknewboard,
		ofkcurplayer,
		ofkboardstr,
		parsemove,
		ofkunparse,
		ofkquickeval,
		"ofkdata",
		"h:c3",
		ofknullmove)
ofknullmove ::((Int,Int),(Int,[(Bool,(Int,Int))]))
ofknullmove = (((-1,-1),(-1,[])))
setboard nb (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (a,b,c,d,e,nb,g,h,i,j,k,l,m,n)


