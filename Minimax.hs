module Minimax where

import Tree
import List
import Thtools

inf = 999
win = 998

contGameNoMoves	(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = c
next		(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = n
move		(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = m
static		(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = e
findwinner	(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = w
newboard	(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = blank
curplayer	(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = cp
showboard	(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = sb
parser		(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = mp
unparse		(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = up
quickstatic	(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = qe
datapath	(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = dp
args		(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = ar
setargs		(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,_,nm) ar = (c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm)
nullmove	(c,n,m,e,w,blank,cp,sb,mp,up,qe,dp,ar,nm) = nm

moves rules b= map (\m->(move rules b m,m))(next rules b)

maximise = minimax 1
minimise = minimax (-1)

{-
minimax player ab (Node _ l)= mml player(minnums:omit (-player) minnums rest)
        where
                (nums:rest) = (map (minimax' (player)) l)
                minnums = mml (player) (ab:nums)
                minimax' p (Node n []) = (n:[])
		minimax' p (Node _ l) = (minnums:omit p minnums rest)
			where
                                (nums:rest) = (map (minimax' (-p)) l)
                                minnums = mml (-p) nums
                mmleq p [] _ = False
                mmleq p (num:rest) pot
                        | p*num <= p*pot = True
                        | True = mmleq p rest pot
                omit _ _ [] = []
                omit p pot (nums:rest)
                        | mmleq p nums pot = omit p pot rest
                        | True = (minnums:omit p minnums rest)
                        where
                                minnums = mml(-p)nums


-}

minimax player = mmlWith fst player.minimax' player
	where
		minimax' p (Node n []) = [n]
		minimax' p (Node n _) | p*(abs (fst n)) >= win = [n]
		minimax' p (Node (_,m) l) = (mapmm p (map ( (minimax'' (-p))) ({-sortComp (order p)-} l)))
			where
				mapmm p(nums:rest) = (minnums:omit p (fst minnums) rest)
					where
						minnums = mmlWith fst (-p) nums
		minimax'' p (Node n []) = [n]
		minimax'' p (Node n _) | p*(abs (fst n)) >= win = [n]
		minimax'' p (Node (_,m) l) = map(\(x,y)->(x,m))(mapmm p (map ( (minimax'' (-p))) l))
			where
				mapmm p(nums:rest) = (minnums:omit p (fst minnums) rest)
					where
						minnums = mmlWith fst (-p) nums
                mmleq p [] _ = False
                mmleq p (num:rest) pot
                        | p*fst num <= p*pot = True
                        | True = mmleq p rest pot
		omit p _ [] = []
                omit p pot (nums:rest)
			| pot == (p) * win = [] -- ??????
                        | mmleq p nums pot = omit p pot rest
                        | True = (minnums:omit p (fst minnums) rest)
                        where
                                minnums = mmlWith fst(-p)nums





		
ordermoves p (Node n sub) = Node n (map (ordermoves(-p))(sortWith (\(Node _ s1)->length s1) sub))
--lowfirst (Node n sub) = Node n (sortComp (\x y->not(higher x y))(map highfirst sub))
--order p (Node n1 sub1) (Node n2 sub2) = p*fst n1<p*fst n2

taperAt n tn (Node a xs) = taperAt' n (Node a xs)
	where
		taperAt' 0 node = taper tn node
		taperAt' n (Node a xs) = Node a (map (taperAt' (n-1) ) xs)
taper 0 (Node a _)= Node a []
taper n (Node a xs) = Node a (zipWith apply xs (map taper [n-1,n-2..0]))
treeSize (Node _ []) = 1
treeSize (Node _ xs) = 1+sum(map treeSize xs)

prune 0 (Node a _) = Node a []
prune n (Node a x) = Node a (map (prune (n-1)) x)

limitbranch n (Node a []) = Node a []
limitbranch n (Node a x) = Node a (map (limitbranch n) (take n x))

inftree = inftree' 0
	where
		inftree' n = Node [n] (repeat (inftree' (n+1)))

reptree f a = Node a (map (reptree f)(f (fst a)))
gametree rules board =
	reptree
	(mergeSortWith
		(\(x,_)->((*(toInteger(-(curplayer rules x)))).quickstatic rules) x).moves rules)
	(board,((0,0),(-1,[])))

testtree h w = prune h (limitbranch w inftree)

--evaluate n tn rules b=
--	(minimax(curplayer rules b) .ordermoves (curplayer rules b).mapVTree (\(x,y)->(static rules x,y)).taperAt n tn.gametree rules)b
evaluate n tn rules b=
	(minimax(curplayer rules b).ordermoves (curplayer rules b).mapVTree (\(x,y)->(static rules x,y)).taperAt n tn.gametree rules)b
showgame n rules=putStr.concat.nodes.mapVTree (showboard rules).prune n.gametree rules

--test = evaluate 3(ofk[])1 ofknewboard
-- evaluate'' 2 (ofk[]) test
-- evaluate' 2 (ttt[]) test

