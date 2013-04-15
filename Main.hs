module Main where

import Game
import Minimax
import Thtools
import System.Environment
import CGI
import Ofk



handleArgs [] = []
handleArgs (('-':'c':'g':'i':'=':rest):rgs)=parseNV rest ++ handleArgs rgs
handleArgs (a:rgs) = makePair(chopAt '=' a):handleArgs rgs



handleBoard rules nvp
	| not (null boardstr) = read boardstr
	| True = newboard rules
	where
		boardstr = getValue nvp "board"
handleMove rules nvp
	| not (null movestr) = parser rules movestr --(parsemove movestr)
	| True = nullmove rules
	where
		movestr = getValue nvp "move"
handlePlayers rules nvp
	| not (null playerstr) = createplayers (setargs rules playerstr) (chopAt ':' playerstr)
	| True = createplayers rules (chopAt ':' (args rules))
	where
		playerstr = getValue nvp "players"

getPlayerstr rules nvp
	| null playerstr = args rules
	| True = playerstr
	where
		playerstr = getValue nvp "players"

createplayers _ [] = []
createplayers rules (['h']:layers) = humanmove rules:createplayers rules layers
createplayers rules (['w']:layers) = webmove rules:createplayers rules layers
createplayers rules (('c':n):layers) = computermove rules(read n)0:createplayers rules layers

main = do
	args<-getArgs
	nvp<-return(handleArgs args)
	rules <- return (ofk[])

	thepreboard <- return (handleBoard rules nvp)
	themove <- return(handleMove rules nvp)
	theboard <- return (move rules(thepreboard)(themove))
	playerstr <- return (getPlayerstr rules nvp)
	thesetup <- return (setboard(theboard)(setargs rules playerstr))
	theplayers <- return(handlePlayers thesetup nvp)
	moveboard <- playgame thesetup  theplayers
	return ()
	where
		curplayername (-1,_) = "One"
		curplayername (1,_) = "Two"

getParams = do
	putStrLn "how many moves should player 1 look ahead?"
	n1s<-getLine
	putStrLn "how many moves should player 2 look ahead?"
	n2s<-getLine
	putStrLn "after a full search, how many moves should I taper player 1?"
	tn1s<-getLine
	putStrLn "after a full search, how many moves should I taper player 2?"
	tn2s<-getLine
	putStrLn "Is player 1 a (C)omputer or a (H)uman?"
	(p1:_) <- getLine
	putStrLn "Is player 2 a (C)omputer or a (H)uman?"
	(p2:_) <- getLine
	n1<-return(read n1s :: Int)
	n2<-return(read n2s :: Int)
	tn1<-return(read tn1s :: Int)
	tn2<-return(read tn2s :: Int)
	return ([(p1,n1,tn1),(p2,n2,tn2)])

parseParams str = parseParams' str []
	where
		parseParams' (('-':'w':[]):str) players = parseParams' str (('w',0,0):players)
		parseParams' (('-':'c':ns):str) players = parseParams' str (('c',read ns::Int,0):players)
		parseParams' (('-':'t':tns):str) (('c',n,0):players) = parseParams' str (('c',n,read tns::Int):players)
		parseParams' (('-':'h':[]):str) players = parseParams' str (('h',0,0):players)
		parseParams' [] players = reverse players


