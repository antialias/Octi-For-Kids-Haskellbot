module Game where

import Minimax
import Thtools
import CGI

data PlayerAction = Play | AlertWin

computermove rules _ _ b AlertWin = return (nullmove rules)
computermove rules n tn b Play = do
			putStrLn (unparse rules bestmove)
			return(bestmove)
	where
		bestmove = snd (evaluate n tn rules b)
		nextrulesb = next rules b

humanmove rules b AlertWin = do
	putStrLn ("Player " ++ show (fst b)++" wins.")
	return (nullmove rules)
humanmove rules b Play= do
			putStrLn (showboard rules b)
			mv <- getLine
			if elem mv (map (unparse rules) (next rules b))
				then
					return (parser rules mv)
				else
					do
						putStrLn "illegal move"
						humanmove rules b Play
	where
		nextrulesb = next rules b

webmove rules b AlertWin =  do
			putStrLn (htmldoc "Game Over" ["Player " , show (fst b) , " wins.<br>" ,
				htmllink "/ofk/ai/" (preformatted (showboard rules  b))])
			return (nullmove rules)

webmove rules b Play= do
			putStrLn "<html><title>Octi For Kids Haskell bot: by Thomas Hallock</title><body>"
			putStrLn "<form method = get action = /cgi-bin/ofk/fcgi>"
			putStrLn ("players: <input type = text name = players value = \"" ++ args rules ++ "\"><p>")
			putStrLn "<table border = 0><tr><td valign = top align = left>"
			putStrLn ("<p><font size = -2><pre>"++ (showboard rules b) ++ "</pre></font></p>")
			putStrLn ("board configuration:<br>" ++ inputtext "board" 24 (show b))
			putStrLn "<td valign = top align = right>"
			putStrLn "<br>type move:<input type = text name = move>"
			putStrLn "<input type = submit value = move><br>"
			params <- return(encode("board="++(show b)++"&players="++(args rules)))
			putStrLn ("click move:<br>"++
		              (( concat
		                ((map
		                (\x->
		                        " <a href = \"/cgi-bin/ofk/fcgi?"++
		                        ((((params++"&move=")++).(encode.(unparse rules)))x)++
		                        "\">"++unparse rules x++
		                        "</a>")
		                (next rules b)))++"<br>")))
			putStrLn "</table>"
			putStrLn "</form>"
			putStrLn (htmllink ("/cgi-bin/ofk/fcgi?args="++encode(args rules)) "New Game")
			putStrLn "</body></html>"
			return (nullmove rules)
	where
		nextrulesb = next rules b

makeplayers rules [] = []
makeplayers rules (('c',n1,tn2):rest) = computermove rules n1 tn2 : makeplayers rules rest
makeplayers rules (('h',_,_):rest) = humanmove rules : makeplayers rules rest
makeplayers rules (('w',_,_):rest) = webmove rules : makeplayers rules rest

play rules players = playgame rules (makeplayers rules players)

playgame rules (players@[p1',p2']) = do
	play' (p1') (p2') (newboard rules)
	where
		play' p1 p2 b = do
			if (findwinner rules b == 0) && (length nextrulesb >0 || (length nextrulesb <=0 && contGameNoMoves rules))
				then do
					themove <- p1 b Play
					if (themove == nullmove rules)
						then do
							return (b)
						else do 
							nb <- return (move rules b themove)
							play' p2 p1 nb
				else do
					doall (map (\x-> x b AlertWin) players) 
					return (b)
	
			where
				nextrulesb = next rules b

playgame rules [player] = do
	b <- return (newboard rules)
	putStrLn "got here"
	putStrLn (show b)
	putStrLn(concat(map ((++", ").unparse rules) (next rules b)))
	return (b)
	themove <- player b Play
	if(nullmove rules == themove)
		then do
			return (b)
		else do
			theboard <- return (move rules b themove)
			return (theboard)
