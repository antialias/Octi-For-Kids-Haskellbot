module CGI where

import Thtools

parseNV [] = []
parseNV text = map (makePair . chopAt '=') (chopAt '&' (unescape text))


encode [] = []
encode (t:ext) = (fromascii t) ++ encode ext
fromascii '+' = "%2B"
fromascii '(' = "%28"
fromascii ')' = "%29"
fromascii ' ' = "%20"
fromascii c = [c]

chopAt c = chopAt' [[]]
			where
				chopAt' r [] = reverse (map reverse r)
				chopAt' (r:et) (t:ext) | t == c = chopAt' ([]:(r:et)) ext
				chopAt' (r:et) (t:ext) = chopAt' ((t:r):et) ext
makePair [x,y] = (x,y)

getValue nvp key= if(null stuff)then "" else (snd(head(stuff)))
	where
		stuff = filter((== key).fst)nvp
text2html ('\n':ext) = "<br>" ++ text2html ext
text2html (t:ext) = t:text2html ext

inputtext name width value = "<input type = text name = "++name++" size = " ++ show width ++ " value = \"" ++ value ++ "\">"

preformatted str = "<pre>"++str++"</pre>"

htmlheader title = "<html><title>"++title++"</title><body>"
htmlfooter = "</body></html>"

htmldoc title body = htmlheader title ++ (concat body) ++ htmlfooter

htmllink address text = "<a href = \"" ++ address ++ "\">" ++ text ++ "</a>"
