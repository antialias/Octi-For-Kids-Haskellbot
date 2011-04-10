#!/bin/sh
echo Content-type: text/html
echo 


./ofk.exe -cgi=$QUERY_STRING
