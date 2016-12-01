#! /bin/sh -e
# Avoid black background when converting png to jpg.  Remove alpha transparency.

[ "x$1" = "x" ] && printf "usage: %s <img>\n" $(basename $0) 1>&2 && exit 1

[ ! -f "$1.bak" ] && cp -a $1 $1.bak
convert $1 -fuzz 20% -trim t.jpg
mv t.jpg $1
