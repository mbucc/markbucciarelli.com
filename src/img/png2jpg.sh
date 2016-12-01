#! /bin/sh -e
# Avoid black background when converting png to jpg.  Remove alpha transparency.

[ "x$1" = "x" ] && printf "usage: %s <img>\n" $(basename $0) 1>&2 && exit 1

f=$1
f1=$(echo $f|sed 's/\.png$/\.jpg/')

[ "x$f" = "x$f1" ] && printf "error: file must be a png file\n" 1>&2 && exit 1

convert $f -background white -alpha remove $f1
