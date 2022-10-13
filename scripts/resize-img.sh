#! /bin/sh -e
# Resize an image to 700 pixels wide, and limit color palette to 256 colors.
# October 10, 2022

SRC="$1"
DST="$2"

[ "x$SRC" = "x" ] && echo "usage: $(basename $0) src dst" >&2 && exit 1
[ "x$DST" = "x" ] && echo "usage: $(basename $0) src dst" >&2 && exit 1

# For the png8 argument, see http://stackoverflow.com/a/11408390
#convert -interlace NONE -resize 700 "$SRC" png8:"$DST"
convert -interlace NONE -resize 700 "$SRC" "$DST"
