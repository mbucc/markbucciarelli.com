#! /bin/sh -e
# Create HTML page will full res picture.
# Usage: ./html.sh ../www/picture_info/mnopq.html

# 
#   ../www/picture_info/mnopq.html --> info.mnopq
#

FN=info.$(echo $1 | cut -d '/' -f 4 | sed 's/\.html//')

. $FN

. mustache.sh

mustache < template.mustache
