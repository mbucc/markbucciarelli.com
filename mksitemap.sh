#! /bin/sh -e

for f in $(find _site -type f | grep \.html)
do
	f1=$(echo $f | sed 's;_site/;;')
	echo http://markbucciarelli.com/$f1
done
