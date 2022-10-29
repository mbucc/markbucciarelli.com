#! /bin/sh -e

cat test.markdown| awk -f ../bin/margin_notes.awk > t0
cat t0 | awk -f ../bin/sections.awk > t1
cat t1 | awk -f ../bin/figures.awk > t2
mv t2 test.actual
rm -f t[0-9]

diff -uw test.expected test.actual
