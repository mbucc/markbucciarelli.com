#! /bin/sh -e

# "quoted" (but with curly quotes)
#
# http://unix.stackexchange.com/a/189810
#
#  character          byte(s), hex
#  -----------------  ------------
#  left_curly_quote   e2  80  9c
#  q                  71
#  u                  75
#  o                  6F
#  t                  74
#  e                  65
#  d                  64
#  right_curly_quote  e2  80  9d

dc<<EOF
16i0
$(printf %sP E2 80 9C 71 75 6F 74 65 64 E2 80 9D)
EOF


