BEGIN    { first=1; hadsection=0; }
         { p=q; q=$0 }
/^----*/ { if (!first) print "</section>"
           print "<section>"
           print p;
           first = 0
           next
         }
NR > 1   { print p }
END      { print q
           if (!first) print "</section>"
         }
