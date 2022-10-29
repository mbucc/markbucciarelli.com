BEGIN    { first=1; }
         { p=q; q=$0 }
/^----*/ { if (!first) print "</section>"
           print "\n<section>"
           print p;
           first = 0
           next
         }
         { print p }
END      { print q
           print "</section>"
         }
