#! /bin/sh -e
# Convert a Hakyll html file to mkws.   Renders tags as margin note using tufte.css.
# October 22, 2022

tmpf=t
for f in $(ls *.upphtml); do
  [ "$f" = "index.upphtml" ] && continue
  title=$(grep -i '^title:' $f|sed -e 's/^[^:]*://' -e 's/^[ 	]*//' -e 's/[ 	]*$//' -e "s/^'//" -e "s/'$//")
  date=$(grep -i '^date:' $f|sed -e 's/^[^:]*://' -e 's/^[ 	]*//' -e 's/[ 	]*$//' -e "s/^'//" -e "s/'$//")
  tags=$(grep -i '^tags:' $f|sed -e 's/^[^:]*://' -e 's/^[ 	]*//' -e 's/[ 	]*$//' -e "s/^'//" -e "s/'$//")

  # 
  #          Write <article> tag, blog title, and blog tags.
  #
  rm -f "$tmpf"
  printf "<article>\n" >> "$tmpf"
  printf "    <h1>%s</h1>\n" "$title" >> "$tmpf"
  printf "    <label for=blog-title class=margin-toggle>&#8853;</label>\n" >> "$tmpf"
  printf "    <input type=checkbox id=blog-title class=margin-toggle/>\n" >> "$tmpf"
  printf "    <span class=marginnote id=blogtags>" >> "$tmpf"
  ti=0
  for tag in $(echo "$tags" | tr , ' '); do
    [ $ti -gt 0 ] && printf ", " >> "$tmpf"
    printf "\n        <a href=/tags.html#$tag>$tag</a>" >> "$tmpf"
    ti=$(( ti + 1 ))
  done
  printf "\n    </span>\n\n" >> "$tmpf"

  # 
  #          Write blog date.
  #
  printf "    <p class=subtitle>%s</p>\n" "$date" >> "$tmpf"
  printf "\n" >> "$tmpf"

  # 
  #          Write post body, indenting each line so it prints as
  #          a child to the enclosing section.
  #
  sed -n '/<section/,$p' "$f" | sed 's/^/    /' >> "$tmpf"

  printf "</article>\n" >> "$tmpf"

  mv "$tmpf" $f
done
