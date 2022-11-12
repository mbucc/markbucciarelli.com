/<\? *mn:.*\?>/ {
	id = $3
	printf "<label for=%s class=margin-toggle>&#8853;</label>\n", id
	printf "<input type=checkbox id=%s class=margin-toggle />\n", id
	printf "<span class=marginnote >\n"
	next
}
/<\? *mn *\?>/ {
	printf "</span>\n"
	next
}
{
	print
}
