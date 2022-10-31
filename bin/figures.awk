BEGIN {
		fullwidth=0
	}

/<\? *fullwidth *\?>/	{
		fullwidth=1
		next
	}

/<img/	{
		printf "<figure"
		if (fullwidth) printf " class=fullwidth"
		print ">"
		fullwidth=0
		# cmark wraps image tags in paragraph and Tufte CSS
		# says not to use paragraphs, just a figure tag.
                gsub("</?p>","")
		print
		print "</figure>"
		next
	}

{ print }
