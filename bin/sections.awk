BEGIN	{
		first=1
	}

/<h2/	{
		if (!first) print "</section>"
		print "<section>"
		print $0
		first=0
		next
	}

	{ print }

END	{
		if (!first) print "</section>"
	}
