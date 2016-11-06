#! /bin/sh -e
# Convert stdin to HTML entities and send to stdout.

sed -E \
	-e "s/\`\`/\&ldquo;/g" \
	-e "s/\`/\&lsquo;/g" \
	-e "s/''/\&rdquo;/g" \
	-e "s/'/\&rsquo;/g" \
	-e 's/\.\.\./\&hellip;/g' \
	-e 's/---/\&mdash;/g'
