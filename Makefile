NOW = $(shell date +%s)

www.tgz:  www/*.html www/*.css www/img/* www/imginfo/* www/robots.txt www/keybase.txt www/feed.rss www/sitemap.xml
	tar czvf www.tgz ./www

.PHONEY: deploy
deploy: www.tgz
	scp $? tinyvz:/tmp/www.${NOW}.tgz
