SITE=http://markbucciarelli.com

.PHONY: site
site: posts

.PHONY: posts
posts:
	mkdir -p www/posts
	(cd www/posts && MKWSTHEMEDIR=../../share ../../bin/mkws ${SITE} ../../posts)

.PHONY: deploy
deploy: site
	./mksitemap.sh > _site/sitemap.txt
	eval `ssh-agent`
	ssh-add $$HOME/.ssh/id_tinyvz_production
	rsync -avz ./_site/ tinyvz:/home/markbucciarelli/www
	ssh tinyvz chown -R markbucciarelli:markbucciarelli /home/markbucciarelli/www
