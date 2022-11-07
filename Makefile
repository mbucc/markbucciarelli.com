SITE=http://markbucciarelli.com

.PHONY: site
site: posts work/css/tufte.tweaks.css work/css/tufte.min.css

.PHONY: posts
posts:
	mkdir -p work/posts
	(cd work/posts && MKWSTHEMEDIR=../../src/share ../../bin/mkws ${SITE} ../../src/posts)

work/css/%.css: src/css/%.css
	mkdir -p work/css
	cp $? $@

.PHONY: deploy
deploy: site
	./mksitemap.sh > _site/sitemap.txt
	eval `ssh-agent`
	ssh-add $$HOME/.ssh/id_tinyvz_production
	rsync -avz ./_site/ tinyvz:/home/markbucciarelli/work
	ssh tinyvz chown -R markbucciarelli:markbucciarelli /home/markbucciarelli/work

.PHONY: clean
clean:
	rm -rf work
