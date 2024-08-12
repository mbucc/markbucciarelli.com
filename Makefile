SITE=http://markbucciarelli.com

.PHONY: site
site: posts root css fonts images
	find work -type f -exec chmod 644 {} \;

.PHONY: root
root:
	mkdir -p work
	(cd work && MKWSTHEMEDIR=../src/share ../bin/mkws ${SITE} ../src)

.PHONY: posts
posts:
	mkdir -p work/posts
	(cd work/posts && MKWSTHEMEDIR=../../src/share ../../bin/mkws ${SITE} ../../src/posts)

.PHONY: css
css: work/css/tufte.tweaks.css work/css/tufte.min.css

work/css/%.css: src/css/%.css
	mkdir -p work/css
	cp $? $@

.PHONY: fonts
fonts:
	cp -r src/css/et-book ./work/css/

.PHONY: images
images:
	mkdir -p work/img
	cp -r src/img work/
	mkdir -p work/imginfo
	cp src/imginfo/* work/imginfo/

.PHONY: deploy
deploy: site
	aws --profile blog s3 sync work s3://blog-content-mb

.PHONY: clean
clean:
	rm -rf work/*
