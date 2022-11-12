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

.PHONY: orch
orch:
	(cd deployments ; ansible-playbook apache.yml)

.PHONY: deploytest
deploytest: site orch
	rsync -avz ./work/ testblog:/var/www/test.markbucciarelli.com

.PHONY: clean
clean:
	rm -rf work/*
