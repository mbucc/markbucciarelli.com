SRC=${HOME}/src

all: css www

.PHONEY: deploy
deploy: www
	rsync -avz ./www/ tinyvz:/home/markbucciarelli/www
	ssh tinyvz chown -R markbucciarelli:markbucciarelli /home/markbucciarelli/www

.PHONY: www
www:
	(cd src ; make)

css: www/tufte.css

PATCH=${SRC}/tufte-css_with-wrapping/HEAD_with-wrapping.patch
CSS=${SRC}/tufte-css/tufte.css
www/tufte.css: ${CSS} ${PATCH}
	rm -f www/line-wrap-arrow.svg
	cp ${CSS} $@
	(cd www ; patch < ${PATCH})
	(cd www ; mv line-wrap-arrow.svg ./img/)

.PHONY: clean
clean:
	rm -f  www/tufte.css www/img/line-wrap-arrow.svg
	rm -f www/*.rej www/*.orig
	(cd src ; make clean)
