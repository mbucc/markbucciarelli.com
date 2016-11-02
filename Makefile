SRC=${HOME}/src

all: css

.PHONEY: deploy
deploy:
	(cd rawimg; make)
	(cd src ; make)
	rsync -avz ./www/ tinyvz:/home/markbucciarelli/www
	ssh tinyvz chown -R markbucciarelli:markbucciarelli /home/markbucciarelli/www

css: www/tufte.css

PATCH=${SRC}/tufte-css_with-wrapping/HEAD_with-wrapping.patch
CSS=${SRC}/tufte-css/tufte.css
www/tufte.css: ${CSS} ${PATCH}
	cp ${CSS} $@
	(cd www ; patch < ${PATCH})
