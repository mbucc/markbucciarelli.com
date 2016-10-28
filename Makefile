.PHONEY: deploy
deploy:
	(cd rawimg; make)
	(cd src ; make)
	rsync -avz ./www/ tinyvz:/home/markbucciarelli/www
	ssh tinyvz chown -R markbucciarelli:markbucciarelli /home/markbucciarelli/www
