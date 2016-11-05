all: setup ebin/ms_sup.beam ebin/ms_callback.beam ebin/microserver.app ebin/microserver.beam

setup: ebin src include priv
ebin:
	mkdir ebin 
src:
	mkdir src 
include:
	mkdir include 
# priv: application specific files. Use code:priv_dir/1 to access this directory.
priv:
	mkdir priv 

ELLI=${HOME}/src/elli
${ELLI}/ebin:
	(cd ${ELLI} ; make)

ebin/%.beam: src/%.erl ${ELLI}/ebin
	ERL_LIBS=${ELLI} erlc -o ebin/ $<

ebin/microserver.app: src/microserver.app.src
	cp $? $@

.PHONY: clean
clean:
	rm -f ebin/*
