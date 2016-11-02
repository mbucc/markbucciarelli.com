all: setup ebin/appserver_sup.beam ebin/appserver_callback.beam

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

.PHONY: clean
clean:
	rm -f ebin/*.beam
