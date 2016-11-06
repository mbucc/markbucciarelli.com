all: ebin/es_sup.beam ebin/es_callback.beam ebin/erlsrv.app ebin/erlsrv.beam

ELLI=${HOME}/src/elli
${ELLI}/ebin:
	(cd ${ELLI} ; make)

ebin/%.beam: src/%.erl ${ELLI}/ebin
	ERL_LIBS=${ELLI} erlc -o ebin/ $<

ebin/erlsrv.app: src/erlsrv.app.src
	cp $? $@

.PHONY: clean
clean:
	rm -f ebin/*
