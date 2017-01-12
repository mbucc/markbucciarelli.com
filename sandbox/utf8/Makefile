all: utf8.txt \
		ebin/file_read_file.beam \
		ebin/es_sup.beam \
		ebin/es_callback.beam \
		ebin/erlsrv.app \
		ebin/erlsrv.beam

.PHONY: test1
test1: file_read_file.beam
	erl -pa ebin -s file_read_file -s init stop

utf8.txt: makeutf8.sh
	./makeutf8.sh > $@


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
	rm -f utf8.txt
	rm -f *.dump

