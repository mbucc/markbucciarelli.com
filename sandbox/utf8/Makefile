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



#-------------------------- START ELLI DEPENDENCY
lib/elli/%: ${HOME}/src/elli/%
	mkdir -p lib/elli
	cp -r $? lib/elli

${HOME}/src/elli/ebin ${HOME}/src/elli/include:
	(cd ${HOME}/src/elli ; make)
#---------------------------- END ELLI DEPENDENCY

#---------------------- START MUSTACHE DEPENDENCY
lib/%/ebin: ${HOME}/src/bbmustache/_build/dev/lib/%/ebin
	mkdir -p ${basename $@}
	cp -r $? ${basename $@}

${HOME}/src/bbmustache/_build/dev/lib/%/ebin: ${HOME}/src/bbmustache
	(cd ${HOME}/src/bbmustache ; make compile)

${HOME}/src/bbmustache:
	(cd ${HOME}/src ; git clone https://github.com/soranoba/bbmustache.git)
	(cd ${HOME}/src/bbmustache ; git checkout v1.4.0)
#------------------------ END MUSTACHE DEPENDENCY


lib: lib/elli/ebin lib/elli/include lib/bbmustache/ebin lib/edown/ebin

ebin/%.beam: src/%.erl lib
	ERL_LIBS=lib erlc -o ebin/ $<

ebin/erlsrv.app: src/erlsrv.app.src
	cp $? $@

.PHONY: clean
clean:
	rm -f ebin/*
	rm -rf lib
	rm -f utf8.txt
	rm -f *.dump

