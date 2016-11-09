all: ebin/es_sup.beam ebin/es_callback.beam ebin/erlsrv.app ebin/erlsrv.beam

#------------------------------------------------------------------------------
#
#                           D E P E N D E N C I E S 
#
#------------------------------------------------------------------------------

SRC=${HOME}/src

ELLI_TAG=v1.0.5
ELLI=${SRC}/elli
ELLI_BIN=${ELLI}

PROMETHEUS_TAG=v3.1.0
PROMETHEUS=${SRC}/prometheus.erl
PROMETHEUS_BIN=${PROMETHEUS}/_build/default/lib/prometheus

${ELLI}:
	(cd ${SRC}; git clone https://github.com/knutin/elli.git)
	(cd ${ELLI}; git checkout tags/${ELLI_TAG})
${ELLI_BIN}/ebin: ${ELLI}
	(cd ${ELLI} ; make)

${PROMETHEUS}:
	(cd ${SRC}; git clone https://github.com/deadtrickster/prometheus.erl.git)
	(cd ${PROMETHEUS}; git checkout tags/${PROMETHEUS_TAG})
${PROMETHEUS_BIN}/ebin: ${PROMETHEUS}
	(cd ${PROMETHEUS} ; rebar3 compile)

https://github.com/deadtrickster/prometheus.erl.git


#------------------------------------------------------------------------------
#
#                                S O U R C E S 
#
#------------------------------------------------------------------------------


ebin/%.beam: src/%.erl ${ELLI_BIN}/ebin ${PROMETHEUS_BIN}/ebin
	ERL_LIBS=${ELLI_BIN}:${PROMETHEUS_BIN} erlc -o ebin/ $<

ebin/erlsrv.app: src/erlsrv.app.src ${ELLI_BIN}/ebin ${PROMETHEUS_BIN}/ebin
	cp $< $@

#------------------------------------------------------------------------------
#
#                                  C L E A N 
#
#------------------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f ebin/*
