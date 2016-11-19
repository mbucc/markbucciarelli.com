all: ebin/es_sup.beam ebin/es_callback.beam ebin/erlsrv.app ebin/erlsrv.beam \
		ebin/elli_prometheus.beam

#------------------------------------------------------------------------------
#
#                           D E P E N D E N C I E S 
#
#------------------------------------------------------------------------------

SRC=${HOME}/src

# elli-lib is newer and changed handle_event() API (Timings --> {Timings, Sizes}).
# However, there is a bug with using request_start, so I reverted to older Elli.
# See https://github.com/elli-lib/elli/issues/19
#ELLI_GIT=https://github.com/elli-lib/elli.git
#ELLI_TAG=2.0.0
#ELLI=${SRC}/elli
#ELLI_APP=${ELLI}/_build/default/lib/elli

ELLI_GIT=https://github.com/knutin/elli.git
ELLI_TAG=v1.0.5
ELLI=${SRC}/elli
ELLI_APP=${ELLI}

PROMETHEUS_GIT=https://github.com/deadtrickster/prometheus.erl.git
PROMETHEUS_TAG=v3.1.0
PROMETHEUS=${SRC}/prometheus.erl
PROMETHEUS_APP=${PROMETHEUS}/_build/default/lib/prometheus


${ELLI}:
	(cd ${SRC}; git clone ${ELLI_GIT})
	(cd ${ELLI}; git checkout tags/${ELLI_TAG})
${ELLI_APP}/ebin: ${ELLI}
	(cd ${ELLI} ; make)

${PROMETHEUS}:
	(cd ${SRC}; git clone ${PROMETHEUS_GIT})
	(cd ${PROMETHEUS}; git checkout tags/${PROMETHEUS_TAG})
${PROMETHEUS_APP}/ebin: ${PROMETHEUS}
	(cd ${PROMETHEUS} ; rebar3 compile)

https://github.com/deadtrickster/prometheus.erl.git


#------------------------------------------------------------------------------
#
#                                S O U R C E S 
#
#------------------------------------------------------------------------------


ebin/%.beam: src/%.erl ${ELLI_APP}/ebin ${PROMETHEUS_APP}/ebin
	ERL_LIBS=${ELLI_APP}:${PROMETHEUS_APP} erlc -o ebin/ $<

ebin/erlsrv.app: src/erlsrv.app.src ${ELLI_APP}/ebin ${PROMETHEUS_APP}/ebin
	cp $< $@

#------------------------------------------------------------------------------
#
#                                  C L E A N 
#
#------------------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f ebin/*
	rm -rf data		# prometheus data
