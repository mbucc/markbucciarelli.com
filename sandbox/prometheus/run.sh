#! /bin/sh -e

SRC=${HOME}/src

#ELLI=${SRC}/elli
#ELLI_APP=${ELLI}/_build/default/lib/elli
ELLI=${SRC}/elli
ELLI_APP=${ELLI}

PROMETHEUS=${SRC}/prometheus.erl
PROMETHEUS_APP=${PROMETHEUS}/_build/default/lib/prometheus

ERL_LIBS=${ELLI_APP}:${PROMETHEUS_APP}:. erl
