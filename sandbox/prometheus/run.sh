#! /bin/sh -ex

SRC=${HOME}/src
ELLI_TAG=v1.0.5
ELLI=${SRC}/elli
ELLI_BIN=${ELLI}
PROMETHEUS_TAG=v3.1.0
PROMETHEUS=${SRC}/prometheus.erl
PROMETHEUS_BIN=${PROMETHEUS}/_build/default/lib/prometheus

ERL_LIBS=${ELLI_BIN}:${PROMETHEUS_BIN}:. erl
