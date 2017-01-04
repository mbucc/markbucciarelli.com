#! /bin/sh -e

SRC=${HOME}/src

ELLI=${SRC}/elli
ELLI_APP=${ELLI}

JSX=${SRC}/jsx
JSX_APP=${JSX}/_build/default/lib/jsx

ERL_LIBS=${ELLI_APP}:${JSX_APP}:. erl -noshell -s json
