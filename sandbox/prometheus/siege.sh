#! /bin/sh -e
# Lay siege to Elli.

CLIENTS=5
DURATION=60S
siege --log=siege.log -c $CLIENTS -t $DURATION http://127.0.0.1:3000/hello/world

CLIENTS=50
DURATION=60S
siege --log=siege.log -c $CLIENTS -t $DURATION http://127.0.0.1:3000/hello/world

CLIENTS=5
DURATION=60S
siege --log=siege.log -c $CLIENTS -t $DURATION http://127.0.0.1:3000/hello/world
