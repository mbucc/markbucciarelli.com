all: json_handler.beam json_sup.beam json.beam

#------------------------------------------------------------------------------
#
#                           D E P E N D E N C I E S
#
#  Check out Elli and JSX from github to $HOME/src and compile each.
#------------------------------------------------------------------------------

SRC=${HOME}/src

ELLI_GIT=https://github.com/knutin/elli.git
ELLI_TAG=v1.0.5
ELLI=${SRC}/elli
ELLI_APP=${ELLI}
${ELLI}:
	(cd ${SRC}; git clone ${ELLI_GIT})
	(cd ${ELLI}; git checkout tags/${ELLI_TAG})
${ELLI_APP}/ebin: ${ELLI}
	(cd ${ELLI} ; make)

JSX_GIT=https://github.com/talentdeficit/jsx.git
JSX_TAG=v2.8.1
JSX=${SRC}/jsx
JSX_APP=${JSX}/_build/default/lib/jsx
${JSX}:
	(cd ${SRC}; git clone ${JSX_GIT})
	(cd ${JSX}; git checkout tags/${JSX_TAG})
${JSX_APP}/ebin: ${JSX}
	(cd ${JSX} ; rebar3 compile)

DEPS = ${ELLI_APP}/ebin ${JSX_APP}/ebin

LIBS="${ELLI_APP}:${JSX_APP}"


#------------------------------------------------------------------------------
#
#                      C O M P I L E   A N D   C L E A N 
#
#------------------------------------------------------------------------------

%.beam: %.erl $(DEPS)
	ERL_LIBS=${LIBS} erlc $<

.PHONY: clean
clean:
	rm -f *.beam
