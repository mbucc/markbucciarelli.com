all: json_handler.beam json_sup.beam json.beam

#==============================================================================
#
#                           D E P E N D E N C I E S
#
#  Check out Elli and JSX from github to $HOME/src, compile each and copy the
#  ebin directories and the elli include directory under ./lib.
#==============================================================================

${HOME}/src/elli/ebin:
	(cd ${HOME}/src; git clone https://github.com/knutin/elli.git)
	(cd ${HOME}/src/elli ; git checkout tags/v1.0.5 ; make)

./lib/elli-1.0.5/ebin: ${HOME}/src/elli/ebin
	mkdir -p ./lib/elli-1.0.5
	cp -r $< ./lib/elli-1.0.5

./lib/elli-1.0.5/include: ${HOME}/src/elli
	mkdir -p ./lib/elli-1.0.5
	cp -r ${HOME}/src/elli/include ./lib/elli-1.0.5

${HOME}/src/jsx/_build/default/lib/jsx/ebin:
	(cd ${HOME}/src; git clone https://github.com/talentdeficit/jsx.git)
	(cd ${HOME}/src/jsx ; git checkout tags/v2.8.1 ; rebar3 compile)

./lib/jsx-2.8.1/ebin: ${HOME}/src/jsx/_build/default/lib/jsx/ebin
	mkdir -p ./lib/jsx-2.8.1
	cp -r $< ./lib/jsx-2.8.1


#==============================================================================
#
#                      C O M P I L E   A N D   C L E A N 
#
#==============================================================================

%.beam: %.erl ./lib/elli-1.0.5/ebin ./lib/jsx-2.8.1/ebin ./lib/elli-1.0.5/include
	ERL_LIBS=./lib erlc $<

.PHONY: clean
clean:
	rm -f *.beam
	rm -rf ./lib
