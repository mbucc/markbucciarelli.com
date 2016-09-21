all: server.pdf server1.beam ns.beam

server.pdf: server.latex msc.sty
	pdflatex server.latex
	#@open $@

%.beam: %.erl
	erlc $?

.PHONY: clean
clean:
	rm -f *.aux *.log
	rm -f *.pdf
	rm -f *.beam
