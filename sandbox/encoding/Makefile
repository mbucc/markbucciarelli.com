all: utf8.txt \
		file_read_file.beam

.PHONY: test1
test1: file_read_file.beam
	erl -pa . -s file_read_file -s init stop

%.beam: %.erl
	erlc $<

utf8.txt: makeutf8.sh
	./makeutf8.sh > $@

.PHONY: clean
clean:
	rm -f utf8.txt
	rm -f *.beam
