all: oop.pdf t.db

t.db: oop_schema.sql
	sqlite3 t.db < oop_schema.sql

oop.pdf: oop.tex
	pdflatex oop.tex

clean:
	rm -f *.log *.aux
