ML       = snake.ml
BIN      = snake
LIB      = unix.cmxa threads.cmxa
OPT      = -thread

all:
	ocamlopt $(OPT) $(LIB) $(ML) -o $(BIN);

exec: all
	./$(BIN)
auth:
	echo "* Fanet Théo\n" >> AUTHORS

clean:
	rm -rf *.cmi *.cmx *~ *.o

cleanall: clean
	rm $(BIN)

dist: clean
	tar -cvjf ../$(DISTNAME) ../noId/

