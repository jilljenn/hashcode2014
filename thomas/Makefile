COMP= ocamlbuild -classic-display
FLAGS=
DIRS=
MAIN=main.native
BIN=hcode
GENERATED=$(MAIN) $(BIN)

all:$(MAIN)
	./$(BIN) < data

iter:$(MAIN)
	./$(BIN) iter < data

clock:$(MAIN)
	./$(BIN) clock < data

$(MAIN):
	$(COMP) $(FLAGS) $(DIRS) $(MAIN)
	cp $(MAIN) $(BIN)
	rm $(MAIN)

clean:
	$(COMP) -clean
	rm -f $(GENERATED)
