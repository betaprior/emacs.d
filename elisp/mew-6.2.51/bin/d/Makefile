TGT = smew
OBJ = smew.o Sqlite3DB.o SearchEngine.o
LIB = -lsqlite3

DC = gdc

.SUFFIXES: .d
.d.o:
	$(DC) -c $<

all: $(TGT)

$(TGT): $(OBJ)
	$(DC) -o $@ $(OBJ) $(LIB)

clean:
	rm -f $(TGT) $(OBJ)
