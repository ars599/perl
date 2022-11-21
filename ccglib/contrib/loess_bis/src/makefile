#CFLAGS = -cckr Wl,r -32q -O
#CFLAGS = -cckr -woff all -O
CFLAGS = -O

#FFLAGS = -32 -O
FFLAGS = -O 
LLIB = -Lsupport

#FLIB = -lF77
#FLIB = -lcl
#FLIB = -lfor
LINCL = -Isrc
OBJ = loessc.o loess.o predict.o misc.o loessf.o supp.o

gas: gas.x
	gas.x
gas.x: gas.o $(OBJ)
	cc -o gas.x gas.o $(OBJ) $(LLIB) -lsupport $(FLIB) -lm

madeup: madeup.x
	madeup.x
madeup.x: madeup.o $(OBJ)
	cc -o madeup.x madeup.o $(OBJ) $(LLIB) -lsupport $(FLIB) -lm

ethanol: ethanol.x
	ethanol.x
ethanol.x: ethanol.o $(OBJ)
	cc -o ethanol.x ethanol.o $(OBJ) $(LLIB) -lsupport $(FLIB) -lm

air: air.x
	air.x
air.x: air.o $(OBJ)
	cc -o air.x air.o $(OBJ) $(LLIB) -lsupport $(FLIB) -lm

galaxy: galaxy.x
	galaxy.x
galaxy.x: galaxy.o $(OBJ)
	cc -o galaxy.x galaxy.o $(OBJ) $(LLIB) -lsupport $(FLIB) -lm

clean:
	rm -f *.o *.x core


