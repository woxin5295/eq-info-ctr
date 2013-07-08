BIN=win2db
DATADIR=
DATA=
INCLUDE=
PF=win2db.pf

MAN1=win2db.1

CLEAN=

cflags= 
fflags=
ldflags= 
ldlibs= -lwin $(TRLIBS) 

include $(AEICMAKE)

DIRS=

OBJS= \
	win2db.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
