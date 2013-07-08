BIN=win2orb
DATADIR=
DATA=
INCLUDE=
PF=win2orb.pf

MAN1=win2orb.1

CLEAN=

cflags=
fflags=
ldflags= 
ldlibs= -lwin $(ORBLIBS)

include $(AEICMAKE)

DIRS=


OBJS= \
	win2orb.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
