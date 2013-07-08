BIN=orb2win
DATADIR=
DATA=
INCLUDE=
PF=orb2win.pf

MAN1=orb2win.1

CLEAN=

#cflags= -I/usr/local/aeic/4.2u/include
cflags= -I/usr/local/aeic/4.6/include
fflags=
#ldflags= -L/usr/local/aeic/4.2u/lib -R/usr/local/aeic/4.2u/lib
ldflags= -L/usr/local/aeic/4.6/lib -R/usr/local/aeic/4.6/lib
ldlibs= -lwin $(ORBLIBS) 

include $(AEICMAKE)

DIRS=

OBJS= \
	orb2win.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
