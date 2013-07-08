
BIN = orb_alarm
PF = orb_alarm.pf

#DATADIR = perl
#DATA = mp_oqalarm_ack.pl

#MAN1 = orb_quake_alarm.1
#MAN3P = mp_oqalarm_ack.3p

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lcgeom -lpolygon $(DBLIBS) $(ORBLIBS) $(THREAD)

include $(ANTELOPEMAKE)
DIRS=	

OBJS= orb_alarm.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
