BIN= dmx2orb
PF= dmx2orb.pf 

MAN1= dmx2orb.1


CLEAN=

cflags=
fflags=
ldflags=
ldlibs= $(ORBLIBS)

include $(AEICMAKE)

DIRS=


OBJS= dmx2orb.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
