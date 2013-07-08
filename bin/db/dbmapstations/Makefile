BIN= dbmapstations

MAN1= dbmapstations.1

CLEAN=

cflags=	
fflags=
ldflags=
ldlibs= $(DBLIBS)

include $(AEICMAKE)
		
DIRS=	

DBG= -g

OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
