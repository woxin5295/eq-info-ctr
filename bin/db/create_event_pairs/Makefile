BIN=		create_event_pairs
MAN1=		create_event_pairs.1

CLEAN=

cflags=		-g
fflags= 	-g
ldflags=

#ldlibs  = $(TRLIBS) $(F77LIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
#ldlibs  = $(TRLIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
ldlibs  = $(TRLIBS) -lds -lscv2 -ldbl2 


include $(AEICMAKE)
DIRS=			

OBJS=	 \
event_pairs.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

