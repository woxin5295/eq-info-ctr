BIN=		aeic_db2weekly

MAN1=		aeic_db2weekly.1

CLEAN=

cflags=		-g
ldflags=

#ldlibs  = $(TRLIBS) $(F77LIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
#ldlibs  = $(TRLIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
ldlibs  = $(TRLIBS) -lds -lscv2 -ldbl2 


include $(AEICMAKE)
DIRS=			

OBJS=	 \
aeic_db2weekly.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
