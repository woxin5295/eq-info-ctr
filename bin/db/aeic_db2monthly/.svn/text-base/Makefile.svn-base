BIN=		aeic_db2monthly

MAN1=           aeic_db2monthly.1

CLEAN=

cflags=
ldflags=

#ldlibs  = $(TRLIBS) $(F77LIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
#ldlibs  = $(TRLIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
ldlibs  = $(TRLIBS) -lds -lscv2 -ldbl2


include $(AEICMAKE)
			
DIRS=		

OBJS= aeic_db2monthly.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
