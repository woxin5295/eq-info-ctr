BIN=		hypodd2db
MAN1=		hypodd2db.1

CLEAN=

cflags=		-g
fflags= 	-g
ldflags=

#ldlibs  = $(TRLIBS) $(F77LIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
ldlibs  = $(TRLIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen


include $(AEICMAKE)
DIRS=			

OBJS=	 \
hypodd2db.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

