BIN=		aeic_dberrchk

MAN1=		aeic_dberrchk.1

CLEAN= 		

cflags=		-g
ldflags= 	

#ldlibs  = $(TRLIBS) $(F77LIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
ldlibs  = $(TRLIBS) -lds -lscv2 -ldbl2


include $(AEICMAKE)  	

OBJS=	 \
aeic_dberrchk.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
