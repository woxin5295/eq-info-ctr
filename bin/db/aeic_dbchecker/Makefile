BIN=		aeic_dbchecker

MAN1=		aeic_dbchecker.1

CLEAN= 		

cflags=		-g
ldflags= 	

#ldlibs  = $(TRLIBS) $(F77LIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
#ldlibs  = $(TRLIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
ldlibs  = $(TRLIBS) -lds -lscv2 -ldbl2 

include $(AEICMAKE)  	 
			
OBJS=	 \
aeic_dbchecker.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
