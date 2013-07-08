BIN=		dbcorrelate
MAN1= 		dbcorrelate.1

CLEAN=

cflags=		-g
#fflags= 	-g
ldflags=
#FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib

#ldlibs  = $(TRLIBS) $(F77LIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
ldlibs  = $(TRLIBS) $(F77LIBS) -lds -lscv2 -ldbl2 
#ldlibs  = $(TRLIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
#ldlibs  = $(DBLIBS) $(TRLIBS) -lgrx -lds -lscv2 -ldbl2 -lgen  -L /usr/tools/f/LIB -R /usr/tools/f/LIB -ltoolsf -lX11 -L /usr/tools/vaxlib -R /usr/tools/vaxlib -lvaxlib


include $(AEICMAKE)
DIRS=			

OBJS=	 \
dbcorrelate.o get_tau_c.o stats_c.o nint.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
