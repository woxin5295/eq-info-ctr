BIN=		dbstats

MAN1=		dbstats.1

CLEAN= 		

cflags=		-g
fflags= 	-g
#ldflags= 	-L/Developer/SDKs/MacOSX10.7.sdk/usr/X11/lib
ldflags= 	-L/usr/X11/lib

#ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lgrx -lds -lscv2 -ldbl2 -lgen -lX11
ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lgrx -lds -lscv2 -ldbl2 -lX11
#ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS)
#ldlibs  = $(DBLIBS) $(TRLIBS) -lgrx -lds -lscv2 -ldbl2 -lgen -lX11

include $(AEICMAKE) 

OBJS=	 \
dbstats.o \
sort_on_first_vector.o \
symbol.o \
niceinq.o \
ellipse.o


$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
