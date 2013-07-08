BIN=		pick_counts

MAN1=		pick_counts.1

CLEAN= 		

cflags=		-g
fflags= 	-g
ldflags= 	

#ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lgrx -lds -lscv2 -ldbl2 -lgen -lX11
ldlibs  = $(DBLIBS) $(TRLIBS) -lgrx -lds -lscv2 -ldbl2 -lgen -lX11

include $(AEICMAKE) 

OBJS=	 \
pick_counts.o \
sort_on_first_vector.o 


$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
