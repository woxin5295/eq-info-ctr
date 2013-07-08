BIN=aeic_region_name

MAN1=aeic_region_name.1

cflags=         -g
fflags=         -g
ldflags=        # flags to linker/loader

#ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lgrx -lds -lscv2 -ldbl2 -lgen  -ltoolsf -lX11 -lvaxlib
ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lgrx -lds -lscv2 -ldbl2 -ltoolsf -L/usr/X11/lib -lX11 -lvaxlib

include $(AEICMAKE)

#FFLAGS= -m64 $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib

DIRS=

OBJS=	aeic_region_name.o\
	aeic_reg.o

aeic_region_name: $(OBJS) 
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) 
