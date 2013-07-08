#
#	Makefile for dberrchk
#
BIN=		aeic_dbaddema

MAN1=		aeic_dbaddema.1

CLEAN= 		# Extra files which should be removed during a "make clean"

cflags=		-g
fflags= 	-g
ldflags= 	# flags to linker/loader

#ldlibs=		-Bstatic -lscv2 -ldbl2 -ldb -ltttaup -lresponse -lcoords -lstock -lmath -lF77 -lm
#ldlibs  = $(TRLIBS) $(F77LIBS) $(DBLIBS) -lsunmath -lds -lscv2 -ldbl2 -lgen
#ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) 

#ldlibs  = $(DBLIBS) $(TRLIBS) -lgrx -lds -lscv2 -ldbl2 -lgen  -L /usr/tools/f/LIB -R /usr/tools/f/LIB -ltoolsf -lX11 -L /usr/tools/vaxlib -R /usr/tools/vaxlib -lvaxlib
#ldlibs  = $(DBLIBS) $(TRLIBS) -lgrx -lds -lscv2 -ldbl2 -lgen -lX11 
ldlibs  = $(DBLIBS) $(TRLIBS) -lgrx -lds -lscv2 -ldbl2 -L/usr/X11/lib -lX11 

include $(AEICMAKE)  	# This line must appear following the definitions above, 
CCOMMON=-D_REENTRANT
#FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib
# The above two lines were added for antelope 4.10 compilation

			# and before the DIRS definition below..
DIRS=			# Omit this line to run make in subdirectories, or
			# fill in to run make only in certain subdirectories

OBJS=	 \
aeic_dbaddema.o


$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
