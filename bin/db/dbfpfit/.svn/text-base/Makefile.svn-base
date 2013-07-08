BIN=		dbfpfit
MAN1=dbfpfit.1
PF=dbfpfit.pf

cflags=		-g
fflags= 	-g
ldflags= 	# flags to linker/loader

#ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lgrx -lds -lscv2 -ldbl2 -lgen  -ltoolsf -lX11 -lvaxlib
ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lgrx -lds -lscv2 -ldbl2 -L/opt/antelope/5.2-64/lib/ -ltoolsf -lvaxlib
#ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lgrx -lds -lscv2 -ldbl2 -lgen  -ltoolsf -lX11
#ldlibs  = $(DBLIBS) $(TRLIBS) $(F77LIBS) -lm -lgrx -lds -lscv2 -ldbl2 -lgen  -lX11 
#ldlibs  = $(DBLIBS) $(TRLIBS) -lgrx -lds -lscv2 -ldbl2 -lgen  -L /usr/tools/f/LIB -R /usr/tools/f/LIB -ltoolsf -lX11 -L /usr/tools/vaxlib -R /usr/tools/vaxlib -lvaxlib
include $(AEICMAKE)  	
#CCOMMON=-D_REENTRANT 
#FFLAGS= -m64 $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib
# The above two lines were added for antelope 4.10 compilation
#LIB	= 	-l4014 -lV77

OBJS   = \
dbfpfit.o \
aintodp.o \
compl.o \
csort.o \
ctop.o \
defsym.o \
flippl.o \
fpinp.o \
fpout.o \
fsearch2.o \
get3.o \
getflt.o \
getpol.o \
gettab.o \
gettab2.o \
nfilok.o \
pexcf1.o \
rdiff.o \
rdrsym.o \
readeqdb.o \
refrmt.o \
search2.o \
search3.o \
shrflt1.o \
tandp.o \
tmgen.o 

$(BIN): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) 
