
BIN=		dbmaprelease
MAN1=		

cflags=		-m64 -g
fflags= 	-m64 -g
#ldlibs=		-lsunmath -lgrx -lmap -lev -lscv2 -ldbl2 -lX11 $(TRLIBS)
#ldlibs=		-lsunmath -lgrx -lmap -lev -lscv2 -ldbl2 -lX11 $(TRLIBS) $(F77LIBS)
ldlibs=         -lgrx -lmap -lev -lscv2 -ldbl2 -L/usr/X11/lib -lX11 $(TRLIBS)

include $(AEICMAKE)  
#CCOMMON=-D_REENTRANT	
#FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) 
# The above two lines were added for antelope 4.10 compilation
FPP = f77 -F

DIRS=		

OBJS=	dbmaprelease.o edprojmaprelease.o lola.o get_params_.o get_data_path_.o get_profile_.o grx_dep.o ilen.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


