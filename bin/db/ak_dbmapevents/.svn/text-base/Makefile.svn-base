
BIN=		ak_dbmapevents
MAN1=		ak_dbmapevents.1

DATADIR=	maps
DATA=		

cflags=		-g
fflags= 	-g
#ldlibs=         -lsunmath -lgrx -lmap -lev -lscv2 -ldbl2 -lX11 $(TRLIBS) 
ldlibs=		-lgrx -lmap -lev -lscv2 -ldbl2 -L/usr/X11/lib -lX11 $(TRLIBS) 

include $(AEICMAKE)

DIRS=		

OBJS=	$(BIN).o edprojmap.o lola.o get_params_.o get_data_path_.o \
	get_profile_.o grx_dep.o ilen.o 
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
