BIN= beampat
DATADIR=	
DATA=		
INCLUDE=	

MAN1= beampat.1
MAN3=		
MAN5=		

CLEAN= 		

cflags=		
fflags= 	
ldflags= 	
ldlibs=	-ldbl2 $(TRLIBS) -lgrx $(X11LIBS) 	

include $(AEICMAKE)  
#CCOMMON=-D_REENTRANT	
#FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib
# The above two lines were added for antelope 4.10 compilation
DIRS=			
			
OBJS= beampat.o axis3.o ellipse.o niceinq.o wgrid_.o ilen.o ideblank.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
