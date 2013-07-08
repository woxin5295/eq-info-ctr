BIN= aeic_dbml 

MAN1=aeic_dbml.1 

PF=aeic_dbml.pf

CLEAN= 		

cflags= -g
ldflags= 	

#fflags= 	
#ldlibs= -lgen $(TRLIBS) -L/opt/SUNWspro/lib -R/opt/SUNWspro/lib -lsunmath 
#ldlibs= $(TRLIBS) -L/opt/SUNWspro/lib -R/opt/SUNWspro/lib
ldlibs= $(TRLIBS) $(F77LIBS) -L/usr/local/lib/x86_64 -lgfortran

include $(AEICMAKE) 

#FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib
# The above two lines were added for antelope 4.10 compilation
			
DIRS=			

OBJS= aeic_dbml.o local_mag_util.o cool.o bworth.o mydbgetwf.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

#	$(F77) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
