
BIN= orb2aeic_beeper

PF=orb2aeic_beeper.pf
MAN1=orb2aeic_beeper.1

CLEAN= 		

cflags=	-I/usr/local/include $(TCLINCLUDE)
fflags= 	
ldflags=  -L/usr/local/lib -R/usr/local/lib
ldlibs= -lstock -lPkt -lorb $(TRLIBS) $(SVR4LIBS) $(TCLLIBS) -lexpect -lm

include $(AEICMAKE)  
#CCOMMON=-D_REENTRANT	
#FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib
# The above two lines were added for antelope 4.10 compilation
DIRS=			
			


OBJS=	$(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

