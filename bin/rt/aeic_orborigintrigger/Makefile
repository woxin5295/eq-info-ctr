
BIN= aeic_orborigintrigger

CLEAN= 		

cflags=
fflags= 	
ldflags= 
ldlibs= -lPkt -lorb -lxtra $(TRLIBS) $(SVR4LIBS) -lm

include $(AEICMAKE)  	
			
DIRS=			
			


OBJS=	$(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

