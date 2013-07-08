BIN= netmagselect 

MAN1= netmagselect.1

#ldlibs=	 $(TRLIBS) -lpkt -lorb -lxtra -lPkt $(SVR4LIBS)
ldlibs=	 $(TRLIBS)  -lorb -lxtra -lPkt $(SVR4LIBS)
include $(AEICMAKE)  	
			
DIRS=			
			
OBJS= netmagselect.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o netmagselect netmagselect.o $(LDFLAGS) $(LDLIBS)
