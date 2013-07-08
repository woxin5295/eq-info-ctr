BIN= dbtmwfextractor 

MAN1= dbtmwfextractor.1

#ldlibs=	 $(TRLIBS) -lpkt -lorb -lxtra -lPkt $(SVR4LIBS)
ldlibs=	 $(TRLIBS)  -lorb -lxtra -lPkt $(SVR4LIBS)
include $(AEICMAKE)  	
			
DIRS=			
			
OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
