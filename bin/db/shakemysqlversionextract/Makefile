BIN= shakemysqlversionextract create_pipe_event 

#MAN1= shakemysqlversionextract.1 create_pipe_event.1

#ldlibs=	 $(TRLIBS) -lpkt -lorb -lxtra -lPkt $(SVR4LIBS)
ldlibs=	 $(TRLIBS)  -lorb -lxtra -lPkt $(SVR4LIBS)
include $(AEICMAKE)  	
			
DIRS=			
			
OBJS= shakemysqlversionextract.o create_pipe_event.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o shakemysqlversionextract  shakemysqlversionextract.o $(LDFLAGS) $(LDLIBS)
	$(CC) $(CFLAGS) -o create_pipe_event create_pipe_event.o $(LDFLAGS) $(LDLIBS)
