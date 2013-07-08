BIN= pipelinedb2xml 

MAN1= pipelinedb2xml.1

ldlibs= $(TRLIBS)  -lorb -lxtra -lPkt $(SVR4LIBS)

include $(AEICMAKE)  
	
DIRS=			
			
OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
