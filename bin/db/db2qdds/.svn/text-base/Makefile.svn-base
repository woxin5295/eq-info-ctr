BIN= db2qdds 

MAN1= db2qdds.1

#ldlibs=	 $(TRLIBS) -lpkt -lorb -lxtra -lPkt $(SVR4LIBS)
ldlibs=	 $(TRLIBS)  -lorb -lxtra -lPkt $(SVR4LIBS)
include $(AEICMAKE)  	
			
DIRS=			
			
#OBJS= $(BIN).o dborigin_to_qdds.o
OBJS= db2qdds.o dborigin_to_qdds.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
