BIN= monitordlinfotable 

#MAN1= monitordlinfotable.1

ldlibs=	 $(TRLIBS)  -lorb -lxtra -lPkt $(SVR4LIBS)
include $(AEICMAKE)  	
			
DIRS=			
			
OBJS= monitordlinfotable.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o monitordlinfotable $(OBJS)  $(LDFLAGS) $(LDLIBS)
