BIN = netmag2eventtable 

ldlibs=$(DBLIBS)
include $(AEICMAKE)
DIRS=

OBJS= \
	netmag2eventtable.o
netmag2eventtable : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
