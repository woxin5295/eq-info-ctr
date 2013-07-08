BIN = db2evt
PF=db2evt.pf
MAN1=db2evt.1
INCLUDE=k2evt.h
ldlibs=	$(TKLIBS) $(TCLLIBS) -ltkrtd -lbrttpkt -ltkgpl $(GPLLIBS) -ltttaup -lscv2 -ldbl2 $(ORBLIBS)

include $(AEICMAKE)
DIRS=

OBJS= \
	db2evt.o
db2evt : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
