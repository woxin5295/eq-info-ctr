BIN= dbshowfk

MAN1= dbshowfk.1
MAN3=		
MAN5=		

CLEAN= 		

cflags=		
fflags= 	
ldflags= 	
ldlibs= -lscv2 -ldbl2 $(GPLLIBS) $(TRLIBS)

include $(AEICMAKE)  	
			
DIRS=			
			
OBJS=	dbshowfk.o plot_subs.o find4.o find_region.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
