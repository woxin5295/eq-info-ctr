BIN= dbicepick gui_dbicepick
DATADIR=
DATA=
INCLUDE=
 
MAN1=dbicepick.1
  
   
cflags= -mt 
fflags=
ldflags= 
ldlibs= -lwormsol -lwormutil -lwormds $(TRLIBS) $(SVR4LIBS)

include $(AEICMAKE)

OBJS = dbicepick.o rexallen.o rainit.o raldpar.o

dbicepick: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
