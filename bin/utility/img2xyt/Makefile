BIN= img2xyt

MAN1= img2xyt.1
MAN3=
MAN5=

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include $(AEICMAKE)
# The above two lines were added for antelope 4.10 compilation

OBJS= $(BIN).o diskio.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
