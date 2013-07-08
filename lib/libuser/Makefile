LIB= libuser.a
DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE=

MAN3=

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= $(TRLIBS)

# install libuser directly in $ANTELOPE
include $(ANTELOPEMAKE)

DIRS=

OBJS= wfAAH.o
$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
