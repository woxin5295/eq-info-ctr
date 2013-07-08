LIB= libplotpslib.a
#DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE=

MAN3=

CLEAN=

cflags=
#fflags=
fflags= -std=legacy
ldflags=
ldlibs=

# install libvaxlib directly in $ANTELOPE
include $(ANTELOPEMAKE)

DIRS=

OBJS    = plotd.o pslib.o symbl.o

$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

#$(DLIB) : $(OBJS)
#	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)


#f90 -f77 -m64 -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)

#FFLAGS = -O

