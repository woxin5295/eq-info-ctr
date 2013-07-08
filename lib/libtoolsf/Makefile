LIB= libtoolsf.a
DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE=

MAN3=

CLEAN=

cflags=
#fflags=
fflags= -std=legacy
ldflags=
ldlibs=

# install libuser directly in $ANTELOPE
include $(ANTELOPEMAKE)

DIRS=

OBJS= dtrig.o getenv.o getformat.o pause.o report.o strlen.o

$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)

#FFLAGS = -O

