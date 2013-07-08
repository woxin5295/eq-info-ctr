LIB= libtoolsc.a
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

#OBJS= getstr.o fgetstr.o fdgetstr.o fdputstr.o echo_err.o julgre.o time_conv.o suds_time.o
OBJS= getstr.o fgetstr.o fdgetstr.o fdputstr.o echo_err.o time_conv.o suds_time.o

$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)

#FFLAGS = -O

##########

FFLAGS = -O

