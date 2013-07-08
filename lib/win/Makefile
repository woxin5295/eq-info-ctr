LIB= libwin.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

INCLUDE=win.h 
MAN3=libwin.3

cflags=	

include $(AEICMAKE)
		
OBJS= \
	japan_timecode_to_epoch.o \
	bcd2dec.o \
	strip_channel.o \
	strip_time.o \
	hex_to_ascii.o \
	station_info.o \
	winform.o

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
