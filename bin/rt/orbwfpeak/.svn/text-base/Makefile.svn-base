# Copyright (c) 2003 Boulder Real Time Technologies, Inc.
# All rights reserved 

# This software module is wholly owned by Boulder Real Time 
# Technologies, Inc. This software makefile may be copied
# and used in any way.

BIN = orbwfpeak

MAN1 = orbwfpeak.1

PF = orbwfpeak.pf

ldlibs = -lbrttutil -lbrttpkt $(ORBLIBS)

include $(ANTELOPEMAKE)
DIRS=

OBJS= \
	orbwfpeak.o

$(BIN) : orbwfpeak_version.h $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

.time :
	-$(RM) $@
	-touch $@

orbwfpeak_version.h : .time orbwfpeak_version.h.id force
	$(RM) $@
	produce $@
	touch -r .time orbwfpeak_version.h.id

force:
