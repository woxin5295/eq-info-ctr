# Copyright (c) 2003 Boulder Real Time Technologies, Inc.
# All rights reserved 

# This software module is wholly owned by Boulder Real Time 
# Technologies, Inc. This software makefile may be copied
# and used in any way.

BIN = orbtm

MAN1 = orbtm.1

PF = orbtm.pf

ldlibs = -lbrttutil -lbrttpkt $(ORBLIBS)

include $(ANTELOPEMAKE)
DIRS=

OBJS= \
	orbtm.o

$(BIN) : orbtm_version.h $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

.time :
	-$(RM) $@
	-touch $@

orbtm_version.h : .time orbtm_version.h.id force
	$(RM) $@
	produce $@
	touch -r .time orbtm_version.h.id

force:
