# Sample makefile for nrts_tap based client

CC     = gcc 
INCDIR = datasock/qlib
INCDIR2 =  ../STEIM123
BINDIR = ../bin/$(PLATFORM)
QLIB = datasock/qlib/libqlib.a
LIBDIR = ../vdl
OPTMIZ = -O
INCS   = -I$(INCDIR) -I$(INCDIR2)
OSNAME = SOLARIS
OSNAME2 = SOLARIS2
#CFLAGS = $(OPTMIZ) $(INCS) -L$(LIBDIR) -D$(OSNAME) -DDEBUG_PRINT  -ansi
CFLAGS = $(OPTMIZ) $(INCS) -L$(LIBDIR) -D$(OSNAME) -D$(OSNAME2) -ansi
ALL    = vdlasl

all: $(ALL)


remove: FORCE
	rm $(ALL)

clean: FORCE

include SCRIPTS/makefile.modules

#
#	VDLASL - VDL from the LISS server at ASL
# 
vdlasl:	 vdl.o vdltrg.o vdlput.o vdlqsub.o ifft512.o ifft256.o  ifft1024.o fftlib.o cmprs.o vdltcp_nopass.o filt2.o filt10.o filt5.o filtheli.o feedme_seed.o safetcp.o vdlfilt.o feedme_test_null.o  vdlmath.o
	 makedate
	$(CC) $(CFLAGS)  datetime.c -c
	$(CC) $(CFLAGS)  vdl.o vdltrg.o vdlput.o feedme_seed.o safetcp.o ../STEIM123/steimlib.o feedme_test_null.o vdlqsub.o ifft256.o ifft512.o ifft1024.o fftlib.o cmprs.o vdltcp_nopass.o datetime.o filt2.o filt5.o filt10.o filtheli.o vdlfilt.o vdlmath.o -o vdlasl -lm -lnsl -lsocket
