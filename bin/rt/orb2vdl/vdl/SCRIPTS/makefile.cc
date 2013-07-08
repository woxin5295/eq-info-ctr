# Sample makefile for nrts_tap based client

CC     = cc 
INCDIR = datasock/qlib
#INCDIR =  ../vdl
BINDIR = ../bin/$(PLATFORM)
QLIB = datasock/qlib/libqlib.a
LIBDIR = ../vdl
OPTMIZ = -O
INCS   = -I$(INCDIR)
OSNAME = SUNOS
CFLAGS = $(OPTMIZ) $(INCS) -L$(LIBDIR) -D$(OSNAME)  
ALL    = vdltap vdl 

all: $(ALL)


remove: FORCE
	rm $(ALL)

clean: FORCE

FORCE:
test: test.o
	$(CC) $(CFLAGS) test.c -o test -lm
test.o: test.c
	$(CC) $(CFLAGS) test.c -c
#
#	VDL Berkeley - Data Sockets
#
vdlucb:  vdl.o vdltrg.o vdlput.o vdlqsub.o ifft512.o ifft256.o fftlib.o cmprs.o vdltcp.o filt2.o filt10.o feedme.o feedme_test.o $(QLIB)
	 makedate
	$(CC) $(CFLAGS)  datetime.c -c
	$(CC) $(CFLAGS)  vdl.o vdltrg.o vdlput.o feedme.o feedme_test.o vdlqsub.o ifft256.o ifft512.o fftlib.o cmprs.o vdltcp.o datetime.o filt2.o filt10.o $(QLIB) -o vdlucb -lm
feedme.o: feedme.c vdl.h 
	$(CC) $(CFLAGS)  feedme.c -c
feedme_test.o: feedme_test.c vdl.h
	$(CC) $(CFLAGS) feedme_test.c -c
#
#   VDLRING - VDL from Alpha type disk files
#
vdlring: vdl.o vdltrg.o vdlput.o vdlqsub.o ifft512.o ifft256.o fftlib.o cmprs.o vdltcp.o filt2.o filt10.o dsklpsub.o vdlring.o
	makedate
	$(CC) $(CFLAGS)  datetime.c -c 
	$(CC) $(CFLAGS)  -g vdl.o vdlring.o vdltrg.o vdlput.o vdlqsub.o dsklpsub.o ifft256.o ifft512.o fftlib.o cmprs.o vdltcp.o datetime.o filt2.o filt10.o -o vdlring -lm
vdlring.o: ../alpha/vdlring.c ../alpha/dsklp.h
	$(CC) $(CFLAGS)  ../alpha/vdlring.c -c 
dsklpsub.o: ../alpha/dsklpsub.c ../alpha/dsklp.h
	$(CC) $(CFLAGS)  ../alpha/dsklpsub.c -c 
#
#   VDL
#
vdl: vdl.o vdltrg.o vdlput.o vdlqsub.o ifft512.o ifft256.o fftlib.o cmprs.o vdltcp.o filt2.o filt10.o vdltap.o 
	 makedate
	$(CC) $(CFLAGS)  datetime.c -c
	$(CC) $(CFLAGS)  vdl.o vdltrg.o vdlput.o vdltap.o vdlqsub.o ifft256.o ifft512.o fftlib.o cmprs.o vdltcp.o datetime.o filt2.o filt10.o -o vdl -lnrts -liris -lutil -lm
vdl.o: vdl.c vdl.h vdlgbl.h cmprs.h ifft.h vdlqsub.h
	$(CC) $(CFLAGS) vdl.c -c
vdltrg.o: vdltrg.c vdl.h vdlgbl.h cmprs.h ifft.h vdlqsub.h
	$(CC) $(CFLAGS)  vdltrg.c -c
vdlput.o: vdlput.c vdl.h vdlgbl.h cmprs.h ifft.h vdlqsub.h
	$(CC) $(CFLAGS)  vdlput.c -c
vdlqsub.o: vdlqsub.c vdl.h vdlqsub.h vdlgbl.h
	$(CC) $(CFLAGS)  vdlqsub.c -c
vdltcp.o: vdltcp.c vdlgbl.h vdl.h
	$(CC) $(CFLAGS)  vdltcp.c -c
vdltap.o: vdltap.c $(INCDIR)/nrts.h
	$(CC) $(CFLAGS)  vdltap.c -c
vdltap2.o: vdltap2.c $(INCDIR)/nrts.h
	$(CC) $(CFLAGS)  vdltap2.c -c
ifft256.o: ifft256.c ifft.h
	$(CC) $(CFLAGS)  ifft256.c -c
ifft512.o: ifft512.c ifft.h
	$(CC) $(CFLAGS)  ifft512.c -c
fftlib.o: fftlib.c ifft.h
	$(CC) $(CFLAGS)  fftlib.c -c
trigtst: trigtst.o satrig.o vdltap.o ifft512.o vdlqsub.o fftlib.o ifft256.o
	$(CC) $(CFLAGS)  trigtst.o satrig.o vdlqsub.o vdltap.o ifft256.o ifft512.o fftlib.o -o trigtst -lnrts -liris -lutil -lm
trigtst.o: trigtst.c satrig.h satriggbl.h
	$(CC) $(CFLAGS) trigtst.c -c
satrig.o: satrig.c satrig.h satriggbl.h
	$(CC) $(CFLAGS) satrig.c -c
vdlmom: vdlmom.o
	$(CC) $(CFLAGS) vdlmom.o -o vdlmom -lm
vdlmom.o: vdlmom.c
	$(CC) $(CFLAGS) vdlmom.c -c

