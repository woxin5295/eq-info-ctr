#CFLAGS = -D_REENTRANT -D_EARTHWORM -DDEBUG_PRINT $(GLOBALFLAGS)
CFLAGS = -D_REENTRANT -D_EARTHWORM $(GLOBALFLAGS)

B = ../../bin
L = ../../lib

BINARIES =   vdl.o vdltrg.o vdlput.o vdlqsub.o ifft512.o ifft256.o ifft1024.o\
		 fftlib.o\
		 cmprs.o vdltcp.o filt2.o filt10.o feed_vdl_ew.o feedme_test.o\
	 	 $L/getutil.o  $L/kom.o $L/logit.o $L/transport.o  $L/sleep_ew.o\
		 $L/socket_ew.o $L/time_ew.o $L/threads_ew.o $L/sema_ew.o\
		 $L/queue_max_size.o $L/swap.o

vdl: $(BINARIES)
	cc -o $B/vdl $(BINARIES) -lnsl -lsocket -mt -lposix4 -lthread -lc -lm
