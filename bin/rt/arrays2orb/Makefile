BIN= arrays2orb
MAN1= 

ldlibs=	 $(TRLIBS) -lPkt -lorb $(SVR4LIBS)

include $(AEICMAKE)  	
DIRS=

OBJS =  arrays2orb.o \
	init_blocks.o \
	CTFrame_time_to_epoch.o \
	CTFrame2orbpkts.o \
	strreplace.o \
	g2i.o \
	mystuff_iw_tracebuf.o \
	get_next_block.o \
	get_pf.o \
	malloced_split.o \
	ctframe_crc.o

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS) 
