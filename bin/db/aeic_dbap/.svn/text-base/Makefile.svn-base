
#
#	Makefile for dbap
#
BIN=		aeic_dbap
MAN1=		aeic_dbap.1

cflags=		$(TCLINCLUDE)
ldflags= 	-L$(TCLLIB)

ldlibs=         -lscv2 -ldbl2 $(TKLIBS) $(TCLLIBS) $(GPLLIBS) -lmath -ltttaup $(TRLIBS) -L$(TCLLIB) -ltcl -litcl -litk
#ldlibs=         -lscv2 -ldbl2 $(TKLIBS) $(TCLLIBS) -L /opt/antelope/tcltk8.4-64/lib/itcl3.3 -L /opt/antelope/tcltk8.4-64/lib/itk3.3 -R /opt/antelope/tcltk8.4-64/lib/itcl3.3 -R /opt/antelope/tcltk8.4-64/lib/itk3.3 $(GPLLIBS) -lmath -ltttaup $(TRLIBS) -ltcl -litcl3.3 -ltclx -litk3.3 -ltkx

include $(AEICMAKE)
CCOMMON=-D_REENTRANT
FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib
# The above two lines were added for antelope 4.10 compilation
 
DIRS=

OBJS=	 dbap.o beam_subs.o dbassoc.o db_utils.o exec_subs.o filter_subs.o merge_db.o objs.o parse_pf.o \
		plot_subs.o qplot.o sc_subs.o spgrm.o tcl_wrapper.o \
		trace_resamp.o trace_subs.o utils.o mem_tracker.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id: Makefile,v 1.7 2011-01-14 00:55:59 cvsaeic Exp $ 
