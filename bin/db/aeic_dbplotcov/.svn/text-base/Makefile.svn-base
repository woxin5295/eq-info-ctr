
BIN=		aeic_dbplotcov
MAN1=		aeic_dbplotcov.1

ldlibs=		-lscv2 -ldbl2 -lgrx $(TRLIBS) $(X11LIBS) $(DBLIBS)

include $(AEICMAKE)  
#CCOMMON=-D_REENTRANT
#FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/local/include -I$(ANTELOPE)/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib
# The above two lines were added for antelope 4.10 compilation
	
DIRS=

OBJS=	aeic_dbplotcov.o readcov.o ilen.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)



# $Id: Makefile,v 1.4 2010-08-27 23:19:55 mitch Exp $ 
