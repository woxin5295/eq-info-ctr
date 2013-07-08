LIB= libvaxlib.a
#DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE=

MAN3=

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

# install libvaxlib directly in $ANTELOPE
include $(ANTELOPEMAKE)

DIRS=

OBJS    = decod.o\
        aask.o\
        auxpln.o\
        bell.o\
        cart2pole.o\
        ccat.o\
        centers.o\
        cortim.o\
        cromult.o\
        crossprod.o\
        date_s1960.o\
        dble.o\
        dfjl.o\
        dnstrng.o\
        dubl.o\
        dubl1.o\
        dubl2.o\
        dubltest.o\
        formall.o\
        formf.o\
        formit.o\
        forms.o\
        four1.o\
        ftype.o\
        gc2gg.o\
        geosub.o\
        getbias.o\
        gg2gc.o\
        gmt_ast.o\
        helpit.o\
        iaskk.o\
        ibegtru.o\
        ieee_bombout.o\
        ipto.o\
        iround.o\
        jdate.o\
        jldy.o\
        jul1800.o\
        juldy.o\
        lask.o\
        lentru.o\
        libput_screen.o\
        libspawn.o\
        locstr.o\
        lshft.o\
        mbox.o\
        median.o\
        mstime.o\
        newform.o\
        nmarg.o\
        numarg.o\
        openit.o\
        phaget.o\
        phaget2.o\
        plclip.o\
        pltclp.o\
        pole2cart.o\
        polvec.o\
        ran3.o\
        raskk.o\
        rnd.o\
        rshft.o\
        s1960_date.o\
        shofon.o\
        sort.o\
        stpcom.o\
        string.o\
        symb.o\
        symcen.o\
        symsta.o\
        tandpnew.o\
        tenxvec.o\
        tiltdaz.o\
        time.o\
        tinside.o\
        torf.o\
        trim.o\
        txtcen.o\
        upstrng.o\
        vecpol.o\
        vgp3help.o\
        xmeanrms.o\
        xmedian.o

$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

#$(DLIB) : $(OBJS)
#	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)


#f90 -f77 -m64 -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)

#FFLAGS = -O

