      subroutine nfilok(defact, hite, ibs, ifil, ityp, nfil, npts)
c compute nfil and defact
      real    defact          ! fractional reduction in size prior to each fill sym
      logical frstim          ! true only the first time in this sub.
      data    frstim/.true./
      real    hite            ! height of symbol
      integer ibs             ! yz portion of symbol - npts*10 + ityp
      integer ifil            ! fill code - (0=no fill, 9=all filled)
      integer ityp            ! type of symbol (0=reg poly, 1=star, 2=* pattern)
      integer mxnfil(99)      ! number of filling symbols required for hite = 1.
      data mxnfil/99*10/
      integer nfil            ! number of additional symbols needed for fill
      integer nfilht          ! number of filling lines, adjusted for hite
      integer npts            ! number of points on symbol
c
c	write(95,*) '---> called nfilok with:'
c	write(95,*) 'defact, hite, ibs, ifil, ityp, nfil, npts ='
c	write(95,*) defact, hite, ibs, ifil, ityp, nfil, npts
c
      if(frstim) then
        mxnfil(30) = 20
        mxnfil(40) = 24
        mxnfil(50) = 25
        mxnfil(60) = 26
        mxnfil(70) = 27
        mxnfil(80) = 28
        mxnfil(90) = 28
        mxnfil(31) = 12
        mxnfil(41) = 11
        mxnfil(51) = 10
        mxnfil(61) = 9
        mxnfil(71) = 8
        mxnfil(81) = 7
        mxnfil(91) = 7
      endif
      if((ityp .eq. 2) .or. (npts .lt. 3)) then
        nfil = 0
c      	write(95,*) '---> returned from nfilok 1'
        return
      endif
      nfilht = mxnfil(ibs)*hite + .5
      nfil = nfilht*(ifil)/9. + 0.5
      defact = 1./(nfilht + 1)
c      write(91,*) 'ibs, hite, nfil'
c      write(91,*)  ibs, hite, nfil
c      write(91,*) 'defact ', defact
c      	write(95,*) '---> returned from nfilok 2'
      return
      end
