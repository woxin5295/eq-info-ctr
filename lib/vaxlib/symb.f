cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c THE FIRST CALL TO SYMB SETS SOME INITIAL VALUES, INCLUDING SETTING THE
c NUMBER OF USER SYMBOLS TO 0, BUT DOES NOT PLOT A SYMBOL.
c MAKE THE FIRST CALL TO SYMB BEFORE CALLING USRSYM, THE SUB. THAT 
c DEFINES USER SYMBOLS.
c CALL WITH HITE<0 TO RESET NUMBER OF USER SYMBOLS TO 0.
c
      subroutine symb(sym, hite, x, y, dash, space, az)
c ANGLE OF SYMBOL DERIVED FROM SYM VALUE  
      real ang
c CLOCKWISE ROTATION OF SYMBOL (DEGREES)  
      real az
c DASH LENGTH                             
      real dash
c FRACTIONAL REDUCTION IN SIZE PRIOR TO EA
      real defact
c TRUE ONLY THE FIRST TIME IN THIS SUB.   
      logical frstim
c HEIGHT OF SYMBOL                        
      real hite
c YZ PORTION OF SYMBOL - NPTS*10 + ITYP   
      integer ibs
c FILL CODE - (0=NO FILL, 9=ALL FILLED)   
      integer ifil
c INDEX TO ROOT SYMBOL ARRAY              
      integer isk
c                               CORRESPONDS TO SYMBOL NUMBER YZ
c IKEY(YZ) IS THE ROOT SYMBOL NUMBER THAT 
      integer ikey(99)
c XYZ PORTION OF SYMBOL (INTEGER PART)    
      integer isym
c                               3 OR GREATER=USER SYMBOL)
c TYPE OF SYMBOL (0=REG POLY, 1=STAR, 2=* 
      integer ityp
c CALCOMP PLOT CONTROL NUMBER             
      integer iup
c NUMBER OF ADDITIONAL SYMBOLS NEEDED FOR 
      integer nfil
c NUMBER OF POINTS ON SYMBOL              
      integer npts
c # OF USER SYMBOLS DEFINED SO FAR (20 MAX
      integer nu
c RADIANS PER DEGREE                      
      real rad
c SPACE LENGTH                            
      real space
c                               X (IFIL) IS THE FILL CODE (0=NO FILL, 9=
cFILLED)
c                               Y (NPTS) IS THE NUMBER OF POINTS IN SYMB
cOL 
c                               Z (ITYP) IS THE SYMBOL STYLE (0=REG POLY
c, 
c                                                    1=STAR, 2=* PATTERN
c                               ABC IS THE AZIMUTH FOR ROTATION (090=90 
cDEG)
c PLOTTING SYMBOL XYZ.ABC                 
      real sym
      logical trace
c X COORDINATE OF CENTER OF SYMBOL        
      real x
c XR(I,J) IS THE J'TH X VALUE FOR ROOT SYM
      real xr(30, 20)
c  X(I,J) IS THE J'TH X VALUE FOR USER SYM
      real xu(20, 200)
c Y COORDINATE OF CENTER OF SYMBOL        
      real y
c YR(I,J) IS THE J'TH Y VALUE FOR ROOT SYM
      real yr(30, 20)
c
c  Y(I,J) IS THE J'TH Y VALUE FOR USER SYM
      real yu(20, 200)
c
      common /symb1/ xu, yu, nu, xr, yr, ikey
      data frstim / .true. /
      data rad / .017453293 /
      trace = .false.
      if (sym .lt. 0.) trace = .true.
c
c READ ROOT SYMBOLS INTO ARRAYS
      absym = abs(sym)
      if (frstim) then
        nu = 0
        call rdrsym(xr, yr, ikey)
        frstim = .false.
        return 
      end if
c RESET USER SYMBOLS IF HITE IS NEGATIVE
      if (hite .lt. 0.) then
        nu = 0
        return 
      end if
c JUST DROP PEN IF SYMBOL HITE = 0.
      if (hite .eq. 0.) then
        call pltt(x, y, 3)
        call pltt(x, y, 2)
        return 
      end if
c DECODE SYMBOL TYPE
      isym = absym
      ang = (absym - isym) * 1000
      ifil = isym / 100
      ibs = isym - (ifil * 100)
      ityp = ibs - ((ibs / 10) * 10)
      npts = (ibs - ityp) / 10
c     PRINT *, 'SYM IFIL NPTS ITYP ANG  IBS'
c     PRINT *, ABSYM, IFIL, NPTS, ITYP, ANG, IBS
c IF SYMBOL IS TO BE FILLED, DECIDE HOW MANY INTERIOR SYMBOLS TO PLOT (N
cFIL)
c AND THE FRACTIONAL DECREASE IN SIZE BETWEEN SYMBOLS
      nfil = 0
      if (ifil .ne. 0) call nfilok(defact, hite, ibs, ifil, ityp, nfil, 
     &npts)
c PLOT NORMAL SYMBOL
      if ((ikey(ibs) .eq. 0) .or. (ityp .lt. 3)) then
      if (ikey(ibs) .eq. 0) then
      write(6, '(a, i5, 1x, a)') 'WARNING - SYMBOL NUMBER ', ibs, 
     &' IS NOT DEFINED,'
      write(6, '(a)') 'SO A + WILL BE PLOTTED.'
      call pltarr(14, hite, x, y, dash, space, az, ang, xr, yr, nfil, 
     &defact, ifil, ityp, 30, trace)
      return 
      end if
      call pltarr(ikey(ibs), hite, x, y, dash, space, az, ang, xr, yr, 
     &nfil, defact, ifil, ityp, 30, trace)
      return 
c PLOT USER DEFINED SYMBOL
c       PRINT *, 'USE USER DEFINED SYMBOL ', IBS 
      else
      call pltarr(ikey(ibs), hite, x, y, dash, space, az, ang, xu, yu, 
     &nfil, defact, ifil, ityp, 20, trace)
      return 
      end if
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
*F77CVT -- Sun extension:  32-character name
      block data v0000000000000000000000000000001
      common /symb1/ xu, yu, nu, xr, yr, ikey
      real xu(20, 200)
      real yu(20, 200)
      integer nu
      real xr(30, 20)
      real yr(30, 20)
      integer ikey(99)
      data ikey / 99*0 /
      end
c
      subroutine usrsym(usrfil, ierr)
c HEIGHT OF SYMBOL                        
      real hite
c RETURNED FROM OPENIT - NOT EQUAL 0 IF OP
      integer ierr
c                               CORRESPONDS TO SYMBOL NUMBER YZ
c IKEY(YZ) IS THE ROOT SYMBOL NUMBER THAT 
      integer ikey(99)
c                               3 OR GREATER=USER SYMBOL)
c TYPE OF SYMBOL (0=REG POLY, 1=STAR, 2=* 
      integer ityp
c INPUT LINE FROM SYMBOL DEFINITION FILE  
      character line*50
c # OF USER SYMBOLS DEFINED SO FAR (20 MAX
      integer nu
c PLOTTING SYMBOL XYZ.ABC                 
      real sym
c FILE WITH USER DEFINED SYMBOLS          
      character usrfil*50
c X COORDINATE OF CENTER OF SYMBOL        
      real x
c XR(I,J) IS THE J'TH X VALUE FOR ROOT SYM
      real xr(30, 20)
c TEMPORARY X VALUES FOR CURRENT SYMBOL   
      real xt(200)
c  X(I,J) IS THE J'TH X VALUE FOR USER SYM
      real xu(20, 200)
c Y COORDINATE OF CENTER OF SYMBOL        
      real y
c YR(I,J) IS THE J'TH Y VALUE FOR ROOT SYM
      real yr(30, 20)
c TEMPORARY Y VALUES FOR CURRENT SYMBOL   
      real yt(200)
c  Y(I,J) IS THE J'TH Y VALUE FOR USER SYM
      real yu(20, 200)
c
c  SYMBOL NUMBER TO BE DEFINED (2 DIGIT) >
      integer ibs
c
      common /symb1/ xu, yu, nu, xr, yr, ikey
      write(6, '(a, a)') 'USRFIL = ', usrfil
      call openit('read', usrfil, -20, ierr)
      if (ierr .ne. 0) then
      write(6, '(a, a)') 'COULD NOT OPEN FILE: ', usrfil
      write(6, '(a)') 'SO USER SYMBOLS WERE NOT DEFINED'
      return 
      end if
   45 read(20, *, end=80) ibs
      ityp = ibs - ((ibs / 10) * 10)
      if ((ibs .gt. 99) .or. (ityp .lt. 3)) then
      write(6, '(a)') 
     &'USER DEFINED SYMBOLS MUST HAVE CODES OF 30 OR GREATER'
      write(6, '(a, i5, a)') 'BUT BE LESS THAN 100.', ibs, 
     &' WILL NOT WORK.'
      return 
      end if
      n = 0
      if (nu .gt. 19) then
      write(6, '(a)') 'MAX OF 20 USER DEFINED SYMBOLS EXCEEDED.'
      write(6, '(a)') 'EXTRA DEFINITIONS WILL BE SKIPPED.'
      goto 80
      end if
      nu = nu + 1
   46 read(20, 47, end=79) line
c     PRINT *, LINE
   47 format(a)
c INCLUDE PREDEFINED SYMBOL
      if (line(1:1) .eq. '*') then
c       PRINT *, LINE
c DECODE SYMBOL TYPE
      read(line(2:50), *) sym, hite, x, y
      isym = sym
      ang = (sym - isym) * 1000
      ifil = isym / 100
      ibst = isym - (ifil * 100)
      ityp = ibst - ((ibst / 10) * 10)
      npts = (ibst - ityp) / 10
c IF SYMBOL IS TO BE FILLED, DECIDE HOW MANY INTERIOR SYMBOLS TO PLOT (N
cFIL)
c AND THE FRACTIONAL DECREASE IN SIZE BETWEEN SYMBOLS
      nfil = 0
      if (ifil .ne. 0) call nfilok(defact, hite, ibst, ifil, ityp, nfil
     &, npts)
c PLOT NORMAL SYMBOL
      if ((ikey(ibst) .eq. 0) .or. (ityp .lt. 3)) then
      if (ikey(ibst) .eq. 0) then
      write(6, '(a, i5, a)') 'WARNING - ROOT NUMBER ', ibs, 
     &' IS NOT DEFINED,'
      write(6, '(a)') 'SO A + WILL BE USED IN THE DEFINITION.'
      call getxyt(14, hite, x, y, ang, xr, yr, nfil, defact, ifil, ityp
     &, 30, xt, yt, nt)
      goto 475
      end if
      call getxyt(ikey(ibst), hite, x, y, ang, xr, yr, nfil, defact, 
     &ifil, ityp, 30, xt, yt, nt)
c         PRINT *, 'DEFINE SYMBOL BASED ON USER SYMBOL'
      else
      call getxyt(ikey(ibst), hite, x, y, ang, xu, yu, nfil, defact, 
     &ifil, ityp, 20, xt, yt, nt)
      end if
  475 do 48 i = 1, nt
      n = n + 1
      if (n .gt. 200) goto 70
      xu(nu,n) = xt(i)
      yu(nu,n) = yt(i)
   48 continue
      goto 46
      end if
      n = n + 1
      if (n .gt. 200) then
   70 write(6, '(a)') 
     &'MAXIMUM OF 200 POINTS PER USER DEFINED SYMBOL'
      write(6, '(a, i5)') 'TERMINATE DEFINITION LOOP WITHOUT SYMBOL #'
     &, ibs
      nu = nu - 1
      goto 80
c     PRINT *, 'NU N ', NU, N
      end if
      read(line, *) xu(nu,n), yu(nu,n)
      if (xu(nu,n) .eq. 9999.) goto 75
c
c     FINISHED ONE SYMBOL 
      goto 46
c     PRINT *, 'DEFINED SYMBOL NUMBER ', NU, ' AS ', IBS, ' WITH ',
c    * N, ' POINTS.'
c     DO 481 I = 1, N
c       PRINT *, XU(NU, I), YU(NU, I)
c481  CONTINUE
   75 ikey(ibs) = nu
c
c ERROR TERMINATION
      goto 45
   79 write(6, '(a, a)') 'USER DEFINED SYMBOL FILE: ', usrfil
      write(6, '(a)') 'DID NOT TERMINATE CORRECTLY WITH X = 9999.'
      ierr = 1
c 
      nu = nu - 1
   80 close(20) 
      return 
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
c READ REGULAL SYMBOLS INTO ARRAYS
      subroutine rdrsym(xr, yr, ikey)
c RETURNED FROM OPENIT - NOT EQUAL 0 IF OPENI
      integer ierr
c IKEY(YZ) IS THE ROOT SYMBOL NUMBER THAT    
      integer ikey(99)
c XR(I, J) IS THE J'TH X VALUE FOR SYMBOL # I
      real xr(30, 20)
c YR(I, J) IS THE J'TH Y VALUE FOR SYMBOL # I
      real yr(30, 20)

      call openit
     * ('read', 
     * '/Seis/W/we/src/plot/libcalunix/REGSYM', +20, ierr)
      if (ierr .ne. 0) then
        write(6, '(a)') 
     &  'COULD NOT OPEN SYMBOL DEFINITION FILE, SO STOP'
      endif
      do 20 i = 1, 30
        read(20, *) iv
        if (iv .ne. i) then
          write(6, '(a)') 
     *      'LOGIC NOT CORRECT ', iv, ' NOT EQUAL TO ', i
          stop 
        end if
        do 18 j = 1, 20
            read(20, *) xr(i,j), yr(i,j)
            if (xr(i,j) .eq. 9999.) goto 20
   18   continue
c READ IN IKEY POINTERS
   20 continue

   30 read(20, *, end=40) i, j
      ikey(i) = j
      goto 30
   40 close(20) 
      return 
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
c COMPUTE NFIL AND DEFACT
      subroutine nfilok(defact, hite, ibs, ifil, ityp, nfil, npts)
c FRACTIONAL REDUCTION IN SIZE PRIOR TO EA
      real defact
c TRUE ONLY THE FIRST TIME IN THIS SUB.   
      logical frstim
c HEIGHT OF SYMBOL                        
      real hite
c YZ PORTION OF SYMBOL - NPTS*10 + ITYP   
      integer ibs
c FILL CODE - (0=NO FILL, 9=ALL FILLED)   
      integer ifil
c TYPE OF SYMBOL (0=REG POLY, 1=STAR, 2=* 
      integer ityp
c NUMBER OF FILLING SYMBOLS REQUIRED FOR H
      integer mxnfil(99)
c NUMBER OF ADDITIONAL SYMBOLS NEEDED FOR 
      integer nfil
c NUMBER OF FILLING LINES, ADJUSTED FOR HI
      integer nfilht
c
c NUMBER OF POINTS ON SYMBOL              
      integer npts
      data frstim / .true. /
      data mxnfil / 99*10 /
      if (frstim) then
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
      end if
      if ((ityp .eq. 2) .or. (npts .lt. 3)) then
      nfil = 0
      return 
      end if
      nfilht = (mxnfil(ibs) * hite) + .5
      nfil = ((nfilht * ifil) / 9.) + 0.5
c     PRINT *, 'IBS, HITE, NFIL'
c     PRINT *,  IBS, HITE, NFIL
c     PRINT *, 'DEFACT ', DEFACT
      defact = 1. / (nfilht + 1)
      return 
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
c PLOT PRE-DEFINED SYMBOL NUMBER NU
      subroutine pltarr(isk, hite, x, y, dash, space, az, ang, xx, yy, 
     &nfil, defact, ifil, ityp, ndx, trace)
c ANGLE OF SYMBOL DERIVED FROM SYM VALUE  
      real ang
c CLOCKWISE ROTATION OF SYMBOL (DEGREES)  
      real az
c COS(ORIENTATION ANGLE)                  
      real caz
c DASH LENGTH                             
      real dash
c FRACTIONAL REDUCTION IN SIZE PRIOR TO EA
      real defact
c FRACTIONAL SIZE OF FILL SYMBOL          
      real factor
c HEIGHT OF SYMBOL                        
      real hite
c FILL CODE - (0=NO FILL, 9=ALL FILLED)   
      integer ifil
c TYPE OF SYMBOL (0=REG POLY, 1=STAR, 2=* 
      integer ityp
c CALCOMP PLOT CONTROL NUMBER             
      integer iup
c INDEX TO SYMBOL ARRAY                   
      integer isk
c NUMBER OF ADDITIONAL SYMBOLS NEEDED FOR 
      integer nfil
c RADIANS PER DEGREE                      
      real rad
c SIN(ORIENTATION ANGLE)                  
      real saz
c SIZE OF INER SQUARE FOR COMPLETELY FILLE
      real smlest
c SPACE LENGTH                            
      real space
c IF TRUE, WRITE ALL X,Y VALUES TO FILE 21
      logical trace
c X VALUE TO PLOT                         
      real xplt
c TEMPORARY X VALUES FOR CURRENT SYMBOL   
      real xt(200)
c XX(I,J) IS THE J'TH X VALUE FOR SYMBOL #
      real xx(ndx, *)
c Y VALUE TO PLOT                         
      real yplt
c TEMPORARY Y VALUES FOR CURRENT SYMBOL   
      real yt(200)
c YY(I,J) IS THE J'TH Y VALUE FOR SYMBOL #
      real yy(ndx, *)
      data rad / .017453293 /
      iup = 3
      if ((az + ang) .eq. 0.) then
c         PRINT *, XX(ISK,N), YY(ISK,N)
      do 30 n = 1, 200
      if (xx(isk,n) .eq. 8888.) then
      if (trace) write(21, *) xx(isk,n), yy(isk,n)
      xt(n) = 8888.
      iup = 3
      goto 30
      end if
      if (xx(isk,n) .eq. 9999.) goto 42
      xt(n) = xx(isk,n) * hite
      xplt = x + xt(n)
      yt(n) = yy(isk,n) * hite
      yplt = y + yt(n)
      call dashit(xplt, yplt, iup, dash, space, 0.)
      iup = 2
      if (trace) write(21, *) xt(n), yt(n)
   30 continue
      else
      caz = cos(rad * (az + ang))
      saz = sin(rad * (az + ang))
      do 40 n = 1, 200
      if (xx(isk,n) .eq. 8888.) then
      if (trace) write(21, *) xx(isk,n), yy(isk,n)
      xt(n) = 8888.
      iup = 3
      goto 40
      end if
      if (xx(isk,n) .eq. 9999.) goto 42
      xt(n) = ((xx(isk,n) * hite) * caz) + ((yy(isk,n) * hite) * saz)
      xplt = x + xt(n)
      yt(n) = ((yy(isk,n) * hite) * caz) - ((xx(isk,n) * hite) * saz)
      yplt = y + yt(n)
      call dashit(xplt, yplt, iup, dash, space, 0.)
      iup = 2
      if (trace) write(21, *) xt(n), yt(n)
   40 continue
      end if
   42 if (trace) write(21, *) xx(isk,n), yy(isk,n)
      nsv = n - 1
      if (nfil .ne. 0) then
      factor = 1.
      do 60 i = 1, nfil
      iup = 3
      factor = factor - defact
      do 50 n = 1, nsv
      if (xt(n) .eq. 8888.) then
      if (trace) write(21, *) xt(n), yt(n)
      iup = 3
      goto 50
      end if
      xplt = x + (xt(n) * factor)
      yplt = y + (yt(n) * factor)
      call dashit(xplt, yplt, iup, dash, space, 0.)
      iup = 2
      if (trace) write(21, *) xt(n) * factor, yt(n) * factor
   50 continue
c       CALL TRANSP
c       PRINT *, 'FINAL VALUE OF FACTOR = ', FACTOR
c       FILL IN THE CENTER
   60 continue
      if (ifil .eq. 9) then
      smlest = (defact * hite) * .125
c         PRINT *, 'HITE, DEFACT'
c         PRINT *, 'ITYP, SMLEST', ITYP, SMLEST
      if (ityp .eq. 1) smlest = smlest * .5
      call mbox(x - smlest, y - smlest, x + smlest, y + smlest)
      end if
      end if
      return 
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
c GET POINTS FOR SYMBOL ISK
      subroutine getxyt(isk, hite, x, y, ang, xu, yu, nfil, defact, ifil
     &, ityp, ndx, xt, yt, nt)
c ANGLE OF SYMBOL DERIVED FROM SYM VALUE  
      real ang
c COS(ORIENTATION ANGLE)                  
      real caz
c FRACTIONAL REDUCTION IN SIZE PRIOR TO EA
      real defact
c FRACTIONAL SIZE OF FILL SYMBOL          
      real factor
c HEIGHT OF SYMBOL                        
      real hite
c FILL CODE - (0=NO FILL, 9=ALL FILLED)   
      integer ifil
c INDEX TO SYMBOL ARRAY                   
      integer isk
c TYPE OF SYMBOL (0=REG POLY, 1=STAR, 2=* 
      integer ityp
c CALCOMP PLOT CONTROL NUMBER             
      integer iup
c NUMBER OF ADDITIONAL SYMBOLS NEEDED FOR 
      integer nfil
c RADIANS PER DEGREE                      
      real rad
c SIN(ORIENTATION ANGLE)                  
      real saz
c SIZE OF INER SQUARE FOR COMPLETELY FILLE
      real smlest
c TEMPORARY X VALUES FOR CURRENT SYMBOL   
      real xt(200)
c  X(I,J) IS THE J'TH X VALUE FOR USER SYM
      real xu(ndx, *)
c TEMPORARY Y VALUES FOR CURRENT SYMBOL   
      real yt(200)
c  Y(I,J) IS THE J'TH Y VALUE FOR USER SYM
      real yu(ndx, *)
      data rad / .017453293 /
      j = 0
      factor = 1. + defact
      do 60 i = 1, nfil + 1
      factor = factor - defact
      if (ang .eq. 0.) then
      do 30 n = 1, 200
      if (xu(isk,n) .eq. 8888.) then
      j = j + 1
      xt(j) = 8888.
      goto 30
      end if
      if (xu(isk,n) .eq. 9999.) goto 60
      j = j + 1
      xt(j) = x + ((xu(isk,n) * hite) * factor)
      yt(j) = y + ((yu(isk,n) * hite) * factor)
   30 continue
      else
      caz = cos(rad * ang)
      saz = sin(rad * ang)
      do 40 n = 1, 200
      if (xu(isk,n) .eq. 8888.) then
      j = j + 1
      xt(j) = 8888.
      goto 30
      end if
      if (xu(isk,n) .eq. 9999.) goto 60
      j = j + 1
      xt(j) = (x + (((xu(isk,n) * hite) * caz) * factor)) + (((yu(isk,n)
     & * hite) * saz) * factor)
      yt(j) = (y + (((yu(isk,n) * hite) * caz) * factor)) - (((xu(isk,n)
     & * hite) * saz) * factor)
   40 continue
      end if
c     FILL IN THE CENTER
   60 continue
      if (ifil .eq. 9) then
      smlest = (defact * hite) * .125
      if (ityp .eq. 1) smlest = smlest * .5
      j = j + 1
      xt(j) = 8888.
      j = j + 1
      xt(j) = x - smlest
      yt(j) = y - smlest
      j = j + 1
      xt(j) = x - smlest
      yt(j) = y + smlest
      j = j + 1
      xt(j) = x + smlest
      yt(j) = y + smlest
      j = j + 1
      xt(j) = x + smlest
      yt(j) = y - smlest
      j = j + 1
      xt(j) = x - smlest
      yt(j) = y - smlest
      end if
      nt = j
      return 
      end
