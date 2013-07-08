      subroutine readeqdb (ain, az, cmpsit, date, db, dist, distmx, 
     & erate, eunit, event, fmagmn, ijeff, iorin, isnrwt, iunit, jjwt, 
     & kilsta, maxobs, minobs, munit, mxqual, mxstat, ndistwt, nkil,
     & nor, nqualwt, nr, nrev, nrwt, plusert, pobs, prcntx, presmx,
     & prmk, revsta, sigmaf, stn, sumwt, sunit, wterat, wtkey, wtlim,
     & wtqlim, wtmax, wtobs, xainmn, xainmx, xcode )
c
c
c  reads event and arrival info from css database. returns dummy summary card  
c  and corresponding phase first motions, qualitites, angles of incidence, station  
c  names, and azimuths. calculates standard deviation (sigmaf) of fit from estimated 
c  weights of the data.  the estimated data weights are parameter-file inputs; 
c  corresponding data weights are calculated from:
c  (quality weight based on deltim parameter) or (weight based on signal-to-noise ratio) +
c  (distance weight) + (first motion weight ) = total observation weight
c  note: for fm = + or - (not clear up or down), the reading weight code is defined by plusert
c  
c  all individual weight parameters range from 0 (not used) to 1 (max weight). 
c  total weight range is from 0 to 3.
c
      include "db.i"
      real              ain(mxstat)  ! (output) ray incidence angles 
      real              az(mxstat)   ! (output) ray azimuth angles (corresponding to ain)
      logical           cmpsit       ! (input) true for composite solutions of all data
      character*10      date(mxstat) ! (output) arrival time date yrmodyhrmn
      real              dist(mxstat) ! (output) epicentral distance
      real*8            distmx       ! (input) maximum permitted epicentral distance 
      real              erate(mxqual) ! (input) assumed weighted error rates for each data class
      integer           eunit        ! (input) logical unit # of output of error messages
      character*80      event        ! (output) summary card
      real*8            fmag         ! event magnitude
      real              fmagmn       ! (input) minimum permitted magnitude
      integer           ijeff        ! (input) flag: 1(0)=do (not) use data weighted out by jeffrey's weighting
      integer           isnrwt       ! (input) flag controling signal-to-noise weighting (0=omit, 1=use)
      integer           iunit        ! (input) logical unit # of hypoellipse archive file
      integer           j            ! dummy loop index
      integer           k            ! counter of good phase readings
      integer           kwt          ! number of phases to be used in current eq
      integer           jjwt(mxstat) ! index for data weight 
      character*4       kilsta(mxstat) ! (input) ignored station names
      integer           maxobs       ! (input) maximum number of observations
      integer           minobs       ! (input) minimum number of observations required 
      integer           munit        ! (input) logical unit for writing events used in composite
      integer           mxqual       ! (input) maximum # of qualities permitted
      integer           mxstat       ! (input) maximum # of stations permitted
      integer           ndistwt      ! # of distance weights
      integer           nkil         ! (input) number of ignored stations
      integer           nqualwt      ! # of quality weights
      integer           nsofar       ! current number of first motions in composite solution 
      integer           nr           ! (output) -1=eof, 0=skip event,
c                                    !  nr>0 => number of stations with first motions, same as kwt
      integer           nrwt         ! (output) number of observations with non-zero weight
      integer           nrev         ! (input) number of reversed stations
      real*8            plusert      ! (input) error rate weight for + or -
      real              pobs(mxstat) ! (output) observed first motion polarities; .5=compression, -.5=dilatation
      real              prcntx       ! (output) % of stations that are machine picked
      real              presmx       ! (input) maximum permitted p-residual
      character*4       prmk(mxstat) ! (output) first motion description (eg. ipu0)
      character*4       revsta(mxstat) ! (input) reversed station names
      real              sigmaf       ! (output) calculated standard deviation of fit based on data errors
      real*8            snr          ! signal-to-noise ratio
      character*4       stn(mxstat)  ! (output) station names
      real              sumwt        ! (output) sum of observed first motion weights
      integer           sunit        ! (input) logical unit # of summary records of events in composite solution
      real              wterat(mxqual) ! (input) error rates for alternate weighting
      character*4       wtkey         ! (input) key to alternate weights - dist, azim angi
      real              wtlim(mxqual) ! (input) limits of alternate variable
      real              wtqlim(mxqual) ! (input) limits of quality weighting
      real              wtmax         ! (output) maximum weight
      real              wtobs(mxstat) ! (output) observed first motions weights
      real              xainmn        ! (input) minimum ang of inc to exclude
      real              xainmx        ! (input) maximum ang of inc to exclude
      character*7       xcode(mxstat) ! (output) code giving reason for exclusion
c                                       k = kill station (1); i = angel of inc. (2);
c                                       m = missing values (3); d = distance (4); 
c                                       r = P-residual (5); w = quality weight (6); 
c                                       + = not clear up or down (7)


      integer iorin, nor, narr, nsnr, keys, ndef
      integer db(4), dba(4), dbtmp1(4), dbev(4), dbtmp(4), dbo(4), 
     &dbsnr(4)
      character expr*256, dbfm*2, station*4, tmpdate*10, tmpprmk*4
      character*20 or_time_str, ar_time_str
      real*8 lat, lon, dep, delta, esaz, ema, pres, deltim, snrtmp
      real*8 or_time, ar_time
      
c
c reset values
c
      prcntx = 0.

c find weight equivalents for ray file, used by fpplot for scaling symbols
      if(ijeff.ne.0.or.isnrwt.ne.0) then
        wtmax = 3.
        wtmin = plusert + .5
      else
        wtmax = 2.
        wtmin = plusert + .25
      endif

        wtmax1 = wtmin+(wtmax-wtmin)*3/4
        wtmax2 = wtmin+(wtmax-wtmin)/2
        wtmax3 = wtmin+(wtmax-wtmin)/4

c open origin table

      call dblookup (dbo, db, "", "origin", "", "")
      
      if (.not.cmpsit) then
c not composite, read one event with iorin passed from main program
c subset origin table for iorin
      write(expr,111) iorin
111   format('orid == ',i10)
      call dbsubset (dbo, dbo, expr, 0)
      
c get time,lat,lon,depth,mag of event
      dbo(4) = 0
      idb = dbgetv( dbo, 0, "time", or_time,
     &                      "lat", lat,
     &                      "lon", lon,
     &                      "depth", dep,
     &                      "ndef", ndef,
     &                      "ml", fmag, 0)


c convert epoch time into strtime
      call strtime(or_time_str,or_time)
      write(event,31) or_time_str, lat, lon, dep, fmag, ndef
31    format(a20,x,f7.4,x,f9.4,x,f6.2,x,f3.1,x,i3)
      print*,event
            
c if mag less then minmag, quit and return to main program
        if(fmag .lt. fmagmn) then
        print*,'Event orid=',iorin,' has magnitude',fmag,' less than all
     &owable minimum'
        return
        endif

c read first motions 
c join origin, assoc and arrival tables for the given orid
      call dblookup(dba, db, 0, "assoc", 0, 0)
      call dbjoin(dbtmp, dbo, dba, 0,0,0,0,0) 
      call dblookup(dba, db, 0, "arrival", 0, 0)  
      call dbjoin(dbev, dbtmp, dba, 0,0,0,0,0) 
      call dbquery(dbev, dbRECORD_COUNT, narr)
      if (narr. lt. minobs) then
      print*,'Number of potentially usable observations is less than min
     &imum'
      nr = 0
      return
      endif
c subset for P phase
      write(expr , 112)
112   format("phase == 'P'")
      call dbsubset (dbev, dbev, expr, 0)
      call dbquery(dbev, dbRECORD_COUNT, narr)
      if (narr. lt. minobs) then
      print*,'Number of potentially usable observations is less than min
     &imum'
      nr = 0
      return
      endif

c subset for vertical channels
      write(expr , 113)
113   format("chan == 'SHZ' || chan == 'BHZ' || chan == 'EHZ' || 
     &chan == 'HHZ'")
      call dbsubset (dbev, dbev, expr, 0)
      call dbquery(dbev, dbRECORD_COUNT, narr)
      if (narr. lt. minobs) then
      print*,'Number of potentially usable observations is less than min
     &imum'
      nr = 0
      return
      endif

c subset for arrivals with first motions
      write(expr , 114)
114   format("fm != '-' && fm != '..'")
      call dbsubset (dbev, dbev, expr, 0)
      call dbquery(dbev, dbRECORD_COUNT, narr)
      if (narr. lt. minobs) then
      print*,'Number of potentially usable observations is less than min
     &imum'
      nr = 0
      return
      endif

c      write(expr , 115) distmx
c115   format("delta <= ",f5.2)
c      print*, expr
c      call dbsubset (dbev, dbev, expr, 0)
c      write(expr , 116) presmx, -presmx
c116   format("timeres <= ",f6.2," && timeres >= ",f6.2)
c      print*, expr
c      call dbsubset (dbev, dbev, expr, 0)
c dbev should be all we need now: P phases, vertical channels, 
c with 1st motions    
      
      print*,'Number of potentially usable 1st motions = ', narr
      
      print*,'Station Distance   Asimuth    Angle    1stMotion    Weight 
     &  XCode'
        
c find snr range

      if(isnrwt.ne.0) then
      
c subset for snr > 0
        write(expr , 117)
117     format("snr > 0")
        call dbsubset (dbsnr, dbev, expr, 0)
        call dbquery (dbsnr, dbRECORD_COUNT, nsnr)
c find min and max snr
        if (nsnr. gt. 0) then
          write(expr , 118)
118       format("snr")
          call strtbl (keys, expr, 0)
          call dbsort (dbsnr, dbsnr, keys, 0, "")
          call dbquery (dbsnr, dbRECORD_COUNT, nsnr)
          dbsnr(4) = 0
          jdb = dbgetv( dbsnr, 0, "snr", snrtmp, 0)
          snrmin = snrtmp
          dbsnr(4) = nsnr-1
          jdb = dbgetv( dbsnr, 0, "snr", snrtmp, 0)
          snrmax = snrtmp
c find snr limits for assigning weights
          snr1 = (snrmax-snrmin)/4
          snr2 = (snrmax-snrmin)/2
          snr3 = (snrmax-snrmin)*3/4
          print*,'snrmax = ', snrmax,' snrmin =', snrmin
        else
          nr = 0
          print*,'No signal-to-noise values in arrival table.'
          print*,'Can''t assign weights based on SNR values, so quit.'
          return
        endif
        
      endif
      
c starting arrivals loop

      kwt = 0
      do j = 0,narr-1
c assign 0 weight
      wtobs(j+1) = 0

c get distance, azimuth, station name, first motion, travel-time residual,
c pick uncertainity, arrival time, signal-to-noise ratio, angle of incidence
      dbev(4) = j
          jdb = dbgetv( dbev, 0, "delta", delta,
     &                           "esaz", esaz,
     &                           "sta", station,
     &                           "timeres", pres,
     &                           "deltim", deltim,
     &                           "time", ar_time,
     &                           "snr", snr,
     &                           "fm", dbfm,
     &                           "ema", ema, 0)
        
      	  stn(j+1) = station
c convert epoch time into yrmodyhrmn for output ray file
          call strtime(ar_time_str,ar_time)
          tmpdate(1:2) = ar_time_str(9:10)
          tmpdate(3:4) = ar_time_str(1:2)
          tmpdate(5:6) = ar_time_str(4:5)
          tmpdate(7:8) = ar_time_str(13:14)
          tmpdate(9:10) = ar_time_str(16:17)
          date(j+1) = tmpdate

c check for missing values and go to next arrival if missing values
      if (delta. lt. 0) then
        write(eunit, 201) station
        write(*, 201) station
201     format('Readeqdb: Station ',a4,' has no delta parameter')
        xcode(j+1)(3:3) = 'm' 
        go to 82
      endif

      if (esaz. lt. 0. or. esaz. gt. 360) then
        write(eunit, 202) station
        write(*, 202) station
202   format('Readeqdb: Station ',a4,' has out of range esaz parameter')
        xcode(j+1)(3:3) = 'm' 
        go to 82
      endif

      if (pres. eq. -999.) then
        write(eunit, 203) station
        write(*, 203) station
203     format('Readeqdb: Station ',a4,' has Null pres parameter')
        xcode(j+1)(3:3) = 'm' 
        go to 82
      endif

      if (deltim. lt. 0. and. ijeff. ne. 0) then
        write(eunit, 204) station
        write(*, 204) station
204     format('Readeqdb: Station ',a4,' has Null deltim parameter')
        xcode(j+1)(3:3) = 'm' 
        go to 82
      endif

      if (snr. lt. 0. and. isnrwt. ne. 0) then
        write(eunit, 205) station
        write(*, 205) station
205   format('Readeqdb: Station ',a4,' has out of range snr parameter')
        xcode(j+1)(3:3) = 'm' 
        go to 82
      endif
          
      if (ema. eq. -1.) then
        write(eunit, 206) station
        write(*, 206) station
206     format('Readeqdb: Station ',a4,' has no ema parameter')
        xcode(j+1)(3:3) = 'm' 
        go to 82
      endif

c proceed if all necessary parameters are present

      	  dist(j+1) = delta*111.195
      	  az(j+1) = esaz
      	  ain(j+1) = ema
      	  
c assign first motions
      	if (dbfm(1:1).eq.'c') then
      	  pobs(j+1) = .5
      	elseif (dbfm(1:1).eq.'d') then
      	  pobs(j+1) = -.5
      	else
      	  dbfm(1:1) = 'N'
c      	  pobs(j+1) = 0.0
      	end if

c downgrade observations if not clear up or not clear down
        if (dbfm(1:1) .eq. 'c'.or.dbfm(1:1) .eq. 'd') then
          wtobs(j+1) = wtobs(j+1) + 1
        else
          wtobs(j+1) = 0
          xcode(k)(7:7) = '+'
        endif

c
c ignore this station?
c
        xcode(j+1) = '       '
        errat = 0.
        if (nkil .gt. 0) then
          do 81 i = 1, nkil
            if (stn(j+1) .eq. kilsta(i)) then
              xcode(j+1)(1:1) = 'k'
              errat = .5
              goto 82
            endif
81        continue
        endif
      	
c check for distance
        if (dist(j+1) .gt. distmx) then
          xcode(j+1)(4:4) = 'd'
          errat = .5
        endif

c check range of angle of incidence
        if((ain(j+1) - xainmn)*(ain(j+1) - xainmx) .le. 0.) then
          xcode(j+1)(2:2) = 'i'
          errat = .5
        endif

c check travel time residual range
       if (abs(pres) .gt. presmx) then
          xcode(j+1)(5:5) = 'r'
          errat = .5
        endif

c if station is not used, go to next one
        if(errat.eq.0.5) goto 82
        
c flip polariites if station is designated as reversed
        if (nrev.gt.0) then
          do 90 i = 1, nrev
            if (stn(j+1) .eq. revsta(i)) pobs(j+1) = -pobs(j+1)
90        continue
        endif

c take care of observation weights based on deltim parameter   
        if(ijeff.ne.0) then

          do l = 1,nqualwt
            if(l.eq.1) then
              tmpmin = -1
              tmpmax = wtqlim(1)
            else
              tmpmin = wtqlim(l-1)
              tmpmax = wtqlim(l)
            endif
        
            if (deltim .le. tmpmax. and. deltim .gt. tmpmin) then
              wtobs(j+1) = wtobs(j+1)+erate(l)
             errat = 1
            endif
           
          enddo

            if(errat.ne.1) then
              wtobs(j+1) = 0
              xcode(k)(6:6) = 'w'
            endif
        
        endif
                  
c take care of observation weights based on signal to noise ratio

      if (isnrwt.ne.0) then
      
      if ( snr.lt.snr1 .and. snr.gt.0) then
        wtobs(j+1) = wtobs(j+1)+0.25
      elseif ( snr.lt.snr2. and. snr.ge.snr1 ) then
        wtobs(j+1) = wtobs(j+1)+0.5
      elseif ( snr.lt.snr3. and. snr.ge.snr2 ) then
        wtobs(j+1) = wtobs(j+1)+0.75
      elseif ( snr.ge.snr3 ) then
        wtobs(j+1) = wtobs(j+1)+1  
      else  
        wtobs(j+1) = 0
        xcode(k)(6:6) = 'w'
      endif

      endif
            
c take care of distance weighting for observations with non-zero weight

        if (wtobs(j+1). gt. 0) then
c          elseif (dnstrng(wtkey) .eq. 'dist') then
        
          do l = 1,ndistwt
            if(l.eq.1) then
              tmpmin = -1
              tmpmax = wtlim(1)
            else
              tmpmin = wtlim(l-1)
              tmpmax = wtlim(l)
            endif
        
            if (delta .le. tmpmax. and. delta .gt. tmpmin) then
              wtobs(j+1) = wtobs(j+1)+wterat(l)
              errat = 1
            endif
           
          enddo
         
            if(errat.ne.1) then
              wtobs(j+1) = 0
              xcode(k)(4:4) = 'd'
            endif
         
         endif

        if(wtobs(j+1).ne.0) kwt=kwt+1


82     continue
c compose prmk variable for ray file, then used by fpplot
      tmpprmk=' p  '
c      tmpprmk(3:3)=dbfm(1:1)
      if(pobs(j+1).eq.0.5) then
        tmpprmk(3:3)= 'c'
      else if(pobs(j+1).eq.-0.5) then
        tmpprmk(3:3)= 'd'      
      else
        tmpprmk(3:3)= 'N'
      end if
      if(wtobs(j+1).ge.wtmax) tmpprmk(4:4)='0'
      if(wtobs(j+1).ge.wtmax1.and.wtobs(j+1).lt.wtmax) tmpprmk(4:4)='0'
      if(wtobs(j+1).ge.wtmax2.and.wtobs(j+1).lt.wtmax1) tmpprmk(4:4)='1'
      if(wtobs(j+1).ge.wtmax3.and.wtobs(j+1).lt.wtmax2) tmpprmk(4:4)='2'
      if(wtobs(j+1).gt.0.and.wtobs(j+1).lt.wtmax3) tmpprmk(4:4)='3'
      if(wtobs(j+1).eq.0) tmpprmk(4:4)='4'
      prmk(j+1) = tmpprmk

       print *, stn(j+1), dist(j+1), az(j+1), ain(j+1), 
     & pobs(j+1),wtobs(j+1),xcode(j+1)

      enddo

      print*,'Number of usable 1st motions = ', kwt
      
      if(kwt. lt. minobs ) then
        nr=0
        print*,'Number of first motions is less than minimum'
        goto 99
      elseif(kwt .gt. maxobs) then
        nr=0
        print*,'Number of first motions is greater than maximum'
        goto 99
      else
        nr=kwt
      endif
      
      else
c composite run, read all arrivals in the database
      
      nsofar = 0
      kwt = 0
      nused = 0
      iadd = 0
      
c subset for mag >= magmin
      call dbquery(dbo, dbRECORD_COUNT, nor)
      print*,'Total number of events in the database nor = ',nor
      write(expr,119) fmagmn, fmagmn
119   format("ml >= ",f5.2," || mb >= ",f5.2)
      call dbsubset (dbo, dbo, expr, 0)
      call dbquery(dbo, dbRECORD_COUNT, nor)
      print*,'Number of events greater than minimum magnitude cutoff nor
     & = ',nor
      
      if(nor. lt. 1) then
      print*,'No events passed magnitude criterion for composite run'
      nr = 0
      return
      elseif(nor. eq. 1) then
      print*,'One event passed magnitude criterion for composite run.'
      print*,'Calculate focal mechanism as for a single event.'
      endif

c find snr range 

      if(isnrwt.ne.0) then
      
c join origin, assoc and arrival tables
        call dblookup(dba, db, 0, "assoc", 0, 0)
        call dbjoin(dbtmp, dbo, dba, 0,0,0,0,0) 
        call dblookup(dba, db, 0, "arrival", 0, 0)  
        call dbjoin(dbtmp1, dbtmp, dba, 0,0,0,0,0) 
        call dbquery(dbtmp1, dbRECORD_COUNT, nsnr)
      
        if (nsnr.gt.0) then
      
          write(expr , 117)
          call dbsubset (dbsnr, dbtmp1, expr, 0)
          call dbquery (dbsnr, dbRECORD_COUNT, nsnr)
          write(expr , 118)
          call strtbl (keys, expr, 0)
          call dbsort (dbsnr, dbsnr, keys, 0, "")
          call dbquery (dbsnr, dbRECORD_COUNT, nsnr)
          dbsnr(4) = 0
          jdb = dbgetv( dbsnr, 0, "snr", snrtmp, 0)
          snrmin = snrtmp
          dbsnr(4) = nsnr-1
          jdb = dbgetv( dbsnr, 0, "snr", snrtmp, 0)
          snrmax = snrtmp
          snr1 = (snrmax-snrmin)/4
          snr2 = (snrmax-snrmin)/2
          snr3 = (snrmax-snrmin)*3/4
          print*,'snrmax = ', snrmax,' snrmin =', snrmin
        else
          nr = 0
          print*,'No signal-to-noise values in arrival table.'
          print*,'Can''t assign weights based on SNR values, so quit.'
          return
        endif
        
      endif
      
      
c do loop for origins
      do i = 0,nor-1
      
      dbo(4) = i
c get orid,time,lat,lon,depth,mag of event
      idb = dbgetv( dbo, 0, "orid", iorin,
     &                      "time", or_time,
     &                      "lat", lat,
     &                      "lon", lon,
     &                      "depth", dep,
     &                      "ndef", ndef,
     &                      "ml", fmag, 0)
       
c convert epoch time into strtime
      call strtime(or_time_str,or_time)
      write(event,31) or_time_str, lat, lon, dep, fmag
      print*,event
       
c subset for iorin
      write(expr,111) iorin
      call dbsubset (dbev, dbo, expr, 0)
      
c read first motions for given event
c join origin, assoc and arrival tables for the given orid
      call dblookup(dba, db, 0, "assoc", 0, 0)
      call dbjoin(dbtmp, dbev, dba, 0,0,0,0,0) 
      call dblookup(dba, db, 0, "arrival", 0, 0)  
      call dbjoin(dbev, dbtmp, dba, 0,0,0,0,0) 
      call dbquery(dbev, dbRECORD_COUNT, narr)
      if (narr. le. 0) then
        print*,'Go to next event'
        print*,'No potentially usable observations for orid = ',iorin
        go to 85
      endif

c subset for P phase
      write(expr , 112)
      call dbsubset (dbev, dbev, expr, 0)
      call dbquery(dbev, dbRECORD_COUNT, narr)
      if (narr. le. 0) then
        print*,'No potentially usable observations for orid = ',iorin
        print*,'Go to next event'
        go to 85
      endif

c subset for vertical channels
      write(expr , 113)
      call dbsubset (dbev, dbev, expr, 0)
      call dbquery(dbev, dbRECORD_COUNT, narr)
      if (narr. le. 0) then
        print*,'No potentially usable observations for orid = ',iorin
        print*,'Go to next event'
        go to 85
      endif

c subset for arrivals with first motions
      write(expr , 114)
      call dbsubset (dbev, dbev, expr, 0)
      call dbquery(dbev, dbRECORD_COUNT, narr)
      if (narr. le. 0) then
        print*,'No potentially usable observations for orid = ',iorin
        print*,'Go to next event'
        go to 85
      endif

c dbev should be all we need now: P phases, vertical channels, 
c with 1st motions    
      
      if(narr. le.0) then
        print*,'No potentially usable arrivals for orid = ',iorin
        print*,'Go to next event'
        go to 85
      endif

      print*,'N potentially usable 1st motions = ', narr,' for orid = ',
     &iorin
              
c starting arrivals loop

      nsofar = 0

      do j = 0,narr-1
c assign 0 weight
      jj = iadd+j+1
      wtobs(jj) = 0

c get distance, azimuth, station name, first motion, travel-time residual,
c pick uncertainity, arrival time, signal-to-noise ratio, angle of incidence
      dbev(4) = j
          jdb = dbgetv( dbev, 0, "delta", delta,
     &                           "esaz", esaz,
     &                           "sta", station,
     &                           "timeres", pres,
     &                           "deltim", deltim,
     &                           "time", ar_time,
     &                           "snr", snr,
     &                           "fm", dbfm,
     &                           "ema", ema, 0)
        
c convert epoch time into yrmodyhrmn for output ray file
          call strtime(ar_time_str,ar_time)
          tmpdate(1:2) = ar_time_str(9:10)
          tmpdate(3:4) = ar_time_str(1:2)
          tmpdate(5:6) = ar_time_str(4:5)
          tmpdate(7:8) = ar_time_str(13:14)
          tmpdate(9:10) = ar_time_str(16:17)
          date(jj) = tmpdate

       	  stn(jj) = station

c check for missing or out of range values and go to next arrival if missing
      if (delta. lt. 0) then
        write(eunit, 211) station, iorin
        write(*, 211) station, iorin
211     format('Readeqdb: Station ',a4,' origin ',i10,' has no delta par
     &ameter')
        xcode(jj)(3:3) = 'm' 
        go to 84
      endif

      if (esaz. lt. 0. or. esaz. gt. 360) then
        write(eunit, 212) station, iorin
        write(*, 212) station, iorin
212     format('Readeqdb: Station ',a4,' origin ',i10,' has out of range 
     &esaz parameter')
        xcode(jj)(3:3) = 'm' 
        go to 84
      endif

      if (pres. eq. -999.) then
        write(eunit, 213) station, iorin
        write(*, 213) station, iorin
213     format('Readeqdb: Station ',a4,' origin ',i10,' has Null pres pa
     &rameter')
        xcode(jj)(3:3) = 'm' 
        go to 84
      endif

      if (deltim. lt. 0. and. ijeff. ne. 0) then
        write(eunit, 214) station, iorin
        write(*, 214) station, iorin
214     format('Readeqdb: Station ',a4,' origin ',i10,' has Null deltim 
     &parameter')
        xcode(jj)(3:3) = 'm' 
        go to 84
      endif

      if (snr. lt. 0. and. isnrwt. ne. 0) then
        write(eunit, 215) station, iorin
        write(*, 215) station, iorin
215     format('Readeqdb: Station ',a4,' origin ',i10,' has out of range
     & snr parameter')
        xcode(jj)(3:3) = 'm' 
        go to 84
      endif
          
      if (ema. eq. -1.) then
        write(eunit, 216) station, iorin
        write(*, 216) station, iorin
216     format('Readeqdb: Station ',a4,' origin ',i10,' has no ema param
     &eter')
        xcode(jj)(3:3) = 'm' 
        go to 84
      endif

c proceed if all parameters are OK
      	  dist(jj) = delta*111.195
      	  az(jj) = esaz
      	  ain(jj) = ema
      	  
c assign first motions
      	if (dbfm(1:1).eq.'c') then
      	  pobs(jj) = .5
      	elseif (dbfm(1:1).eq.'d') then
      	  pobs(jj) = -.5
      	else
      	  dbfm(1:1) = 'N'
      	end if

c downgrade observations if not clear up or not clear down
        if (dbfm(1:1) .eq. 'c'.or.dbfm(1:1) .eq. 'd') then
          wtobs(jj) = wtobs(jj) + 1
        elseif(dbfm(1:1) .eq. 'p'.or.dbfm(1:1) .eq. 'm') then
          wtobs(jj) = wtobs(jj) + plusert
          xcode(k)(7:7) = '+'
        else
          wtobs(jj) = 0
          xcode(k)(7:7) = '+'
        endif
c
c ignore this station?
c
        xcode(jj) = '       '
        errat = 0.
        if (nkil .gt. 0) then
          do 83 ii = 1, nkil
            if (stn(jj) .eq. kilsta(ii)) then
              xcode(jj)(1:1) = 'k'
              errat = .5
              goto 84
            endif
83        continue
        end if
      	
c check for distance
        if (dist(jj) .gt. distmx) then
          xcode(jj)(4:4) = 'd'
          errat = .5
        endif

c check range of angle of incidence
        if((ain(jj) - xainmn)*(ain(jj) - xainmx) .le. 0.) then
          xcode(jj)(2:2) = 'i'
          errat = .5
        endif

c check travel time residual range
        if (abs(pres) .gt. presmx) then
          xcode(jj)(5:5) = 'r'
          errat = .5
        endif

c if station is not used, go to next one
        if(errat.eq.0.5) goto 84
        
c flip polariites if station is designated as reversed
        if (nrev.gt.0) then
          do 91 ii = 1, nrev
            if (stn(jj) .eq. revsta(ii)) pobs(jj) = -pobs(jj)
91        continue
        endif

c take care of observation weights based on deltim parameter   
        if(ijeff.ne.0) then

          do l = 1,nqualwt
            if(l.eq.1) then
              tmpmin = -1
              tmpmax = wtqlim(1)
            else
              tmpmin = wtqlim(l-1)
              tmpmax = wtqlim(l)
            endif
        
            if (deltim .le. tmpmax. and. deltim .gt. tmpmin) then
              wtobs(jj) = wtobs(jj)+erate(l)
             errat = 1
            endif
           
          enddo

            if(errat.ne.1) then
              wtobs(jj) = 0
              xcode(k)(6:6) = 'w'
            endif
        
        endif
                  
c take care of observation weights based on signal to noise ratio
c
      if (isnrwt.ne.0) then
      
      if ( snr.lt.snr1 .and. snr.gt.0) then
        wtobs(jj) = wtobs(jj)+0.25
      elseif ( snr.lt.snr2. and. snr.ge.snr1 ) then
        wtobs(jj) = wtobs(jj)+0.5
      elseif ( snr.lt.snr3. and. snr.ge.snr2 ) then
        wtobs(jj) = wtobs(jj)+0.75
      elseif ( snr.ge.snr3 ) then
        wtobs(jj) = wtobs(jj)+1  
      else  
        wtobs(jj) = 0
        xcode(k)(6:6) = 'w'
      endif

      endif
      
c take care of distance weighting for observations with non-zero weight

      if(wtobs(jj). ne. 0) then
c        elseif (dnstrng(wtkey) .eq. 'dist') then
        
          do l = 1,ndistwt
            if(l.eq.1) then
              tmpmin = -1
              tmpmax = wtlim(1)
            else
              tmpmin = wtlim(l-1)
              tmpmax = wtlim(l)
            endif
        
            if (delta .le. tmpmax. and. delta .gt. tmpmin) then
              wtobs(jj) = wtobs(jj)+wterat(l)
              errat = 1
            endif
           
          enddo
         
            if(errat.ne.1) then
              wtobs(jj) = 0
              xcode(k)(4:4) = 'd'
            endif
         
       endif

c count number of usable motions for this event
        if (wtobs(jj).ne.0) nsofar = nsofar + 1

84    continue

c compose prmk variable for ray file, then used by fpplot

      tmpprmk=' p  '
      tmpprmk(3:3)=dbfm(1:1)
      if(wtobs(jj).ge.wtmax) tmpprmk(4:4)='0'
      if(wtobs(jj).ge.wtmax1.and.wtobs(jj).lt.wtmax) tmpprmk(4:4)='0'
      if(wtobs(jj).ge.wtmax2.and.wtobs(jj).lt.wtmax1) tmpprmk(4:4)='1'
      if(wtobs(jj).ge.wtmax3.and.wtobs(jj).lt.wtmax2) tmpprmk(4:4)='2'
      if(wtobs(jj).gt.0.and.wtobs(jj).lt.wtmax3) tmpprmk(4:4)='3'
      if(wtobs(jj).eq.0) tmpprmk(4:4)='4'
      prmk(jj) = tmpprmk

       print *, stn(jj), dist(jj), az(jj), ain(jj), 
     & pobs(jj), pres, deltim, xcode(jj),wtobs(jj)

      enddo
c end loop for arrivals

        iadd = narr

c count total number of arrivals for composite
      kwt = kwt + nsofar
      print*,'For orid = ', iorin,' N usable 1st motions = ', nsofar
      
      if (nsofar .gt. 0) then
        nused = nused+1
        write(sunit,*) event
      endif
      
85    continue
      enddo
c end loop for origins for composite solution

       if(kwt. lt. minobs ) then
        print*,'Number of first motions is less than minimum'
        goto 99
      elseif(kwt .gt. maxobs) then
        nr = 0
        print*,'Number of first motions is greater than maximum'
        goto 99
      else
        nr = kwt
      endif

      write (eunit, 121) kwt, nused, minobs, maxobs
      write (*, 121) kwt, nused, minobs, maxobs
121   format('Used ',i6,' first motions from ',i6,' events with n obs fr
     &om',i6,' to ',i9)
      
      endif
c endif for composite or non-composite choice

c
c finish up with the weights
c
      nrwt = 0
      sumwt = 0
      wtmax = 0

      if( nr .le. 0) return
      do 1002 k = 1, nr
c        jwt = jjwt(k)
c        nclas(jwt) = nclas(jwt) + 1
        if(wtobs(k) .gt. wtmax) wtmax = wtobs(k)
        if(wtobs(k) .ne. 0.) then
          sumwt = sumwt + wtobs(k)
          nrwt = nrwt + 1
        endif
1002  continue
      if(nrwt .eq. 0) then
        print *, 'this will not work! all readings have zero weight.'
        nr = 0
        return
      endif
      sigmaf = sqrt( float(nrwt) )/sumwt
99    return
      end