      logical function fpinp ( auth, ddelc, ddelf, del0c, dfitc, distmx, 
     & dlamc, dlamf, dphic, dphif, erate, eunit, fmagmn, iamwt, ijeff, 
     & iprnt, isnrwt, kilsta, maxobs, minobs, mxdip,mxqual, mxrake, 
     & mxstat, mxstrk, ndelc, ndelf, ndistwt, nev, nkil, nlamc, nlamf, 
     & nphic, nphif, nqualwt, nrev, pf, phi0c, plusert, presmx, revsta, 
     & weight, wterat, wtkey, wtlim, wtqlim, xainmn, xainmx, xlam0c )
c read in control parameters
c ignore records beginning with an ! mark
c
      integer           pf  ! (input) logical unit # of input parameter file
      real*8		afitf  ! (output) sets fit increment for fine search display of p&t axes
      real              ddelc  ! (output) fault dip increment in degrees for coarse search
      real*8            ddelf  ! (output) fault dip increment in degrees for fine search
      real              del0c  ! (output) initial fault dip angle in degrees for coarse search
      real*8            dfitc  ! (output) increment to coarse fit function
      real*8 distmx
      real              dlamc  ! (output) fault rake increment in degrees for coarse search
      real*8            dlamf  ! (output) fault rake increment in degrees for fine search
      real              dphic  ! (output) fault strike increment in degrees for coarse search
      real*8            dphif  ! (output) fault strike increment in degrees for fine search
      real              erate(mxqual)    ! (output) assumed weighted error rates for data, read from control card
      integer           eunit  ! (input) logical unit # of output of error messages
      real*8            fmagmn ! (output) minimum permitted magnitude
      integer           iamwt  ! (output) flag controling amplitude weighting (0=omit, 1=use)
      integer           isnrwt ! (output) flag controling signal-to-noise weighting (0=omit, 1=use)
      integer           ijeff  ! (output) flag: 1(0)=do (not) use data weighted out by jeffrey's weighting
      integer           iprnt  ! (output) flag: 1(0)=do (not) print out fit parameters
      character*4       kilsta(mxstat)   ! (output) ignored station names
      integer           maxobs ! (output) maximum number of observations 
      integer           minobs ! (output) minimum number of observations required 
      integer           mxdip  ! (input) maximum # of dip increments permitted
      integer           mxqual ! (input) maximum # of qualities permitted
      integer           mxrake ! (input) maximum # of rake increments permitted
      integer           mxstat ! (input) maximum # of stations permitted
      integer           mxstrk ! (input) maximum # of strike increments permitted
      integer           ndelc  ! (output) number of fault dip increments for coarse search
      integer           ndelf  ! (output) default number of fault dip increments for fine search
      integer           nev    ! (output) maximum number of events to process
      integer           nkil   ! (output) number of ignored stations
      integer           nlamc  ! (output) number of fault rake increments for coarse search
      integer           nlamf  ! (output) default number of fault rake increments for fine search
      integer           nphic  ! (output) number of fault strike increments for coarse search
      integer           nphif  ! (output) default number of fault strike increments for fine search
      integer           nrev   ! (output) number of reversed stations
      real              phi0c  ! (output) initial fault strike angle in degrees for coarse search
      real*8            plusert ! (output) error rate for + or -
      real*8            presmx ! (output) maximum abs value of permitted p-residual
      character*4       revsta(mxstat)  ! (output) reversed station names
      real              weight(mxqual)  ! (output) weights associated with qualities
      real              wtqlim(mxqual)  ! (output) limits of quality weighting
      real              xlam0c ! (output) initial fault rake angle in degrees for coarse search
      real*8            xainmn ! (output) minimum ang of inc
      real*8            xainmx ! (output) maximum ang of inc
c
      real              ddlcdf ! default fault dip increment in degrees for coarse search
      real              ddlfdf ! default fault dip increment in degrees for fine search
      real              dl0cdf ! default initial fault dip angle in degrees for coarse search
      real              dlmcdf ! default fault rake increment in degrees for fine search
      real              dlmfdf ! default fault rake increment in degrees for fine search
      real              dphcdf ! default fault strike increment in degrees for coarse search
      real              dphfdf ! default fault strike increment in degrees for fine search
      integer           i      ! dummy loop index
      real              lm0cdf ! default initial fault rake angle in degrees for coarse search
      integer           ndlcdf ! default number of fault dip increments for coarse search
      integer           ndlfdf ! default number of fault dip increments for fine search
      integer           nlmcdf ! default number of fault rake increments for coarse search
      integer           nlmfdf ! default number of fault rake increments for fine search
      integer           nphcdf ! default number of fault strike increments for coarse search
      integer           nphfdf ! default number of fault strike increments for fine search
      real              ph0cdf ! default initial fault strike angle in degrees for coarse search
      real              wterat(mxqual) ! (output) error rate for alternate weighting
      character*4       wtkey  ! (output) key to alternate weights - dist, azim angi
      real              wtlim(mxqual)  ! (output) limits of alternate variable

      integer           ndistwt     ! # of distance weights
      integer           nqualwt     ! # of quality weights
      character*15      auth

      character*60 strdistwt(mxqual), strqualwt(mxqual)
      integer idistwt, iqualwt, ikilsta, irevsta
c     Mitch Max OS X string to C interface variables
      character*12 ch_max_distance / 'max_distance' /
      character*13 ch_min_magnitude / 'min_magnitude' /
      character*7 ch_min_obs / 'min_obs' /
      character*6 ch_iprint / 'iprint' /
      character*10 ch_max_events / 'max_events' /
      character*12 ch_depth_relmin / 'depth_relmin' /
      character*14 ch_quality_weight / 'quality_weight' /
      character*10 ch_snr_weight / 'snr_weight' /
      character*11 ch_ampl_weight / 'ampl_weight' /
      character*8 ch_max_pres / 'max_pres' /
      character*7 ch_max_obs / 'max_obs' /
      character*7 ch_min_ain / 'min_ain' /
      character*7 ch_max_ain / 'max_ain' /
      character*11 ch_fine_search / 'fine_search' /
      character*13 ch_plus_err_rate / 'plus_err_rate' /
      character*9 ch_fm_author / 'fm_author' /
      character*11 ch_qual_weight / 'qual_weight' /
      character*11 ch_dist_weight / 'dist_weight' /
      character*17 ch_reversed_stations / 'reversed_stations' /
      character*15 ch_ignore_stations / 'ignore_stations' /
      character*12 ch_nstrinc_fine / 'nstrinc_fine' /
      character*11 ch_strinc_fine / 'strinc_fine' /
      character*12 ch_ndipinc_fine / 'ndipinc_fine' /
      character*11 ch_dipinc_fine / 'dipinc_fine' /
      character*11 ch_nrkinc_fine / 'nrkinc_fine' /
      character*10 ch_rkinc_fine / 'rkinc_fine' /
c
c set up default grid spacing
c
      parameter (ph0cdf =  0., dl0cdf = 10., lm0cdf = -180.)
      parameter (dphcdf = 20., ddlcdf = 20., dlmcdf =   20.)
      parameter (nphcdf =   9, ndlcdf =   5, nlmcdf =    18)
      parameter (dphfdf =  5., ddlfdf =  5., dlmfdf =   10.)
      parameter (nphfdf =  19, ndlfdf =  19, nlmfdf =     7)
c
c set up default values
c
c      print*,'made it to fpinp.f'

      distmx = 10. 
      fmagmn = 0. 
      minobs = 10 
      ijeff = 0
      iprnt = 0 
      nev = 2000 
      dfitc = 0.05 
      iamwt = 0 
      presmx = 3. 
      maxobs = 1000 
      xainmn = 180. 
      xainmx = 181. 
      afitf = -99. 
      plusert = .5

      fpinp = .true.

      wtkey = '    '

      call pfget_double(pf,ch_max_distance, distmx)
      call pfget_double(pf,ch_min_magnitude, fmagmn)
      call pfget_int(pf,ch_min_obs, minobs)
      call pfget_int(pf,ch_iprint, iprnt)
      call pfget_int(pf,ch_max_events, nev)
      call pfget_double(pf,ch_depth_relmin, dfitc)
      call pfget_int(pf,ch_quality_weight, ijeff)
      call pfget_int(pf,ch_snr_weight, isnrwt)
      call pfget_int(pf,ch_ampl_weight, iamwt)
      call pfget_double(pf,ch_max_pres, presmx)
      call pfget_int(pf,ch_max_obs, maxobs)
      call pfget_double(pf,ch_min_ain, xainmn)
      call pfget_double(pf,ch_max_ain, xainmx)
      call pfget_double(pf,ch_fine_search, afitf)
      call pfget_double(pf,ch_plus_err_rate, plusert)
      call pfget_string(pf,ch_fm_author, auth)

c      print *,distmx,fmagmn,minobs,iprnt,nev,dfitc,ijeff,
c     &isnrwt,iamwt,presmx,maxobs,xainmn,xainmx,afitf,plusert
c
c check parameters
c
      if (isnrwt .ne. 0. and. ijeff. ne. 0) then
        write (eunit, *) '***** fpinp error: both quality_weight and snr
     &_weight > 0, should be only one *****'
        fpinp = .false.
      end if
      if (distmx .le. 0.) then
        write (eunit, *) '***** fpinp error: distmx .le. 0 *****'
        fpinp = .false.
      end if
      if (iprnt .ne. 0 .and. iprnt .ne. 1) then
        write (eunit, *) '***** fpinp error: iprnt .ne. 0 or 1 *****'
        fpinp = .false.
      end if
      if (minobs .lt. 0) then
        write (eunit, *) '***** fpinp error: minobs less than 0 *****'
        fpinp = .false.
      end if
      if (maxobs .eq. 0) then
        maxobs = 9999
      endif
      if (nev .le. 0) then
        write (eunit, *) '***** fpinp error: nev .le. than 0 *****'
        fpinp = .false.
      end if
      if (dfitc .lt. 0) then
        write (eunit, *) '***** fpinp error: dfitc .lt. than 0 *****'
        fpinp = .false.
      else if (dfitc .ge.  0.25) then
        write (eunit, *) '***** warning: dfitc truncated to 0.25 *****'
      end if
      if (iamwt .ne. 1 .and. iamwt .ne. 0) then
        write (eunit, *) '***** fpinp error: iamwt .ne. 0 or 1 *****'
        fpinp = .false.
      end if
      if (presmx .le. 0.) then
        write (eunit, *) '***** fpinp error: presmx = ', presmx, ', whic
     &h is .le. 0 *****'
        fpinp = .false.
      end if
      if (plusert.gt.1. or. plusert.lt.0) then
        write (eunit, *) '***** fpinp error: plusert = ', plusert, ', bu
     &t should be <=1 or >=0 *****'
        fpinp = .false.
      end if

c
c  read in quality weighting assignments
c

      if (ijeff .ne. 0) then
        call pfget_tbl(pf,ch_qual_weight,iqualwt)
        nqualwt = maxtbl(iqualwt)
c        print*,nqualwt
      
        if(nqualwt.eq.0) then
        call die(0,'No quality weight assignments in parameter file')
      
        elseif(nqualwt.gt.mxqual) then
        call die(0,'Change number of quality weigths to less than 9')

        else
        print*,'Quality weights will be used as follows'     
          do j = 0,nqualwt-1
          call strgettbl(iqualwt,j,strqualwt(j+1))
          read(strqualwt(j+1),*) wtqlim(j+1),erate(j+1)
          print*,'deltim = ',wtqlim(j+1),' sec, weight = ', erate(j+1)
          enddo
        endif
        
      else

        nqualwt = 0
        print*,'Quality weights will not be used'  

      endif   
      

c      print*, 'nqualwt = ', nqualwt
c
c  read in distance weighting assignments
c

      call pfget_tbl(pf,ch_dist_weight,idistwt)
      wtkey = 'dist'
      ndistwt = maxtbl(idistwt)
c      print*,ndistwt
      
      if(ndistwt.eq.0) then
      wtkey = '    '
      print*,'No distance weighting will be applied - no weights in par
     &ameter file'

      elseif(ndistwt.gt.mxqual) then
      call die(0,'Change number of distance weigths to less than 9')
      else
        print*,'Distance weight will be applied as follows:'
        do j = 0,ndistwt-1
        call strgettbl(idistwt,j,strdistwt(j+1))
        read(strdistwt(j+1),*) wtlim(j+1),wterat(j+1)
        print*,'dist = ',wtlim(j+1),' deg, weight = ', wterat(j+1)
        enddo
      endif

c      print*, 'ndistwt = ', ndistwt

c
c  read in list of reversed, ignored stations
c
      
c      print*,'mxstat = ',mxstat
      call pfget_tbl(pf,ch_reversed_stations,irevsta)
      nrev = maxtbl(irevsta)
      print*,'Number of reversed stations in the parameter file - ',nrev
            if (nrev .gt. mxstat) then
              write (eunit, *) '***** fpinp error: # of reversed station
     &s exceeds ', mxstat, ' *****'
              fpinp = .false.
            end if
      if (nrev .gt. 0) then
      do j = 0,nrev-1
      call strgettbl(irevsta,j,revsta(j+1))
c      print*,revsta(j+1)
      enddo
      end if

      call pfget_tbl(pf,ch_ignore_stations,ikilsta)
      nkil = maxtbl(ikilsta)
      print*,'Number of ignored stations in the parameter file - ',nkil
            if (nkil .gt. mxstat) then
              write (eunit, *) '***** fpinp error: # of ignored stations
     & exceeds ', mxstat, ' *****'
              fpinp = .false.
            end if
      if (nkil .gt. 0) then
      do j = 0,nkil-1
      call strgettbl(ikilsta,j,kilsta(j+1))
      print*,kilsta(j+1)
      enddo
      end if

c
c  set up default values for fine search
c
      nphif = 23
      dphif = 4.
      ndelf = 23
      ddelf = 4.
      nlamf = 46
      dlamf = 4.
      
c  read in fine grid search parameters

      call pfget_int(pf,ch_nstrinc_fine, nphif)
      call pfget_double(pf,ch_strinc_fine, dphif)
      call pfget_int(pf,ch_ndipinc_fine, ndelf)
      call pfget_double(pf,ch_dipinc_fine, ddelf)
      call pfget_int(pf,ch_nrkinc_fine, nlamf)
      call pfget_double(pf,ch_rkinc_fine, dlamf)
c      print*,'Fine search parameters - ',
c     *nphif, dphif, ndelf, ddelf, nlamf, dlamf
c
c  check fine grid search parameters
c
      if (dphif .lt. 0.) then
        write (eunit, 60) 'dphif'
        fpinp = .false.
      end if
      if (ddelf .lt. 0.) then
        write (eunit, 60) 'ddelf'
        fpinp = .false.
      end if
      if (dlamf .lt. 0.) then
        write (eunit, 60) 'dlamf'
        fpinp = .false.
      end if
      if (nphif .gt. mxstrk) then
        write (eunit, 62) 'nphif', nphif, mxstrk
        fpinp = .false.
      end if
      if (ndelf .gt. mxdip) then
        write (eunit, 62) 'ndelf', ndelf, mxdip
        fpinp = .false.
      end if
      if (nlamf .gt. mxrake) then
        write (eunit, 62) 'nlamf', nlamf, mxrake
      end if
60      format (' ', '***** fpinp error: ',a5,
     1   ' must be greater than 0 ',i5)
62      format (' ', '***** fpinp error: ',a5,' = ',i5,';'/
     1   '  must not exceed ',i5)

c  unrestricted search area - no coarse limits set in control file

      phi0c = ph0cdf
      dphic = dphcdf
      phi1 = ph0cdf + (nphcdf - 1)*dphcdf
      nphic = nphcdf
      del0c = dl0cdf
      ddelc = ddlcdf
      del1 = dl0cdf + (ndlcdf - 1)*ddlcdf
      ndelc = ndlcdf
      xlam0c = lm0cdf
      dlamc = dlmcdf
      xlam1 = lm0cdf + (nlmcdf - 1)*dlmcdf
      nlamc = nlmcdf

c write out parameters

      write (eunit, *) 'maximum epicentral distance = ', distmx
      write (eunit, *) 'minimum magnitude = ', fmagmn
      write (eunit, *) 'minimum # observations = ', minobs
      write (eunit, *) 'maximum # observations = ', maxobs
        if (iprnt .eq. 1) then
          write (eunit, *) 'parameter fit file generated (iprnt = 1)'
        else
          write (eunit, *) 'parameter fit file suppressed (iprnt = 0)'
        end if
      write (eunit, *) 'up to ', nev, '  events processed'
      write (eunit, *) 'misfit range for relative minima in coarse sea
     &rch = ',dfitc
        if (iamwt .eq. 0) then
          write (eunit, *) 'amplitude weighting omitted (iamwt = 0)'
        else if (iamwt .eq. 1) then 
          write (eunit, *) 'amplitude weighting used (iamwt = 1)'
        end if
        if (ijeff .eq. 0) then
          write (eunit, *) 'quality weighting omitted (ijeff = 0)'
        else
          write  (eunit, 122)  (wtqlim(j), j=1,4), (erate(j),j =1,4)
122       format ('0', 'weighting based on quality', /,
     &    ' limit (sec):  ', 4f10.2, /,
     &    ' weight:       ', 4f10.2)
        endif
      write  (eunit, 121)  (wtlim(j), j=1,4), (wterat(j),j =1,4)
121   format ('0', 'weighting based on distance', /,
     &    ' limit (deg):  ', 4f10.2, /,
     &    ' weight:       ', 4f10.2)
        if (nkil .gt. 0) then
          write (eunit, *) 'the following stations ignored in fault-plan
     &e calculations'
          do 130 i = 1, nkil
            write (eunit, *) kilsta(i)
130       continue
        end if
          write (eunit, 170) 
170       format ('0', 'unrestricted search range:         coarse search
     &                        fine search')
          write (eunit, 180)
180       format (' ', t28,
     &'start     end   incrmnt  # incrmnts       incrmnt  # incrmnts',
     & /, t28, 
     & '-----------------------------------       -------------------')
          write (eunit, 160) 'strike: ', phi0c, phi1, dphic, nphic,
     & dphif, nphif
          write (eunit, 160) '   dip: ', del0c, del1, ddelc, ndelc,
     & ddelf, ndelf
          write (eunit, 160) '  rake:  ', xlam0c, xlam1, dlamc, nlamc,
     & dlamf, nlamf
160     format(' ', t19, a8, 3(f6.1, 2x), 9x, i2, 9x, f6.1, 9x, i2)
        write (eunit, 190)
190     format ('0')

      print*,'fpinp: nqualwt = ',nqualwt,' ndistwt = ',ndistwt

      return
      end
      
