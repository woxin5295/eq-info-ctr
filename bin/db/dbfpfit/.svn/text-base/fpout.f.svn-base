      subroutine fpout (ddelf, dlamf, dphif, erate, eunit, ievp, ievr,
     & ind, ires, mxdip, mxqual, mxrake, mxstat, mxstrk, ndelf, ndrng,
     & nfit, nlamf, nphif, nrev, nrrng, nsrng, nstat, qcnt, qcntwt,
     & revsta, scnt, scntwt, stat, wterat, wtkey, wtlim)
c
c generate summary listing of polarity discrepancies as a function of station and quality, the distribution of 
c fit parameters, and the distribution of dip, strike, and rake ranges about best fit solution
c
      real              ddelf         ! (input) fault dip increment in degrees for fine search
      real              dlamf         ! (input) fault rake increment in degrees for fine search
      real              dphif         ! (input) fault strike increment in degrees for fine search
      real              erate(mxqual) ! (input) estimated weighted error rates
      integer           eunit         ! (input) logical unit # of output of error messages
      integer           ievp          ! (input) # of events processed
      integer           ievr          ! (input) # of events read
      integer           ind(mxstat)   ! (input) pointer array to sorted order
      integer           ires          ! (input) flag: 0(1)=(un)restricted search
      integer           mxdip         ! (input) maximum # of dip increments permitted
      integer           mxqual        ! (input) maximum # of qualities permitted
      integer           mxrake        ! (input) maximum # of rake increments permitted
      integer           mxstat        ! (input) maximum # of stations permitted
      integer           mxstrk        ! (input) maximum # of strike increments permitted
      integer           ndelf         ! (input) # of fault dip increments for fine search
      integer           ndrng(mxdip)  ! (input) # of dip solution ranges binned into ddelf degree increments
      integer           nfit(20)      ! (input) # of solutions binned into .025 fit increments
      integer           nlamf         ! (input) # of fault rake increments for fine search
      integer           nphif         ! (input) # of fault strike increments for fine search
      integer           nrev          ! (input) # of reversed stations
      integer           nrrng(mxrake) ! (input) # of rake solution ranges binned into dlamf degree increments
      integer           nsrng(mxstrk) ! (input) # of strike solution ranges binned into dphif degree increments
      integer           nstat         ! (input) total # of stations reporting for entire data set
      integer           qcnt(mxqual,2)   ! (input) indx 1=# of dscrpnt 
c                                          plrties for qlity, indx 2=
c                                          # of observations
      real              qcntwt(mxqual,2) ! (input) indx 1=weighted # 
c                                          dscrpnt plrties for qlity, 
c                                          indx 2=sum of weights
      character*4       revsta(mxstat)   ! (input) reversed station names
      integer           scnt(mxstat,2)   ! (input) index 1=# of dscrpnt
c                                           polarities for stat, index 
c                                           2=# of obsrvations
      real              scntwt(mxstat,2) ! (input) indx 1=weighted # 
c                                          dscrpnt polrties for stat, 
c                                          indx 2=sum of weights
      character*4       stat(mxstat)     ! (input) names of all stations reporting
      real              wterat(mxqual)   ! (input) alternate error rate
      character*4       wtkey            ! (input) alternate basis for weighting
      real              wtlim(mxqual)    ! (input) alternate variable limits
c
      character*2       estar            ! flag indicates large discrepancy between actual and estimated error rates
      integer           i                ! loop index
      integer           j                ! dummy variable
      integer           natot            ! sum of # of polarities in agreement with solution
      integer           ndtot            ! sum of # of polarities in discrepancy with solution
      integer           ntot             ! total # of observations
      real              rate             ! weighted error rate per quality class 
      character*1       star             ! denotes station designated as reversed
      real              wer              ! weighted error rate
      real              wtot             ! summation of weights over 
c                                          all stations
c
      ndtot = 0
      ntot = 0
      wtot = 0.
      print *, 'subroutine fpout.  nstat = ', nstat
      do 5 i = 1, nstat
        ndtot = ndtot + scnt(i, 1)
        ntot = ntot + scnt(i, 2)
        wtot = wtot + scntwt(i, 2)
5     continue
      natot = ntot - ndtot
c
c write out summary of polarity discrepancies by station
c
      write (eunit, 10)
10    format ('0', 'summary of stations having polarities in discrepancy
     & with best fit solution (* denotes reversed station)', /,
     & ' station     discrepancies    agreements       total  
     &weighted error rate   total error contribution', /)
c
c sort stations alphabetically
c
      call csort (stat, ind, nstat)
      do 40 i = 1, nstat
        j = ind(i)
        star = ' '
        do 20 k = 1, nrev
          if (stat(j) .eq. revsta(k)) star = '*'
20      continue
        if (scntwt(j, 2) .eq. 0.) then
          wer = 0.
        else
          wer = scntwt(j, 1)/scntwt(j, 2)
        endif
        write (eunit, 30) star, stat(j), scnt(j, 1), scnt(j, 2) -
     & scnt(j, 1), scnt(j, 2), wer, scntwt(j, 1)/wtot
30      format (' ', a1, a4, 3(10x, i5), 9x, f6.3, 10x, f6.4)
40    continue
      write (eunit, 50) ndtot, natot, ntot
50    format ('0', 'total', 3(10x, i5))
c
c write out summary of hand-picked polarity discrepancies by reading quality
c

      if(wtkey .eq. '    ') then
        write (eunit, 60) 
60      format (' the followning summary is based on weight codes')
      else
        write (eunit, 61) wtkey
61      format (' the followning summary is based on ', a4)
      endif
      ndtot = 0
      ntot = 0
      if(wtkey .eq. '    ') then
        write (eunit, 62)
62      format ('0', 'summary of hand-picked data with respect to',
     &  ' best fit solutions', /,
     &  '                                                         ',
     &  'weighted ', /,
     &  ' class       discrepancies    agreements       total     ',
     &  'error rate  ')
        do 90 i = 1, mxqual/2
          estar = '  '
          ndtot = ndtot + qcnt(i, 1)
          ntot = ntot + qcnt(i, 2)
          if (qcntwt(i, 2) .eq. 0.) then
            rate = 0.
          else
            rate = qcntwt(i, 1)/qcntwt(i, 2)
            if (rate .ge. 0.0001) then
              if (abs((erate(i)-rate)/rate) .ge. 0.2) estar = '**'
            end if
          end if
          write (eunit, 80) i - 1, qcnt(i, 1), qcnt(i, 2) -
     &    qcnt(i, 1), qcnt(i, 2), rate, estar, erate(i)
80        format (' ', 2x, i1, 2x, 3(10x, i5), 9x, f6.4, 1x, a3, 1x,
     &    f6.4)
90      continue
      endif
      if(wtkey .ne. '    ') then
        write (eunit, 91)
91      format ('0', 'summary of hand-picked data with respect to', 
     &  ' best fit solutions', /,
     &  '                                                         ',
     &  'weighted      assumed    lower   upper', /,
     &  ' class       discrepancies    agreements       total     ',
     &  'error rate   error rate  limit   limit')
        do 95 i = 1, mxqual/2
          estar = '  '
          ndtot = ndtot + qcnt(i, 1)
          ntot = ntot + qcnt(i, 2)
          if (qcntwt(i, 2) .eq. 0.) then
            rate = 0.
          else
            rate = qcntwt(i, 1)/qcntwt(i, 2)
            if (rate .ge. 0.0001) then
              if (abs((wterat(i)-rate)/rate) .ge. 0.2) estar = '**'
            end if
          end if
          write (eunit, 94) i - 1, qcnt(i, 1), qcnt(i, 2) -
     &    qcnt(i, 1), qcnt(i, 2), rate, estar, wterat(i), wtlim(i), 
     &    wtlim(i+1)
94        format (' ', 2x, i1, 2x, 3(10x, i5), 9x, f6.4, 1x, a3, 1x,
     &    f6.4, f10.2, f8.2)
95      continue
      endif
      natot = ntot - ndtot
      write (eunit, 50) ndtot, natot, ntot
c
c write out summary of machine-picked polarity discrepancies by reading quality
c
      ndtot = 0
      ntot = 0
      if(wtkey .eq. '    ') then
        write (eunit, 96)
96      format ('0', 'summary of machine-picked data with respect to',
     &  ' best fit solutions', /,
     &  '                                                         ',
     &  'weighted ', /,
     &  ' class       discrepancies    agreements       total     ',
     &  'error rate  ')
        do 120 i = mxqual/2 + 1, mxqual
          estar =  '  '
          ndtot = ndtot + qcnt(i, 1)
          ntot = ntot + qcnt(i, 2)
          if (qcntwt(i, 2) .eq. 0.) then
            rate = 0.
          else
            rate = qcntwt(i, 1)/qcntwt(i, 2)
            if (rate .ge. 0.0001) then
              if (abs((erate(i)-rate)/rate) .ge. 0.2)  estar = '**'
            end if
          end if
          write (eunit, 80) i - mxqual/2 - 1, qcnt(i, 1), qcnt(i, 2) -
     &    qcnt(i, 1), qcnt(i, 2), rate, estar, erate(i)
120     continue
      endif
      if(wtkey .ne. '    ') then
        write (eunit, 121)
121     format ('0', 'summary of machine-picked data with respect to',
     &  'best fit solutions', /,
     &  '                                                         ',
     &  'weighted      assumed    lower   upper', /,
     &  ' class       discrepancies    agreements       total     ',
     &  'error rate   error rate  limit   limit')
        do 122 i = mxqual/2 + 1, mxqual
          estar =  '  '
          ndtot = ndtot + qcnt(i, 1)
          ntot = ntot + qcnt(i, 2)
          if (qcntwt(i, 2) .eq. 0.) then
            rate = 0.
          else
            rate = qcntwt(i, 1)/qcntwt(i, 2)
            if (rate .ge. 0.0001) then
              if (abs((wterat(i)-rate)/rate) .ge. 0.2)  estar = '**'
            end if
          end if
          write (eunit, 94) i - mxqual/2 - 1, qcnt(i, 1), qcnt(i, 2) -
     &    qcnt(i, 1), qcnt(i, 2), rate, estar, wterat(i), 
     *    wtlim(i - mxqual/2), wtlim(i + 1 - mxqual/2)
122     continue
      endif
      natot = ntot - ndtot
      write (eunit, 50) ndtot, natot, ntot
c
c write out distribution of fit parameters
c
      write (eunit, 130)
130   format ('0', 'distribution of solution misfit scores', /,
     & 1x, ' misfit score     num    cum num')
      ncfit = 0
      do 150 i = 1, 20
        ncfit = ncfit + nfit(i)
        write (eunit, 140) float(i - 1)*.025, float(i)*.025, nfit(i),
     1                     ncfit
140     format (' ', f5.3, ' - ', f5.3, 3x, i5, 3x, i6)
150   continue
c
c write out distribution of dip, strike, rake ranges for unrestricted searches
c
        write (eunit, 160)
160     format ('0', 'distribution of solution dip ranges', /,
     & 1x, ' range       num    cum num')
        ndcrng = 0
      if (ndelf .gt. 0) then
        do 180 i = 1, ndelf
          ndcrng = ndcrng + ndrng(i)
          write (eunit, 170) float(i - 1)*ddelf, ndrng(i), ndcrng
170       format (' ', 1x, f5.1, 5x, i5, 3x, i6)
180     continue
        write (eunit, 190)
190     format ('0', 'distribution of solution strike ranges', /,
     & 1x, ' range       num    cum num')
        nscrng = 0
      endif
      if (nphif .gt. 0) then
        do 200 i = 1, nphif
          nscrng = nscrng + nsrng(i)
          write (eunit, 170) float(i - 1)*dphif, nsrng(i), nscrng
200     continue
        write (eunit, 210)
210     format ('0', 'distribution of solution rake ranges', /,
     & 1x, ' range       num    cum num')
        nrcrng = 0
      endif
      if (nlamf .gt. 0) then
        do 220 i = 1, nlamf
          nrcrng = nrcrng + nrrng(i)
          write (eunit, 170) float(i - 1)*dlamf, nrrng(i), nrcrng
220     continue
      endif
c
      write (eunit, *) ' '
      write (eunit, *) ievr, ' events read, ', ievp, ' processed'
c
      return
      end
c
c
c
c

