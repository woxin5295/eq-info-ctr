	subroutine frommstm(iyr, imo, idy, ihr, imn, sec, mstime)
c --  convert from mstime to yr, mo, dy, hr, mn, sec
	integer minute, hour, day
	logical isleap
	parameter (minute = 60)
	parameter (hour = 60*minute)
	parameter (day = 24*hour)
	real*8 mstime

c -- full days since Jan 1, 1970
	jdy = mstime/day
c -- excess seconds
	excess1 = mstime - day*jdy
c -- full hours
	ihr = excess1/hour
c -- excess seconds
	excess2 = excess1 - hour*ihr
c -- full minutes
	imn = excess2/minute
c -- seconds
	sec = excess2 - minute*imn
	
c -- full years since Jan 1, 1970
	iyr =  1970 + jdy/365
c -- julian day within next year
	ijdy = jdy - (iyr-1970)*365 + 1
c -- allow for the leap years
	ijdy = ijdy - (iyr-1)/4 + 492 + (iyr-1)/100 - 19
     *    - (iyr-1)/400 + 4
	if(ijdy .le. 0) then
	  iyr = iyr -1
	  if(isleap(iyr)) then
	    ijdy = 365 + ijdy
	  else
	    ijdy = 364 + ijdy
	  endif
	endif
        call mnday(ijdy, isleap(iyr), imo, idy)
	
	return
	end

	subroutine tomstime2(iyr, imo, idy, ihr, imn, sec, mstime)
	real*8 mstime
	integer*2 iyr, imo, idy, idy1, ihr, imn
	integer*2 jdn2
	real*4 sec
	logical err, isleap2

	integer*2 eomleap(15), eom(15)
        data eomleap /0, 31, 60, 91, 121, 152, 182, 213, 
     *             244, 274, 305, 335, 366, 397, 425/
        data eom    /0, 31, 59, 90, 120, 151, 181, 212, 
     *             243, 273, 304, 334, 365, 396, 424/

c	integer*2 minute, hour
c corrected bug -- minute and hour must by *4.  jcl 7/8/95
 	integer*4 minute, hour
	integer*4 day
	parameter (minute = 60)
	parameter (hour = 60*minute)
	parameter (day = 24*hour)
c	print *, 'iyr, imo, idy, ihr, imn, sec'
c	print *, iyr, imo, idy, ihr, imn, sec

	err = .false.
	if(iyr .eq. 0) then
	  print *, 'Error, year = ', iyr
	  err = .true.
	endif

	if(imo .eq. 0) then
c --  if month is zero, then day is day_of_year
	  call mnday2(idy, isleap2(iyr), imo, idy1)
	  idy = idy1
c	  print *, 'mnday2 returns mo dy = ', imo, idy
	endif
	
	if((imo .lt. 1) .or. (imo .gt. 12)) then
	  print *, 'Error, month, day can not be = ', imo, idy
	  err = .true.
	endif

	if (isleap2(iyr)) then
	  if ( (idy .lt. 1) .or. 
     *        (idy .gt. (eomleap(imo+1) - eomleap(imo)))) then
	    print *, 'Error isleap, for month # ', 
     *      imo, ' day can not be ', idy
	    err = .true.
	  endif
	else
	  if ((idy .lt. 1) .or. 
     *        (idy .gt. eom(imo+1) - eom(imo))) then
	    print *, 'Error, for month # ', 
     *      imo, ' day can not be ', idy
	    err = .true.
	  endif
	endif

	if (err) then
	  mstime = 0.0d0
	  return
	endif

	mstime = day*jdn2(iyr, imo, idy) + hour*ihr + minute*imn +
     *    sec*1.d0

	return
	end

	integer function jdn(iyr, imo, idy)
c --  compute julian day from iyr, imo, idy

	logical isleap
	integer eomleap(15), eom(15)
        data eomleap /0, 31, 60, 91, 121, 152, 182, 213, 
     *             244, 274, 305, 335, 366, 397, 425/
        data eom    /0, 31, 59, 90, 120, 151, 181, 212, 
     *             243, 273, 304, 334, 365, 396, 424/

	if(.not. isleap(iyr)) then
	  jdn = eom(imo) + idy
	else 
	  jdn = eomleap(imo) + idy
	endif
	jdn = jdn + 365*(iyr - 1970) + 
     I        (iyr-1)/4 - 492 - (iyr-1)/100 + 19 + (iyr-1)/400 - 5 
	return
	end
        integer*2 function jdn2(iyr, imo, idy)
c --  compute julian day from iyr, imo, idy
 
	logical isleap2
        integer*2 eomleap(15), eom(15), iyr, imo, idy
        data eomleap /0, 31, 60, 91, 121, 152, 182, 213,
     *             244, 274, 305, 335, 366, 397, 425/
        data eom    /0, 31, 59, 90, 120, 151, 181, 212,
     *             243, 273, 304, 334, 365, 396, 424/
 
        if(.not. isleap2(iyr)) then
          jdn2 = eom(imo) + idy
        else
          jdn2 = eomleap(imo) + idy
        endif
        jdn2 = jdn2 + 365*(iyr - 1970) +
     I        (iyr-1)/4 - 492 - (iyr-1)/100 + 19 + (iyr-1)/400 - 5
        return
        end


	logical function isleap(yr)
	integer yr
	isleap = .false.
	if (yr .lt. 0) yr = yr + 1
	if (mod(yr, 4) .eq. 0) isleap = .true.
	if (isleap) then
	  if ((mod(yr, 100) .eq. 0) .and. (mod(yr, 400) .ne. 0))
     *    isleap = .false.
	endif
	return
	end
        logical function isleap2(yr)
	integer*2 yr
 
        isleap2 = .false.
        if (yr .lt. 0) yr = yr + 1
        if (mod(yr, 4) .eq. 0) isleap2 = .true.
        if (isleap2) then
          if ((mod(yr, 100) .eq. 0) .and. (mod(yr, 400) .ne. 0))
     *    isleap2 = .false.
        endif
        return
        end


	subroutine mnday(dyofyr, leap, imo, idy)
c compute month, day from day of year
	integer eomleap(15), eom(15), dyofyr
        data eomleap /0, 31, 60, 91, 121, 152, 182, 213, 
     *             244, 274, 305, 335, 366, 397, 425/
        data eom    /0, 31, 59, 90, 120, 151, 181, 212, 
     *             243, 273, 304, 334, 365, 396, 424/
	logical leap
	if (leap) then
	  do 20 i = 1, 15
	    if(eomleap(i) .ge. dyofyr) goto 22
20	  continue
22        imo = i - 1
	  idy = dyofyr - eomleap(i-1)
	  return
	endif
        if (.not. leap) then
          do 30 i = 1, 15
            if(eom(i) .ge. dyofyr) goto 32 
30        continue 
32        imo = i - 1 
          idy = dyofyr - eom(i-1)
          return 
        endif 
	end
        subroutine mnday2(dyofyr, leap, imo, idy)
c compute month, day from day of year
        integer*2 eomleap(15), eom(15), dyofyr, imo, idy
        data eomleap /0, 31, 60, 91, 121, 152, 182, 213,
     *             244, 274, 305, 335, 366, 397, 425/
        data eom    /0, 31, 59, 90, 120, 151, 181, 212,
     *             243, 273, 304, 334, 365, 396, 424/
        logical leap
c	print *, 'day of year = ', dyofyr
        if (leap) then
c	  print *, 'This is a leap year'
          do 20 i = 1, 15
            if(eomleap(i) .ge. dyofyr) goto 22
20        continue
22        imo = i - 1
          idy = dyofyr - eomleap(i-1)
          return
        endif
        if (.not. leap) then
c	  print *, 'This is not a leap year'
          do 30 i = 1, 15
            if(eom(i) .ge. dyofyr) goto 32
30        continue
32        imo = i - 1
          idy = dyofyr - eom(i-1)
c	  print *, 'eom(i), i, > dyofyr ', eom(i), i, dyofyr
          return
        endif
        end

	subroutine tomstime(iyr, imo, idy, ihr, imn, sec, mstime)
	real*8 mstime
	logical err, isleap


	integer eomleap(15), eom(15)
        data eomleap /0, 31, 60, 91, 121, 152, 182, 213, 
     *             244, 274, 305, 335, 366, 397, 425/
        data eom    /0, 31, 59, 90, 120, 151, 181, 212, 
     *             243, 273, 304, 334, 365, 396, 424/

	integer minute, hour, day
	parameter (minute = 60)
	parameter (hour = 60*minute)
	parameter (day = 24*hour)

	err = .false.
	if(iyr .eq. 0) then
	  print *, 'Error, year = ', iyr
	  err = .true.
	endif

	if(imo .eq. 0) then
c --  if month is zero, then day is day_of_year
	  call mnday(idy, isleap(iyr), imo, idy1)
	  idy = idy1
	endif
	
	if((imo .lt. 1) .or. (imo .gt. 12)) then
	  print *, 'Error, month, day can not be = ', imo, idy
	  err = .true.
	endif

	if (isleap(iyr)) then
	  if ( (idy .lt. 1) .or. 
     *        (idy .gt. (eomleap(imo+1) - eomleap(imo)))) then
	    print *, 'Error, for month # ', 
     *      imo, ' day can not be ', idy
	    err = .true.
	  endif
	else
	  if ((idy .lt. 1) .or. 
     *        (idy .gt. eom(imo+1) - eom(imo))) then
	    print *, 'Error, for month # ', 
     *      imo, ' day can not be ', idy
	    err = .true.
	  endif
	endif

	if (err) then
	  mstime = 0.0d0
	  return
	endif

	mstime = day*jdn(iyr, imo, idy) + hour*ihr + minute*imn +
     *    sec*1.d0

	return
	end

