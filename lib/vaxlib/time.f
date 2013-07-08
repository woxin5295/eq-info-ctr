	integer function tosecs(iyr, imo, ida, ihr, imn, isc)

c -- compute seconds since January 1, 1970 00:00:00

	integer*4 tosecs
	integer*4 mosec(12), npyr, npdy

	data mosec/2678400, 2419200, 2678400, 2592000, 2678400, 2592000,
     1	           2678400, 2678400, 2592000, 2678400, 2592000, 2678400/
	data npyr/31536000/
	data npdy/86400/
	data nphr/3600/
	data npmn/60/

	jyr = iyr
	if (jyr .lt. 1900) jyr = jyr + 1900
	tosecs = isc + imn*npmn + ihr*nphr + (ida-1)*npdy
	i = 0
	do while (i .lt. imo-1)
	  i = i + 1
	  tosecs = tosecs + mosec(i)
	enddo
	if (imo .gt. 2 .and. mod(jyr,4) .eq. 0)
     1		tosecs = tosecs + npdy
	tosecs = tosecs + (jyr-1970)*npyr
     1		+ ((jyr-1970+1)/4)*npdy

	return
	end

c =============================================================================

	subroutine cvtm(nsec, iyr, imo, idy, ihr, imn, isc)

c  convert seconds since the beginning of 1970 to date and time
c
c  largest nsec = 2**31-1 = 2147483647 --> 03:14:07 jan 19 2038
c
c  written by c. stephens  05/11/90

	dimension mosec(12)
	data mosec/2678400, 2419200, 2678400, 2592000, 2678400, 2592000,
     1	           2678400, 2678400, 2592000, 2678400, 2592000, 2678400/
	data npyr/31536000/
	data npdy/86400/
	data nphr/3600/
	data npmn/60/

	left = nsec
	imo = 1
	idy = 1
	ihr = 0
	imn = 0
	isc = 0

c  year
	iyr = 1969
10	iyr = iyr + 1
	isecs = npyr
	if (mod(iyr,4) .eq. 0) isecs = isecs + npdy
	left = left - isecs
	if (left .gt. 0) go to 10
	if (left .eq. 0) then
	  iyr = iyr + 1
	  return
	endif
	left = left + isecs
	
c  month
	leap = 0
	if (mod(iyr,4) .eq. 0) leap = npdy
	do while (imo .lt. 12)
	  isecs = mosec(imo)
	  if (imo .eq. 2) isecs = isecs + leap
	  left = left - isecs
	  if (left .lt. 0) then
	    left = left + isecs
	    go to 30
	  else if (left .eq. 0) then
	    imo = imo + 1
	    return
	  endif
	  imo = imo + 1
	enddo

c  day

30	do while (idy .lt. 31)
	  left = left - npdy
	  if (left .lt. 0) then
	    left = left + npdy
	    go to 40
	  else if (left .eq. 0) then
	    idy = idy + 1
	    return
	  endif
	  idy = idy + 1
	enddo

c  hour

40	do while (ihr .lt. 23)
	  left = left - nphr
	  if (left .lt. 0) then
	    left = left + nphr
	    go to 50
	  else if (left .eq. 0) then
	    ihr = ihr + 1
	    return
	  endif
	  ihr = ihr + 1
	enddo

c  minute

50	imn = 0
	do while (imn .lt. 59)
	  left = left - npmn
	  if (left .lt. 0) then
	    isc = left + npmn
	    return
	  else if (left .eq. 0) then
	    imn = imn + 1
	    isc = 0
	    return
	  endif
	  imn = imn + 1
	enddo

	isc = left
	return

	end
