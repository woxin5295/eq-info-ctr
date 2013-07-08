	subroutine s1960_date(elday, yr, mo, dy)
c converts from the days elapsed since january 1, 1960 to date
c j.c. lahr
	dimension ms(0:12), msly(0:12)
	integer elday, yr, dy
        data ms/0,31,59,90,120,151,181,212,243,273,304,334,365/
        data msly/0,31,60,91,121,152,182,213,244,274,305,335,366/

        if(elday .lt. 0) then
	  print *, 's1960_date elapsed days = ', elday
          print *, 'This algorighm does not work prior to'
          print *, 'January 1, 1960'
          stop
        endif

	nyr = float(elday)/365.25
	yr = 1960 + nyr
c this it ok until 2100, as the next special year is 2000,
c  which is both a 100-year year and a 400-year year.
	jday = elday - nyr*365 - (nyr+3)/4 

	if(mod(yr,4) .eq. 0) then
c leap year
	  do 20 i = 1, 12
	    if(jday .lt. msly(i)) then
	      mo = i
	      dy = jday  + 1 - msly(i-1)
	      goto 30
	    endif
20	  continue
	  print *, 'this should not happen!'
	else
c not a leap year
	  do 25 i = 1, 12
	    if(jday .lt. ms(i)) then
	      mo = i
	      dy = jday + 1 - ms(i-1)
	      goto 30
	    endif
25	  continue
	  print *, 'this should not happen!'
	endif
30      return
	end
	
