c---- TEST FUNCTION JLDY
      logical eof
# 3 "jldy.for"
   10 idflt = 1960
      iyear = iaskk(' GIVE YEAR, (0 TO STOP):',idflt,eof)
      if (iyear .eq. 0) stop 
      idflt = 1
      imon = iaskk(' GIVE MONTH:',idflt,eof)
      idflt = 1
      iday = iaskk(' GIVE DAY:',idflt,eof)
      ntime = jldy(iyear,imon,iday)
      write(unit=6, fmt=100) ntime
  100 format(25h THE JULIAN DAY NUMBER IS,i10)
      goto 10
      end
c---- CALCULATES THE JULIAN DAY
      function jldy(jy, jm, jd)
      dimension ms(12)
c---- DONE, UNLESS THIS IS A LEAP YEAR
      data ms / 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /
# 19 "jldy.for"
      jldy = ms(jm) + jd
# 21 "jldy.for"
      if ((jy - ((jy / 4) * 4)) .ne. 0) return 
   10 if (jm .lt. 3) return 
      jldy = jldy + 1
      return 
      end
