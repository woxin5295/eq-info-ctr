c---- TEST FUNCTION JUL1800
      logical eof
# 3 "jul1800.for"
   10 iyear = iaskk(' GIVE YEAR, (0 TO STOP):',1800,eof)
      if (iyear .eq. 0) stop 
      imon = iaskk(' GIVE MONTH:',1,eof)
      iday = iaskk(' GIVE DAY:',1,eof)
      ntime = jul1800(iyear,imon,iday)
      write(unit=6, fmt=100) ntime
  100 format(45h THE NUMBER OF DAYS SINCE JANUARY 1, 1800 IS ,i10)
      goto 10
      end
c---- CALCULATES THE NUMBER OF DAYS ELAPSED SINCE JANUARY 1, 1800
      function jul1800(jy, jm, jd)
      dimension ms(12)
      data ms / 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /
# 16 "jul1800.for"
      jul1800 = ((((((365 * (jy - 1800)) + (jy / 4)) - 437) + ms(jm)) + 
     &jd) - (jy / 100)) + (jy / 400)
      if ((jy - ((jy / 4) * 4)) .ne. 0) return 
      if ((jy - ((jy / 400) * 400)) .eq. 0) goto 10
      if ((jy - ((jy / 100) * 100)) .eq. 0) return 
   10 if (jm .ge. 3) return 
      jul1800 = jul1800 - 1
      return 
      end
