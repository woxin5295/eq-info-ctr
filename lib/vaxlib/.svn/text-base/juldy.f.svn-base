c---- TEST FUNCTION JULDY
c     logical eof
c  10 idflt = 1960
c     iyear = iaskk(' GIVE YEAR, (0 TO STOP):',idflt,eof)
c     if (iyear .eq. 0) stop 
c     idflt = 1
c     imon = iaskk(' GIVE MONTH:',idflt,eof)
c     idflt = 1
c     iday = iaskk(' GIVE DAY:',idflt,eof)
c     ntime = juldy(iyear,imon,iday)
c     write(unit=6, fmt=100) ntime
c 100 format(45h THE NUMBER OF DAYS SINCE JANUARY 1, 1960 IS ,i10)
c     goto 10
c     end
c---- CALCULATES THE NUMBER OF DAYS ELAPSED SINCE JANUARY 1, 1960
      function juldy(jy, jm, jd)
      dimension ms(12)
      data ms / 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /
      juldy = ((((((365 * (jy - 1960)) + (jy / 4)) - 475) + ms(jm)) + jd
     &) - (jy / 100)) + (jy / 400)
      if ((jy - ((jy / 4) * 4)) .ne. 0) return 
      if ((jy - ((jy / 400) * 400)) .eq. 0) goto 10
      if ((jy - ((jy / 100) * 100)) .eq. 0) return 
   10 if (jm .ge. 3) return 
      juldy = juldy - 1
      return 
      end
