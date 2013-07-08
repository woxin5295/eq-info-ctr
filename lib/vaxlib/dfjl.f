c---- TEST SUBROUTINE DFJL
# 2 "dfjl.for"
   10 iyear = iaskk(' GIVE YEAR, (0 TO STOP):',1960)
      if (iyear .eq. 0) stop 
      jday = iaskk(' GIVE JULIAN DAY',1)
      call dfjl(iyear, jday, imo, idy)
      write(unit=6, fmt=100) imo, idy
  100 format(23h THE MONTH AND DAY ARE ,2i3)
      goto 10
      end
c---- CALCULATES THE DATE FROM THE YEAR AND THE JULIAN DAY
      subroutine dfjl(iyear, jday, imo, idy)
      dimension ms(13), msl(13)
c----   NON LEAP YEAR
      data ms / 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 
     &365 /
      data msl / 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 
     &366 /
# 15 "dfjl.for"
      if ((iyear - ((iyear / 4) * 4)) .ne. 0) then
# 17 "dfjl.for"
      do 20 i = 2, 13
      if (ms(i) .lt. jday) goto 20
      imo = i - 1
      idy = jday - ms(imo)
      return 
   20 continue
      write(unit=*, fmt=*) 'THE JULIAN DAY EXCEEDED 365!!', iyear, jday
      stop 
c----   LEAP YEAR
# 25 "dfjl.for"
      else
# 27 "dfjl.for"
      do 30 i = 2, 13
      if (msl(i) .lt. jday) goto 30
      imo = i - 1
      idy = jday - msl(imo)
      return 
   30 continue
      write(unit=*, fmt=*) 'THE JULIAN DAY EXCEEDED 366!!', iyear, jday
      stop 
      end if
      end
