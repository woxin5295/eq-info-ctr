c     RIPPLE UNREASIONABLE TIMES UPWARD
      subroutine cortim(idy, ihr, imn, sec)
# 3 "cortim.for"
      if (sec .ge. 60.) then
      sec = sec - 60.
      imn = imn + 1
      else
      if (sec .lt. 0.) then
      write(unit=*, fmt=*) idy, ihr, imn, sec
      sec = sec + 60.
      imn = imn - 1
      end if
      end if
      if (imn .ge. 60) then
      imn = imn - 60
      ihr = ihr + 1
      else
      if (imn .lt. 0) then
      imn = imn + 60
      ihr = ihr - 1
      end if
      end if
      if (ihr .ge. 24) then
      ihr = ihr - 24
      idy = idy + 1
      else
      if (ihr .lt. 0) then
      ihr = ihr + 24
      idy = idy - 1
      end if
      end if
      return 
      end
