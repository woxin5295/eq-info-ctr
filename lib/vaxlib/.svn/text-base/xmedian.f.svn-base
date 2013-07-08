      function xmedian(x, key, ntot)
c----- FIND THE MEDIAN VALUE OF X      
c      PRINT *, 'NTOT = ', NTOT
      dimension x(ntot), key(ntot)
# 21 "xmedian.for"
      if (ntot .eq. 0) then
      xmedian = 0.
      return 
      end if
c     PRINT *, X
# 25 "xmedian.for"
      call sort(x, key, ntot, 1)
c     PRINT *, 'ODD'
c      ODD
# 27 "xmedian.for"
      if (mod(ntot,2) .ne. 0) then
# 30 "xmedian.for"
      xmedian = x((ntot / 2) + 1)
      return 
c     PRINT *, 'EVEN'
c     EVEN
# 32 "xmedian.for"
      end if
# 35 "xmedian.for"
      xmedian = (x(ntot / 2) + x((ntot / 2) + 1)) / 2.
      return 
      end
