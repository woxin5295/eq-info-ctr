      subroutine sortk(x,key,no)
c --  sorts array of real*4 elements by associated key,
c       so that original array is preserved
c---- modified from w.h.k. lee sort
      real*4 x(1)
      integer*4 key(1), no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(x(key(i))-x(key(i+mo))) 28,28,27
 27   kemp=key(i)
      key(i)=key(i+mo)
      key(i+mo)=kemp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine dsortk(x,key,no)
c --  sorts array of real*8 elements by associated key,
c       so that original array is preserved
c---- modified from w.h.k. lee sort
      real*8 x(1)
      integer*4 key(1), no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(x(key(i))-x(key(i+mo))) 28,28,27
 27   kemp=key(i)
      key(i)=key(i+mo)
      key(i+mo)=kemp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine i2sortk(x,key,no)
c --  sorts array of integer*2 elements by associated key,
c       so that original array is preserved
c---- modified from w.h.k. lee sort
      integer*2 x(1)
      integer*4 key(1), no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(x(key(i))-x(key(i+mo))) 28,28,27
 27   kemp=key(i)
      key(i)=key(i+mo)
      key(i+mo)=kemp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine i4sortk(x,key,no)
c --  sorts array of integer*4 elements by associated key,
c       so that original array is preserved
c---- modified from w.h.k. lee sort
      integer*4 x(1)
      integer*4 key(1), no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(x(key(i))-x(key(i+mo))) 28,28,27
 27   kemp=key(i)
      key(i)=key(i+mo)
      key(i+mo)=kemp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine sort(x,no)
c --  sorts array of real*4 elements
c---- modified from w.h.k. lee sort
      real*4 x(1), temp
      integer*4 no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(x(i)-x(i+mo)) 28,28,27
 27   temp=x(i)
      x(i)=x(i+mo)
      x(i+mo)=temp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine dsort(x,no)
c --  sorts array of real*8 elements
c---- modified from w.h.k. lee sort
      real*8 x(1), temp
      integer*4 no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(x(i)-x(i+mo)) 28,28,27
 27   temp=x(i)
      x(i)=x(i+mo)
      x(i+mo)=temp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine i2sort(x,no)
c --  sorts array of integer*2 elements
c---- modified from w.h.k. lee sort
      integer*2 x(1), temp
      integer*4 key(1), no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(x(key(i))-x(key(i+mo))) 28,28,27
 27   temp=x(i)
      x(i)=x(i+mo)
      x(i+mo)=temp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine i4sort(x,no)
c --  sorts array of integer*4 elements
c---- modified from w.h.k. lee sort
      integer*4 x(1), temp
      integer*4 key(1), no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(x(key(i))-x(key(i+mo))) 28,28,27
 27   temp=x(i)
      x(i)=x(i+mo)
      x(i+mo)=temp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine csortk(cx,key,no)
c --  sorts array of character elements by associated key,
c       so that original array is preserved
c---- modified from w.h.k. lee sort
      character*(*) cx(1)
      integer*4 key(1), no
C     implicit integer*4 (i-n)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if (cx(key(i)) .le. cx(key(i+mo))) go to 28
 27   kemp=key(i)
      key(i)=key(i+mo)
      key(i+mo)=kemp
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end

      subroutine csort(cx,no)
c --  sorts array of character elements
c---- modified from w.h.k. lee sort
      character*(*) cx(1)
      integer*4 no
      character*256 ctemp
C     implicit integer*4 (i-n)
      lenc = len(cx(1))
      if (lenc .gt. len(ctemp)) then
	print *, 'Error (csort) - length of character elements too large'
	return
      endif
      if (no .le. 0) return
      mo=no
 2    if(mo-15) 21,21,23
 21   if(mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      go to 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if(cx(i).le.cx(i+mo)) go to 28
 27   ctemp(1:lenc)=cx(i)
      cx(i)=cx(i+mo)
      cx(i+mo)=ctemp(1:lenc)
      i=i-mo
      if(i-1) 28,26,26
 28   jo=jo+1
      if(jo-ko) 25,25,2
 29   return
      end
c***********************************************************************
c*******
c  SORTS A DATA ARRAY, X, OF REAL NUMBERS AND ITS ASSOCIATED INDEX KEYS
c  MODIFIED FROM THE ROUTINE SORT AT SLAC
c***********************************************************************
c*******
c        X     - DATA ARRAY (REAL)
c        KEY   - ASSOCIATED INDEX KEYS (INTEGER*2)
c        N     - DIMENSION OF X AND KEY ARRAYS
c***********************************************************************
c*******
      subroutine rsortk(x, key, n)
      dimension x(n)
*F77CVT -- Sun extension:  INTEGER*n declaration
      integer*2 key(n), keysav
# 12 "rsortk.for"
      igap = n
   10 if (igap .le. 1) return 
      igap = igap / 2
      imax = n - igap
   20 iex = 0
      do 30 i = 1, imax
      iplusg = i + igap
      if (x(i) .le. x(iplusg)) goto 30
      savx = x(i)
      keysav = key(i)
      x(i) = x(iplusg)
      key(i) = key(iplusg)
      x(iplusg) = savx
      key(iplusg) = keysav
      iex = iex + 1
   30 continue
      if (iex .ne. 0) goto 20
      goto 10
      end
