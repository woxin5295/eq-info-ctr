c     FINDS THE TRUE LENGTH OF A CHARACTER VARIABLE
      integer function lentru(alph)
      character alph*(*)
      l = len(alph)
      do 100 i = l, 1, -1
      if ((alph(i:i) .ne. ' ') .and. 
     *    (alph(i:i) .ne. '\0')) goto 200
  100 continue
  200 lentru = i
      return 
      end
