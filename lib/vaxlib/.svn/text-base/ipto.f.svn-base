c     FINDS THE LOCATIONS OF TARG IN STRING ALPH
c     FIRST OCCURRENCE IS AT POSITION INDX(1), NEXT AT INDX(2) ETC
      subroutine ipto(alph, targ, indx, nfound)
      character alph*(*), targ*1
      dimension indx(132)
# 6 "ipto.for"
      do 50 i = 1, 132
   50 indx(i) = 0
      nfound = 0
      do 100 i = 1, len(alph)
      if (alph(i:i) .ne. targ) goto 100
      nfound = nfound + 1
      indx(nfound) = i
  100 continue
      itto = 0
      return 
      end
