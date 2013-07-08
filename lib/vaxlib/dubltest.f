      double precision dubl, dx, dubl1, dubl2, dd
# 2 "dubltest.for"
      handle = 0
   20 x = raskk('NEXT VALUE OF X',x)
      write(unit=*, fmt='(E30.15)') x
c
# 5 "dubltest.for"
      n = iaskk('NUMBER OF TIMES TO REPEAT',1)
# 7 "dubltest.for"
      dd = dble(x)
      write(unit=*, fmt=*) 'SYSTEM DOUBLE PRECISION VALUE'
c
# 9 "dubltest.for"
      write(unit=*, fmt='(E30.15)') dd
# 11 "dubltest.for"
c     it = lib$init_timer(handle)
      do 30 i = 1, n
      dx = dubl(x)
   30 continue
      write(unit=*, fmt=*) 'DUBL'
      write(unit=*, fmt='(E30.15)') dx
c
# 17 "dubltest.for"
c     it = lib$show_timer(handle)
# 19 "dubltest.for"
c     it = lib$init_timer(handle)
      do 40 i = 1, n
      dx = dubl1(x)
   40 continue
      write(unit=*, fmt=*) 'DUBL1'
      write(unit=*, fmt='(E30.15)') dx
c
# 25 "dubltest.for"
c     it = lib$show_timer(handle)
# 27 "dubltest.for"
c     it = lib$init_timer(handle)
      do 50 i = 1, n
      dx = dubl2(x)
   50 continue
      write(unit=*, fmt=*) 'DUBL2'
      write(unit=*, fmt='(E30.15)') dx
c     it = lib$show_timer(handle)
      goto 20
      end
