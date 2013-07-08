      subroutine mbox(x, y, xx, yy)
# 2 "mbox.for"
      call vert(y, yy, x)
      call horiz(x, xx, yy)
      call vert(yy, y, xx)
      call horiz(xx, x, y)
      return 
      end
      subroutine vert(top, bottom, xx)
# 9 "mbox.for"
      call pltt(xx, top, 3)
      call pltt(xx, bottom, 2)
      return 
      end
      subroutine horiz(aleft, right, yy)
# 14 "mbox.for"
      call pltt(aleft, yy, 3)
      call pltt(right, yy, 2)
      return 
      end
      subroutine yax(y1, y2, yin, x, xtic)
# 19 "mbox.for"
      yinc = yin
      tlen = y2 - y1
      ninc = tlen / yinc
      if (ninc .lt. 0) then
      yinc = - yinc
      ninc = - ninc
      end if
      call pltt(x, y1, 3)
      if (ninc .gt. 0) then
      do 20 i = 1, ninc
      y = y1 + (i * yinc)
      call pltt(x, y, 2)
      call pltt(x + xtic, y, 2)
      call pltt(x, y, 2)
   20 continue
      end if
      call pltt(x, y2, 2)
      return 
      end
      subroutine xax(x1, x2, xinc, y, ytic)
# 39 "mbox.for"
      tlen = x2 - x1
      ninc = tlen / xinc
      if (ninc .lt. 0) then
      xinc = - xinc
      ninc = - ninc
      end if
      call pltt(x1, y, 3)
      if (ninc .gt. 0) then
      do 20 i = 1, ninc
      x = x1 + (i * xinc)
      call pltt(x, y, 2)
      call pltt(x, y + ytic, 2)
      call pltt(x, y, 2)
   20 continue
      end if
      call pltt(x2, y, 2)
      return 
      end
