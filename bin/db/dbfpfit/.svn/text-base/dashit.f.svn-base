      subroutine dashit(x2, y2, j, dash, space, distmin)
c----- connects a dashed line from previous point to x2,y2
c
c      j            plot control   (2 = down;  3 = up)
c      dash         length of dashes
c      space        length of spaces
c      distmin      square of min spacing between points to plot
c
c      slopup       left over space from previous call
c      slopdn       left over dash
c
c	write(95,*) '---> called dashit with:'
c	write(95,*) 'x2, y2, j, dash, space, distmin ='
c	write(95,*) x2, y2, j, dash, space, distmin
c
      if((dash .eq. 0.) .or. (space .eq. 0)) then
        if(distmin .ne. 0.) then
          if(j .eq. 3) then
            call pltt(x2, y2, j)
            xold = x2
            yold = y2
c	write(95,*) '---> returned from dashit 1'
            return
          else
            if( ((xold-x2)**2. + (yold-y2)**2.) .lt. distmin) then
c	write(95,*) '---> returned from dashit 2'
              return
            end if
            call pltt(x2, y2, j)
            xold = x2
            yold = y2
c	write(95,*) '---> returned from dashit 3'
            return
          endif
        else
          call pltt(x2, y2, j)
        endif
c	write(95,*) '---> returned from dashit 4'
        return
      endif
      if(j .eq. 3) then
        call pltt(x2, y2, j)
        slopup = 0.
        slopdn = 0.
        xold = x2
        yold = y2
c	write(95,*) '---> returned from dashit 5'
        return
      endif
      idash = 1
      delx = x2 - xold
      dely = y2 - yold
      aleft = sqrt( (delx)**2 + (dely)**2 )
      if(aleft .eq. 0) then
c	write(95,*) '---> returned from dashit 6'
        return
      end if
      dshx = dash*delx/aleft
      dshy = dash*dely/aleft
      spax = space*delx/aleft
      spay = space*dely/aleft
      if ( (slopup .eq. 0.) .and. (slopdn .eq. 0.) ) then
10      if(idash .eq. 1) then
c
c         draw a dash
          if(aleft .ge. dash) then
            xold = xold + dshx
            yold = yold + dshy
            call pltt(xold, yold, 2)
            idash = 0
            aleft = aleft - dash
            go to 10
          else
            call pltt(x2, y2, 2)
            slopdn = dash - aleft
            xold = x2
            yold = y2
c	write(95,*) '---> returned from dashit 7'
            return
          endif
        else
c
c         draw a space
          if(aleft .ge. space) then
            xold = xold + spax
            yold = yold + spay
            call pltt(xold, yold, 3)
            idash = 1
            aleft = aleft - space
            go to 10
          else
            call pltt(x2, y2, 3)
            slopup = space - aleft
            xold = x2
            yold = y2
c	write(95,*) '---> returned from dashit 8'
            return
          endif
        endif
      endif
      if(slopdn .ne. 0.) then
c
c         draw a short dash of length slopdn
          if(aleft .ge. slopdn) then
            xold = xold + dshx*slopdn/dash
            yold = yold + dshy*slopdn/dash
            call pltt(xold, yold, 2)
            idash = 0
            aleft = aleft - slopdn
            slopdn = 0.
            go to 10
          else
            call pltt(x2, y2, 2)
            slopdn = slopdn - aleft
            xold = x2
            yold = y2
c	write(95,*) '---> returned from dashit 9'
            return
          endif
      else
c
c         draw a short space of length slopup
          if(aleft .ge. slopup) then
            xold = xold + spax*slopup/space
            yold = yold + spay*slopup/space
            call pltt(xold, yold, 3)
            idash = 1
            aleft = aleft - slopup
            slopup = 0.
            go to 10
          else
            call pltt(x2, y2, 3)
            slopup = slopup - aleft
            xold = x2
            yold = y2
c	write(95,*) '---> returned from dashit 10'
            return
          endif
      endif
      end
