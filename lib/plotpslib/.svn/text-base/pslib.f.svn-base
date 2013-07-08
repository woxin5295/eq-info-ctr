c  subroutine package to simulate calcomp-style plot calls in postscript format
c  for 300 dpi plotter

c  fonts available are those for QMS ps810 turbo laser printer (see newfont)

c  if the logical unit number for output is not specified in plots, the
c  default unit is 88
c______________________________________________________________________________

c -- initialization

	subroutine plots(i1, i2, lun)

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
        common /fontc/ fonsiz, fontyp
 
c    font
        character*30 fontyp
        data fonsiz /0./, fontyp/'Helvetica'/
 
c    logical unit for output
        data luno/88/
 
c    dpi, dots-per-inch
        data dpi /300./

c    current origin
        data ix0, iy0, fact0, msglvl /0, 0, 1., 0/

	if (lun .gt. 0) luno = lun

	write(luno, '(
     *''%!''/
     *''72 300 div dup scale''/
     *''1 setlinecap''/
     *''1 setlinejoin''/
     *''userdict /picstr 2 string put''/
     *''/M /moveto load def''/
     *''/v /rmoveto load def''/
     *''/L /lineto load def''/
     *''/r /rlineto load def''/
     *''/S /stroke load def''/
     *''/A /setgray load def''/
     *''/C /setrgbcolor load def''/
     *''/newpen /setlinewidth load def''
     *)')

c	write(luno, '(i6,1x,i6,'' translate 0 0 M'')') ix0, iy0
 	write(luno, '(''0 0 translate 0 0 M'')') 

	return
	end

        subroutine where(xc, yc, dfact)

c       x, y - returned coordinates of current pen position
c       dfact - returned current plot scaling factor

        common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
    	common /curr/ xcur, ycur
c -- return current plot location and scale factor
      	xc = xcur  
      	yc = ycur
	dfact = 1.0

c -- return current pen position and plotting dpi
c       xc = x0
c       yc = y0
c       dfact = fact0

        return
        end
c______________________________________________________________________________
 
 
 
        subroutine circle(x, y, diameter)
 
c       x, y - position of center of circle
c       radius - radius of circle
 
        common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
        common /curr/ xcur, ycur

	radius = diameter/2.0
        xcur = x + radius
        ycur = y
 
        ix = nint(x*dpi)
        iy = nint(y*dpi)
        ir = nint(radius*dpi)
 
        write(luno,'(5i6,'' 0 360 arc S'')') ix+ir, iy, ix, iy, ir

	return
	end

c______________________________________________________________________________



	subroutine plot(x, y, ipen)

c	x, y - position of pen move in inches
c	ipen - pen control:
c	  = +3  move to (x,y)
c	  = +2  draw to (x,y)
c	  = -2  draw to (x,y) and reset origin
c	  = -3  move to (x,y) and reset origin
c         = +/-888  switch to landscape mode
c	  = +/-999  end of plot

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
	common /curr/ xcur, ycur
      	logical tracit, clipon, autocad, landsc
      	common /mbdx/ tracit, xmn, xmx, ymn, ymx, clipon, autocad

	data numnos/0/, ixtrans/0/, iytrans/0/, landsc/.false./
	save numnos, ixtrans, iytrans
	xcur = x
	ycur = y

	ix = nint(x*dpi)
	iy = nint(y*dpi)
	ip = iabs(ipen)

	if (ip .eq. 2) then
	  write(luno,'(i6,1x,i6,'' L'')') ix, iy
	  numnos = numnos + 1
	  if (numnos .gt. 500) then
            write(luno,'(''S'', i6,1x,i6,'' M'')') ix, iy
	    numnos = 0
	  endif
	  if (ipen .lt. 0) 
     *    write(luno,'(i6,1x,i6,'' translate 0 0 M'')') ix, iy
          if (tracit) write(14, '(2f12.6,3x,i5)') x, y, ipen

	else if (ip .eq. 3) then
	  write(luno,'(i6,1x,i6,'' M'')') ix, iy
          if (tracit) write(14, '(2f12.6,3x,i5)') x, y, ipen
	  if (ipen .lt. 0)  then
            write(luno,'(i6,1x,i6,'' translate 0 0 M'')') ix, iy
	    ixtrans = ixtrans + ix
	    iytrans = iytrans + iy
 	  endif

	else if (ip .eq. 999) then

	  write(luno,'(''S showpage'')')
c Found the next lines to be unnecessary 6/4/95 jcl
c	  write(luno,'(2i12, '' translate '')') -ixtrans, -iytrans
c         ixtrans = 0.0
c         iytrans = 0.0
c	  if(landsc) then
c           write(luno,'(''-90 rotate'')')
c	    write(luno,'(''-8.5 300 mul 0 translate 0 0 M'')')
c	  endif
c	  write(luno,'(''300 72 div dup scale'')')
c	  write(luno,'(''S '')')
 	  numnos = 0
	  return

	else if (ip .eq. 888) then
	  write(luno, '(a)') ' 8.5 300 mul 0 translate 90 rotate '
 	  landsc = .true.

	else
	  if (msglvl .gt. 0) print *, 'Invalid value for ipen (plot) =', ipen

	endif

	ix0 = ix
	iy0 = iy

	return
	end
c______________________________________________________________________________

c -- number

	subroutine number(x, y, height, value, angle, ndec)

c	x, y -   starting coordinate position for lower left corner of number
c	height - character height in inches
c	value -  real*4 number
c	angle -  plotting angle, degrees counterclockwise about (x,y)
c	ndec -   number of decimal digits:
c	  >  0 - number of digits to right of decimal point after rounding
c	  =  0 - only integer portion and decimal point after rounding
c	  = -1 - integer portion, after rounding, and no decimal point
c	  < -1 - |ndec|-1 digits right truncated from integer portion and
c		 plotted after rounding

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
	common /fontc/ fonsiz, fontyp

	character*8 fmt
	character*20 string
	character*30 fontyp

	ix = nint(x*dpi)
	iy = nint(y*dpi)

	if (height .le. 0) return
	if (height .ne. fonsiz) then
	  fonsiz = height
	  write(luno, 
     *'(''/'', a, '' findfont '', i5, '' scalefont setfont'')')
     * fontyp, nint(fonsiz*dpi/0.72)
	endif

	ni = 1
	if (value .gt. 0.) then
	  ni = alog10(abs(value)) + 1
	  if (value .lt. 1.) ni = ni + 1
	else if (value .lt. 0.) then
	  ni = alog10(abs(value)) + 3
	  if (value .gt. -1.) ni = ni + 1
	endif
	
	if (ndec .gt. 0) then
	  lens = ni + ndec + 1
	  write(fmt,'(''(f'',i2,''.'',i2,'')'')') lens, ndec
	  write(string(1:lens),fmt) value

	else if (ndec .eq. 0) then
	  lens = ni + 1
	  write(fmt,'(''(f'', i2, ''.0)'')') lens
	  write(string(1:lens),fmt) value

	else if (ndec .eq. -1) then
	  lens = ni
	  write(fmt,'(''(i'', i2, '')'')') lens
	  write(string(1:lens),fmt) nint(value)

	else
	  lens = ni + ndec
	  write(fmt,'(''(i'',i2,'')'')') lens
	  write(string(1:lens),fmt) nint(value/10**iabs(ndec))

	endif
	
	write(luno, '(i6,1x,i6, '' M '')') ix, iy
	if (angle .ne. 0) write (luno, '(f5.0, '' rotate '')') angle
	write(luno,'(''('',a,'') show'')') string(1:lens)
	if (angle .ne. 0.) write(luno, '(f5.0, '' rotate '')') -angle

	ix0 = ix
	iy0 = iy

	return
	end

c______________________________________________________________________________

c -- determine appropriate plot scaling factors for an array

	subroutine scale(array,axlen,npts,inc)

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl

	dimension array(1)

	return
	end

c______________________________________________________________________________

c -- line

	subroutine line(x,y,npts,inc,lintyp,inteq)

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl

	ix = nint(x*dpi)
	iy = nint(y*dpi)

	ix0 = ix
	iy0 = iy

	return
	end

c______________________________________________________________________________

c -- axis

	subroutine axis(x,y,label,nchar,axlen,angle,fval,dv)

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
	common /fontc/ fonsiz, fontyp

	character*30 fontyp

	ix = nint(x*dpi)
	iy = nint(y*dpi)

	ix0 = ix
	iy0 = iy

	return
	end

c______________________________________________________________________________

c -- pensize

	subroutine newpen(isize)

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
	save iprevsize
  	data iprevsize/999/

	if(iprevsize .eq. isize) return
	iprevsize = isize
	write(luno, '(''S'', i4, '' newpen'')') isize

	return
	end
c______________________________________________________________________________

c -- greytype

        subroutine grey(igrey)

        common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl

	agrey = igrey/255.
        write(luno, '(''S'', f5.2, '' A'')') agrey

        return
        end

c______________________________________________________________________________

c -- color

        subroutine color(icolor)

        common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
        logical nocolor
	save iprevcolor, nocolor
        data iprevcolor/999/, nocolor/.false./

c -- toggle color on or off
	if(nocolor) then
	  if(icolor .eq. 999999) then
	    nocolor = .false.
	  endif
          return
	else
	  if(icolor .eq. 999999) then
	    nocolor = .true.
	    write(luno, '(''S 0. 0. 0. C'')')
	    return
          endif
	endif

	if(iprevcolor .eq. icolor) return
	iprevcolor = icolor

        icolor3 = icolor/1000000
        itemp = icolor - icolor3*1000000
        icolor2 = itemp/1000
        icolor1 = itemp - icolor2*1000.
        write(luno, '(''S'', 3f7.2, '' C'')') 
     *    icolor3/255., icolor2/255., icolor1/255.

        return
        end
c______________________________________________________________________________

c -- color3
 
        subroutine color3(red, green, blue)
 
        common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
        logical nocolor
	double precision prevcolor
        save prevcolor, nocolor
        data prevcolor/999./, nocolor/.false./

c -- toggle color on or off
        if(nocolor) then
          if((green .eq. 999.) .and. (blue .eq. 999)) then
            nocolor = .false.
          endif    
          return
        else   
          if((green .eq. 999.) .and. (blue .eq. 999)) then
            nocolor = .true.
            write(luno, '(''S 0. 0. 0. C'')')
            return
          endif
        endif

        color = red*1000000. + green*1000. + blue
        if(prevcolor .eq. color) return
        prevcolor = color

 
        write(luno, '(''S'', 3f7.2, '' C'')')
     *    red, green, blue
 
        return
        end


c______________________________________________________________________________

c -- symbol (character string)

	subroutine symbol(x, y, height, string, angle, nch)

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
	common /fontc/ fonsiz, fontyp

	character*(*) string
	character*30 fontyp

	if (height .le. 0) return

	lens = nch

	if (lens .gt. len(string)) lens = len(string)
	if (lens .eq. 0) lens = 1

	ix = nint(x*dpi)
	iy = nint(y*dpi)

	if (height .ne. fonsiz) then
	  fonsiz = height
	  write(luno,
     *'(''/'', a, '' findfont '', i5, '' scalefont setfont'')')
     * fontyp, nint(fonsiz*dpi/0.72)
	endif


	write(luno, '(i6,1x,i6, '' M '')') ix, iy
	if (angle .ne. 0) write (luno, '(f5.0, '' rotate '')') angle
	write(luno,'(''('',a,'') show'')') string(1:lens)
	if (angle .ne. 0.) write(luno, '(f5.0, '' rotate '')') -angle

	ix0 = ix
	iy0 = iy

	return
	end
c______________________________________________________________________________

c -- re-scale size of plot

	subroutine factor(fact)

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl

	write(luno, '(2f12.6, '' scale'')') fact, fact
	fact0 = fact

	return
	end
c______________________________________________________________________________

c -- reset message level

	subroutine setmsg(ml)

c	ml - message level:
c	  = 0 no messages
c	  > 0 list error messages

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl

	msglvl = ml

	return
	end
c______________________________________________________________________________

c -- rotate coordinate axes

	subroutine rotate(angle)

c	angle -  rotate coordinate axes counterclockwise by this angle
c	  in degrees about the current origin

	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl

	write(luno,'(f10.5, '' rotate'')') angle
	return
	end
c______________________________________________________________________________

c  -- reset font

	subroutine newfont(ifont)

c	ifont - font type

	common /fontc/ fonsiz, fontyp

	character*30 fontyp

	if (ifont .eq. 0) then
	  fontyp = 'Courier'
	else if (ifont .eq. 1) then
	  fontyp = 'Courier-Bold'
	else if (ifont .eq. 2) then
	  fontyp = 'Courier-Oblique'
	else if (ifont .eq. 3) then
	  fontyp = 'Courier-BoldOblique'
	else if (ifont .eq. 4) then
	  fontyp = 'Times-Roman'
	else if (ifont .eq. 5) then
	  fontyp = 'Times-Bold'
	else if (ifont .eq. 6) then
	  fontyp = 'Times-Italic'
	else if (ifont .eq. 7) then
	  fontyp = 'Times-Boldface'
	else if (ifont .eq. 8) then
	  fontyp = 'Helvetica'
	else if (ifont .eq. 9) then
	  fontyp = 'Helvetica-Bold'
	else if (ifont .eq. 10) then
	  fontyp = 'Helvetica-Oblique'
	else if (ifont .eq. 11) then
	  fontyp = 'Helvetica-BoldOblique'
	else if (ifont .eq. 12) then
	  fontyp = 'Symbol'
	else if (ifont .eq. 13) then
	  fontyp = 'AvantGarde-Book'
	else if (ifont .eq. 14) then
	  fontyp = 'AvantGarde-BookOblique'
	else if (ifont .eq. 15) then
	  fontyp = 'AvantGarde-Demi'
	else if (ifont .eq. 16) then
	  fontyp = 'AvantGarde-DemiOblique'
	else if (ifont .eq. 17) then
	  fontyp = 'Bookman-Demi'
	else if (ifont .eq. 18) then
	  fontyp = 'Bookman-DemiItalic'
	else if (ifont .eq. 19) then
	  fontyp = 'Bookman-Light'
	else if (ifont .eq. 20) then
	  fontyp = 'Bookman-LightItalic'
	else if (ifont .eq. 21) then
	  fontyp = 'Helvetica-Narrow'
	else if (ifont .eq. 22) then
	  fontyp = 'Helvetica-Narrow-Bold'
	else if (ifont .eq. 23) then
	  fontyp = 'Helvetica-Narrow-Bold-Obl'
	else if (ifont .eq. 24) then
	  fontyp = 'Helvetica-Narrow-Oblique'
	else if (ifont .eq. 25) then
	  fontyp = 'NewCenturySchlbk-Roman'
	else if (ifont .eq. 26) then
	  fontyp = 'NewCenturySchlbk-Bold'
	else if (ifont .eq. 27) then
	  fontyp = 'NewCenturySchlbk-Italic'
	else if (ifont .eq. 28) then
	  fontyp = 'NewCenturySchlbk-BoldItalic'
	else if (ifont .eq. 29) then
	  fontyp = 'Palatino-Roman'
	else if (ifont .eq. 30) then
	  fontyp = 'Palatino-Bold'
	else if (ifont .eq. 31) then
	  fontyp = 'Palatino-Italic'
	else if (ifont .eq. 32) then
	  fontyp = 'Palatino-BoldItalic'
	else if (ifont .eq. 33) then
	  fontyp = 'ZapfChancery-MediumItalic'
	else if (ifont .eq. 34) then
	  fontyp = 'ZaptDingbat'
	else if (ifont .eq. 51) then
	  fontyp = 'Helvetica-Condensed'
	else if (ifont .eq. 52) then
	  fontyp = 'Helvetica-Condensed-Bold'
	else if (ifont .eq. 53) then
	  fontyp = 'Helvetica-Condensed-BoldObl'
	else if (ifont .eq. 54) then
	  fontyp = 'Helvetica-Condensed-Oblique'
	else

c	default
	  fontyp = 'Helvetica'

	endif

	fonsiz = 0.

	return
	end
	
c==============================================================================
	subroutine text (xloc,yloc,height,inbuf,angle,nocar)
	character*(*) inbuf
	call symbol (xloc,yloc,height,inbuf,angle,nocar)
	return
	end

	subroutine censtr(x, y, height, string, angle, nocar)
	common /pscom/ luno, dpi, ix0, iy0, fact0, msglvl
	common /fontc/ fonsiz, fontyp
	character*(*) string
        character*30 fontyp
	entry centxt(x, y, height, string, angle, nocar)
	
	if (height .ne. fonsiz) then
	  fonsiz = height
	  write(luno,
     *'(''/'', a, '' findfont '', i5, '' scalefont setfont'')')
     * fontyp, nint(fonsiz*dpi/0.72)
	endif


	ix = nint(x*dpi)
	iy = nint(y*dpi)
	write(luno, '(i6,1x,i6, '' M '')') ix, iy

	if (angle .ne. 0) write (luno, '(f5.0, '' rotate '')') angle

	write(luno, '(3a)') 
     *    '0 (', string(1:lentru(string)),
     *    ') stringwidth pop 2 div sub 0 v' 
	write(luno,'(''('',a,'') show'')') string(1:lentru(string))

	if (angle .ne. 0) write (luno, '(f5.0, '' rotate '')') -angle

	return
	end
	subroutine landscape
        call plot(0., 0., 888)
	return
	end

