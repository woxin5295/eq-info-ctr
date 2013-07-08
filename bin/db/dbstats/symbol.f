      subroutine symbol(xc,yc,r1,r2,az,nsymb,iclose,iclip,thick,ithick)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    routine symbol will draw either a open or closed symbol (open meaning
c    a symbol with a white center, closed a symbol with a black center)
c
c    inputs  - xc     = x-coordinate of the center of the symbol in user units
c	       yc     = y-coordinate of the center of the symbol in user units
c              r1     = semi-major axis of the symbol in inches
c              r2     = semi-minor axis of the symbol in inches
c              az     = azimuth (in degrees) of the semi-major axis from the 
c			vertical (north)
c              nsymb  = symbol number 
c			1  -  circle
c			2  -  Ellipse
c			3  -  triangle
c			4  -  box
c			5  -  six point star
c			6  -  sextagon
c			7  -  eight point star
c			8  -  octagon
c			9  -  downward arrow
c			10 -  Plus sign
c			11 -  circle repeated
c			12 -  ellipse repeated
c			13 -  encircled triangle
c			14 -  encircled box
c			15 -  encircled six point star
c			16 -  encircled sextagon
c			17 -  encircled eight point star
c			18 -  encircled octagon
c			19 -  encircled arrow (not yet implemented)
c			20 -  encircled plus sign
c              iclose = open-closed flag
c			= 0 - symbol open
c			.ne. 0 - symbol closed
c              iclip  = clip flag see nplot
c              thick  = line thickness see nplot
c			note: this is passed to nplot along with iclip.
c 			      This allows big open symbols with thick
c			      circumferences. Probably should not be used
c			      for closed symbols.
c              ithick = thickness flag see nplot
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      common /pscl/ xmin,xmax,ymin,ymax,xscale,yscale,xrange,yrange
c
      common /spc/ xll,yll,xur,yur,ASPECT,rc
c
      data rad / 0.0174532925 /
c

      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),nsymb
      write(6,'(1x,a)')'Error : Symbol :: Proper symbol not chosen!'
c
 1    call ellipse(xc,yc,r1,r2,az,99,iclose,iclip,thick,ithick)
      go to 99
c
 2    call ellipse(xc,yc,r1,r2,az,99,iclose,iclip,thick,ithick)
      go to 99
c
 3    call ellipse(xc,yc,r1,r2,az,3,0,iclip,thick,ithick)
      if(iclose.ne.0) then
        dr=r1/2.
        nit=int((r1-dr)/.01)
        if(nit.gt.50)nit=50
        r11=r1
        r22=r2
        do 100 i=1,nit
          r11=r11-.01
          r22=r22-.01
          call ellipse(xc,yc,r11,r22,az,3,0,iclip,thick,ithick)
100     continue
        call ellipse(xc,yc,r11,r22,az,3,iclose,iclip,thick,ithick)
      endif
      go to 99
c
 4    call ellipse(xc,yc,r1,r2,az,4,iclose,iclip,thick,ithick)
      go to 99
c
 5    call ellipse(xc,yc,r1,r2,az,3,0,iclip,thick,ithick)
      call ellipse(xc,yc,r1,r2,az+180.,3,0,iclip,thick,ithick)
      if(iclose.ne.0) then
        dr=r1/2.
        nit=int((r1-dr)/.01)
        if(nit.gt.50)nit=50
        r11=r1
        r22=r2
        do 200 i=1,nit
          r11=r11-.01
          r22=r22-.01
          call ellipse(xc,yc,r11,r22,az,3,0,iclip,thick,ithick)
          call ellipse(xc,yc,r11,r22,az+180.,3,0,iclip,thick,ithick)
200     continue
        call ellipse(xc,yc,r11,r22,az,3,iclose,iclip,thick,ithick)
        call ellipse(xc,yc,r11,r22,az+180.,3,iclose,iclip,thick,ithick)
      endif
      go to 99
c
 6    call ellipse(xc,yc,r1,r2,az,6,iclose,iclip,thick,ithick)
      go to 99
c
 7    call ellipse(xc,yc,r1,r2,az,4,iclose,iclip,thick,ithick)
      call ellipse(xc,yc,r1,r2,az+45.,4,iclose,iclip,thick,ithick)
      go to 99
c
 8    call ellipse(xc,yc,r1,r2,az,8,iclose,iclip,thick,ithick)
      go to 99
 9    call niceinq(x1,x2,x3,x4,x5,x6,x7,x8,hi,ra,sl)
      call chrsiz(2.*r1+.03,r2/r1,0.)
      call cfont(21)
      call text(xc,yc,az,3,'z',iclip)
      call cfont(9)
      call chrsiz(hi,ra,sl)
      go to 99
c
 10   dyc=r1*rc/yscale*cos(az*rad)
      dxc=r1*rc/xscale*sin(az*rad)
      call line(xc+dxc,yc+dyc,xc-dxc,yc-dyc,thick,ithick,iclip)
      dxc=r2*rc/xscale*cos(az*rad)
      dyc=r2*rc/yscale*sin(az*rad)
      call line(xc+dxc,yc-dyc,xc-dxc,yc+dyc,thick,ithick,iclip)
c     call niceinq(x1,x2,x3,x4,x5,x6,x7,x8,hi,ra,sl)
c     call chrsiz(r1+.07,r2/r1,0.)
c     call cfont(21)
c     call text(xc,yc,az,4,'e',iclip)
c     call cfont(9)
c     call chrsiz(hi,ra,sl)
      go to 99
c
 11   call ellipse(xc,yc,r1,r2,az,99,iclose,iclip,thick,ithick)
      go to 99
c
 12   call ellipse(xc,yc,r1,r2,az,99,iclose,iclip,thick,ithick)
      go to 99
c
 13   call ellipse(xc,yc,r1,r2,az,3,0,iclip,thick,ithick)
      if(iclose.ne.0) then
        dr=r1/2.
        nit=int((r1-dr)/.01)
        if(nit.gt.50)nit=50
        r11=r1
        r22=r2
        do 300 i=1,nit
          r11=r11-.01
          r22=r22-.01
          call ellipse(xc,yc,r11,r22,az,3,0,iclip,thick,ithick)
300     continue
        call ellipse(xc,yc,r11,r22,az,3,iclose,iclip,thick,ithick)
      endif
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 14   call ellipse(xc,yc,r1,r2,az,4,iclose,iclip,thick,ithick)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 15   call ellipse(xc,yc,r1,r2,az,3,0,iclip,thick,ithick)
      call ellipse(xc,yc,r1,r2,az+180.,3,0,iclip,thick,ithick)
      if(iclose.ne.0) then
        dr=r1/2.
        nit=int((r1-dr)/.01)
        if(nit.gt.50)nit=50
        r11=r1
        r22=r2
        do 400 i=1,nit
          r11=r11-.01
          r22=r22-.01
          call ellipse(xc,yc,r11,r22,az,3,0,iclip,thick,ithick)
          call ellipse(xc,yc,r11,r22,az+180.,3,0,iclip,thick,ithick)
400     continue
        call ellipse(xc,yc,r11,r22,az,3,iclose,iclip,thick,ithick)
        call ellipse(xc,yc,r11,r22,az+180.,3,iclose,iclip,thick,ithick)
      endif
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 16   call ellipse(xc,yc,r1,r2,az,6,iclose,iclip,thick,ithick)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 17   call ellipse(xc,yc,r1,r2,az,4,iclose,iclip,thick,ithick)
      call ellipse(xc,yc,r1,r2,az+45.,4,iclose,iclip,thick,ithick)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 18   call ellipse(xc,yc,r1,r2,az,8,iclose,iclip,thick,ithick)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 19   go to 99
c
c20   call niceinq(x1,x2,x3,x4,x5,x6,x7,x8,hi,ra,sl)
c     call chrsiz(r1+.07,r2/r1,0.)
c     call cfont(21)
c     call text(xc,yc,az,4,'e',iclip)
c     call cfont(9)
c     call chrsiz(hi,ra,sl)
 20   dyc=r1*rc/yscale*cos(az*rad)
      dxc=r1*rc/xscale*sin(az*rad)
      call line(xc+dxc,yc+dyc,xc-dxc,yc-dyc,thick,ithick,iclip)
      dxc=r2*rc/xscale*cos(az*rad)
      dyc=r2*rc/yscale*sin(az*rad)
      call line(xc+dxc,yc-dyc,xc-dxc,yc+dyc,thick,ithick,iclip)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 99   return
      end
