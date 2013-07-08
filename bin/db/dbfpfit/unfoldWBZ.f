c read an extended summary record and rotate the mechanism so that
c it will appear correctly when the WBZ is unfolded.

        real              dd        ! dip directions in degrees
        real              da        ! dip angle in degrees
        real              sa        ! rake angle in degrees
        real              rotaz     ! azimuth of rotation vector
        real              rotdip    ! dip of rotation vector
        real              rotdeg    ! amount of rotation
c                                       (following right-hand rule)
        real              darot     ! dip angle of rotated mechanism
        real              ddrot     ! dip direction of rotated mechanism
        real              sarot     ! slip angle of rotated mechanism
	character*80	  fname	    ! file name
	character*80	  aask	    ! statement function to get characters

      	character*2 dnstrng
      	character*140 phcard(257)
      	character*140 sumrec
      	dimension keyph(257)
      	logical eoff, torf, archive
      	maxp = 256

	
      	print *, 'Welcome to unfoldWBZ!  This program will replace'
      	print *, 'the focal mechanism on an extended summary '
	print *, 'record with one that is rotated corresponding to'
	print *, 'an unfolded WBZ.'
      	print *, 'The local WBZ dip and the local azimuth of the'
	print *, 'cs axis specify the rotation.'
      	print *, ' '
c     	print *, 'Added variables are cs, cx, sd, sz, dip, azcs'
c     	print *, 'in columns 111- 140'
c

    
20    	print *, 'Name of original archive or summary file'
	fname = aask('in extend summary format (run addsecloc first)',
     *    'in.addsum', -80)
      	call openit('read', fname, 1, ierr)
      	if(ierr .ne. 0) goto 20
 
      	archive = torf('is this file in archive format', .false.)
21    	fname = aask('new archive file with added coordinates',
     1 	  'out.rotsum', -50)
      	call openit('write', fname, 2, ierr)
      	if(ierr .ne. 0) goto 21
 
c     	phcard(maxp)     array of records read
c     	keyph(maxp)      key to record type
c      	                 0 --- phase record
c      	                -1 --- comment record
c      	                -2 --- summary record
c     	lph             number of records returned
c     	iplt            unit number for reading data
      	iplt = 1
c     	eoff            .false. --- end of file not reached
c      	               .true. --- found end of file
c     	maxp            maximum number of phases allowed
c read the data associated with one earthquake
30    if(archive) then
        call phaget2(phcard, keyph, lph, iplt, eoff, maxp)
	    if(iddrot .lt. 0) iddrot = 360 + iddrot
        if(lph .eq. 0) goto 65
        npro = npro + 1
        do 39 irec = 1, lph
          if(keyph(irec) .ne. -2) goto 39
c         summary record
          if( phcard(irec)(13:16) .eq. '    ') then
d           print *, 'fake summary record'
d           print *, phcard(irec)
c           fake summary record, so do not add any coordinates
          else
d           print *, 'real summary record'
d           print *, phcard(irec)
c           real summary record
            read(phcard(irec), 33, err = 77)
     *        dd, da, sa, cs, cx, sd, sz, dip, azcs
33          format(81x, f3.0, f3.0, f4.0, t111, 6f5.1)
	    rotaz = azcs
	    rotdip = 0.0
	    rotdeg = dip
c rotate mechanism about an axis pointing in the (rotaz, rotdip) direction
c by the amount rotdeg
            call  rotmech (dd, da, sa, rotaz, rotdip, rotdeg,
     *        ddrot1, darot1, sarot1)

c next rotate mechanism about a downward vertical axis by -azcs
	    rotaz = 0.0
	    rotdip = 90.0
	    rotdeg = -azcs
            call  rotmech (ddrot1, darot1, sarot1, rotaz, rotdip, 
     *	      rotdeg, ddrot, darot, sarot)

	    iddrot = ddrot + sign(0.5, ddrot)
	    if(iddrot .lt. 0) iddrot = 360 + iddrot
	    idarot = darot + sign(0.5, darot)
	    isarot = sarot + sign(0.5, sarot)

            write(phcard(irec)(82:91), 35) iddrot, idarot, isarot
35          format(i3, i3, i4)
            goto 50
          endif
39      continue
 
c       event does not include summary record
        write(3, 37)
37      format(' this event does include a summary record:')
        write(3, 38) (phcard(i), i = 1, lph)
38      format(1x, a)
50      do 60 i = 1, lph
          write(2, 55) phcard(i)
55        format(a)
60      continue
65      if(.not. eoff) goto 30
        print *, ' processed ', i6, ' archive events.'
        stop
      else
c non-archive format
72      read(1, '(a)', end=80) sumrec
        if(dnstrng(sumrec(1:2)) .ne. 'c*') then
          npro = npro + 1
          read(sumrec, 33, err = 77)
     *      dd, da, sa, cs, cx, sd, sz, dip, azcs
c33       format(81x, f3.0, f3.0, f4.0, t111, 6f5.1)
          rotaz = azcs
          rotdip = 0.0
          rotdeg = dip
c rotate mechanism about an axis pointing in the (rotaz, rotdip) direction
c by the amount rotdeg
          call  rotmech (dd, da, sa, rotaz, rotdip, rotdeg,
     *      ddrot1, darot1, sarot1)
c	  print *, 'dd, da, sa, rotaz, rotdip, rotdeg, ', 
c    *      'ddrot, darot, sarot'
c	  print *,  dd, da, sa, rotaz, rotdip, rotdeg,  
c    *      ddrot, darot, sarot
 
c next rotate mechanism about a downward vertical axis by -azcs
          rotaz = 0.0
          rotdip = 90.0
          rotdeg = -azcs
          call  rotmech (ddrot1, darot1, sarot1, rotaz, rotdip,
     *      rotdeg, ddrot, darot, sarot)
 
          iddrot = ddrot + sign(0.5, ddrot)
	  if(iddrot .lt. 0) iddrot = 360 + iddrot
          idarot = darot + sign(0.5, darot)
          isarot = sarot + sign(0.5, sarot)
 
          write(sumrec(82:91), 35) iddrot, idarot, isarot
c35       format(i3, i3, i4)
c	  print *, 'original mechanism dipidirection, dip, and rake',
c    *      dd, da, sa
c	  print *, ' rotated mechanism dip-direction, dip, and rake',
c    *      ddrot, darot, sarot

        endif
        write(2, '(a)') sumrec(1:lentru(sumrec))
        goto 72
      endif
 
77    print *, ' *** decode error reading this summary record ***'
      if(archive) then
        print *, phcard(1)
      else
        print *, sumrec
      endif
      stop
80    print *, 'processed ', npro, ' summary records'
      stop
      end
