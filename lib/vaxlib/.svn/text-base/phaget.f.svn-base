c phaget.for
      subroutine phaget(phcard, keyph, lph, iplt, eoff, maxp)
c read the data associated with one earthquake
c
c     phcard(maxp)     array of records read
c     keyph(maxp)      key to record type
c                       0 --- phase record
c                      -1 --- comment record
c                      -2 --- summary record
c                      -3 --- instruction record
c                             (begins with gaus more, dist, or 4 blanks)
c                      -4 --- bad time
c                      -5 --- reset record
c                      -6 --- save record - no longer used
c                      -7 --- rerun record - no longer used
c                      -8 --- scatter record
c                      -9 --- not on station list
c                     -10 --- jump record
c                     -11 --- decode error
c     lph             number of records returned
c     iplt            unit number for reading data
c     eoff            .false. --- end of file not reached
c                     .true. --- found end of file
c     maxp            maximum number of phases allowed
c
      save onesav
      character*115 phcard(maxp), dnstrng
      dimension keyph(maxp)
      logical onesav, eoff, fsteq
      data onesav/.false./, fsteq/.true./
      eoff = .false.
      npurep = 0
c
      lph = 1
c     print *, 'onesav = ', onesav
      if (onesav) then
        phcard(1) = phcard(isv)
        keyph(1) = keyph(isv)
        onesav = .false.
        if ((keyph(1) .ne. -2) .and. (keyph(1) .ne. 0)) then
          return
        endif
        goto 25
      endif
c
c loop from lph = 1 to maxp
15    read(iplt, 20, end = 70) phcard(lph)
20    format(a)
c     print *, lph, phcard(lph)
c
c classify record
25    continue
c
c comment record
      phcard(lph) = dnstrng(phcard(lph))
      if (phcard(lph)(1:2) .eq. 'c*') then
        if( (lph .eq. 1) .and. (fsteq) ) then
c comment records prior to the first summary or phase record should
c be written directly to the archive phase file
          write(11, '(a)') phcard(lph)
          goto 15
        endif
        keyph(lph) = -1
        goto 50
c
c summary record
      else if ((phcard(lph)(81:81) .eq. '/') .or.
     1  (phcard(lph)(81:81) .eq. '\\')) then
        keyph(lph) = -2
        if (npurep .gt. 0) goto 90
        goto 50
c
c instruction record
      else if ((phcard(lph)(1:4) .eq. '    ') .or.
     1        (phcard(lph)(1:4) .eq. 'gaus') .or.
     2        (phcard(lph)(1:4) .eq. 'more') .or.
     3        (phcard(lph)(1:4) .eq. 'dist')) then
        keyph(lph) = -3
        goto 80
c
c reset record
      else if (phcard(lph)(1:4) .eq. 'rese') then
	print *, 'Reset record not allowed in this input stream!!'
	stop
c
c save record
      else if (phcard(lph)(1:4) .eq. 'save') then
	print *, 'Save record not allowed in this input stream!!'
	stop
c
c rerun record
      else if (phcard(lph)(1:4) .eq. 'reru') then
	print *, 'Rerun record not allowed in this input stream!!'
	stop
c
c scatter record
      else if (phcard(lph)(1:4) .eq. 'scat') then
	print *, 'Scatter record not allowed in this input stream!!'
	stop
c
c jump record
      else if (phcard(lph)(1:4) .eq. 'jump') then
	print *, 'Jump record not allowed in this input stream!!'
	stop
      endif
c
c must be regular phase record
      keyph(lph) = 0
      npurep = npurep + 1
50    fsteq = .false.
      lph = lph + 1
      if (lph .le. maxp) goto 15
c
55    write(6, 60) maxp
      write(9, 60) maxp
60    format(///,' xxxxx exceeded maximum of ',i4,
     *           ' records per event, so stop!  xxxxx')
      stop
c
c come here on eof detected
70    eoff = .true.
      onesav = .false.
      if (lph .gt. 1) then
        phcard(lph) = ' '
        keyph(lph) = -3
        write(6, 72)
        write(9, 72)
72      format(//, ' detected end of file prior to final instruction',
     *  ' record.', /, ' check input data for completeness.')
        return
      else
        lph = lph - 1
        return
      endif
c
c come here after an instruction record or other special record
80    return
c
c come here if missing an instruction record
c     ('add' blank instruction record)
90    isv = lph + 1
      phcard(isv) = phcard(lph)
      keyph(isv) = keyph(lph)
      phcard(lph) = ' '
      keyph(lph) = -3
      onesav = .true.
      write(6, 92) phcard(isv)
      write(9, 92) phcard(isv)
92    format(/, ' xxxxx warning - missing instruction record - xxxxx',
     *       /, ' prior to this record:', /, ' ', a)
      return
      end
c end phaget1
