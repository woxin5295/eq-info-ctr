c     Program to create the HYPODD ".cc" file from correl output.
c
c     Usage: create_ccfile.e inputfile outputfile cccmin iflag spdifmin minobs
c
c     inputfile -- cross-correlation (dbcorrelate) output file
c     outputfile -- HYPODD cross-cor input file, usually "dt.cc"
c     cccmin -- minimum cross-correlation coef. value to accept (absolute value)
c               (~ 0.6 to 0.7 normally)
c     iflag --  0 means use both neg. and pos. xcor's; 1 mean use only pos.
c     spdifmin -- if both P and S xcor's present, they are both rejected if the
c              difference is > spdifmin (~0.1 s would be good)
c     minobs -- minimum # of xcor's for any event pair; otherwise all rejected
c               (1 may be OK if xcor times are used in conjunction with normal
c               travel time differences in HYPODD for instance)

      parameter   (maxrows=1000) ! maximum number of ccc's for an event pair
      character   filename*80
      character   anum*10
      character*4 asta,sta(maxrows)
      character*1 apha,pha(maxrows)
      real*8      atdif,otdif,tdif,dt,amp
      real*8      ta(maxrows),to(maxrows)
      real*4      r(maxrows)
      integer     orid1,orid2,orid1last,orid2last
      integer     eof

      numarg = iargc()
      if (numarg .lt. 6) then
        write(6,*) 'usage: create_ccfile.e inputfile outputfile cccmin       
     &iflag spdifmin minobs'
        stop
      endif

      call getarg(1,filename)
      open(1,file=filename,iostat=ios)
      if (ios.ne.0) then
        write(6,*) 'Problem opening file.'
        stop
      endif

      call getarg(2,filename)
      open(2,file=filename,iostat=ios)
      if (ios.ne.0) then
        write(6,*) 'Problem opening file.'
        stop
      endif

      call getarg(3,anum)
      read(anum,*) cccmin

      call getarg(4,anum)
      read(anum,*) iflag

      call getarg(5,anum)
      read(anum,*) spdifmin

      call getarg(6,anum)
      read(anum,*) minobs

      np=0	!event pair counter
      k=0
      eof=0
      ifirst=1
c     Read through xcor list, outputting cc file info.
    1 read(1,*,end=97,err=1) asta,apha,orid1,jdate1,orid2,jdate2,ccc,
     &amp,atdif,otdif
   10 format(a4,1x,a1,1x,i8,i8,i8,i8,f8.3,f15.3,f15.3)
c      print*,asta,apha,orid1,jdate1,orid2,jdate2,ccc,atdif
   51 if ((orid1.eq.orid1last .and. orid2.eq.orid2last) 
     &    .or. ifirst.eq.1) then
        if ((iflag.eq.0 .and. abs(ccc).gt.cccmin) .or.
     &      (iflag.eq.1 .and.      ccc.gt.cccmin)) then
c         Continue to save info.
   52     k=k+1
          sta(k)=asta
          pha(k)=apha
          ta(k)=atdif
          to(k)=otdif
          r(k)=ccc
          ifirst=0
          orid1last=orid1
          orid2last=orid2
        endif
        goto 1
      endif
      goto 98

   97 eof=1
c     Read all stations for event pair, so dump info.
   98 continue

      np=np+1
      if (k.eq.0) goto 96
      if (k.lt.minobs) goto 96
      na=k
      write(6,*) 'event pair # ,orid1,orid2, # arrivals = ',
     &np,orid1last,orid2last,na
c     Write the event header record.
      write(2,'(a1,2i8,a4)') '#',orid1last,orid2last,' 0.0'
      do i=1,na
c       Do a consistency check if there is a P-S pair. The computed event time 
c       differences should be very close.
        if (i.gt.1 .and. (sta(i).eq.sta(i-1)) .and. 
     &      pha(i).eq.'S' .and. pha(i-1).eq.'P') then
          tdif = ta(i)-ta(i-1)
c         write(9,'(f10.3)') tdif
          if (abs(tdif).gt.spdifmin) goto 91
        endif
c N.R. replace dt=-ta+to with dt=ta
c        dt = ta(i)
        dt = -ta(i) + to(i)
c       Rectify the ccc's so that all weights are positive.
        r(i) = abs(r(i))
        write(2,'(a7,f7.3,f6.3,1x,a1)') sta(i),dt,r(i),pha(i)
   91   continue
      enddo

c     Return to processing next event pair.
   96 continue
      orid1last=orid1
      orid2last=orid2
      k=0
      if (eof.eq.0) goto 51

   99 continue
      close(1)
      close(2)
      end
