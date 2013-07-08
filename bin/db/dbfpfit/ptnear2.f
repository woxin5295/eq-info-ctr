      subroutine ptnear2 ( cx1, cx2, cy1, cy2, dan, ddn, sizbst,
     &      sizrst, ipt, 
     &      nread, pi, pltpl, rad, rmax1, rmax2, san, uniq, uq, vd,
     &      psymb, tsymb, prot)
      character*1       dnstrng
      real              cx1       ! x position of large circle center
      real              cx2       ! x position of small circle center
      real              cy1       ! y position of large circle center
      real              cy2       ! y position of small circle center
      real              dan(11)   ! dip angle of nearby solutions
      real              ddn(11)   ! dip direction of nearby solutions
      integer           ipt(11)   ! 0/1/2 plot p and t / plot p / plot t
      integer           nread     ! number of nearby solutions per line
      real              pi        ! pi
      logical           pltpl     ! if .true. plot nearby nodal planes
      real              psymb     ! symbol to be used for p axes
      real              rad       ! pi/180
      real              rmax1     ! radius of large circle
      real              rmax2     ! radius of small circle
      real              san(11)   ! rake of nearby solutions
      real              sizbst
      real              sizrst
      character*1       syml1
      character*1       syml2
      real              tsymb     ! symbol to be used for t axes
      logical           uniq      ! plot only unique solutions
      character*1       uq(11)    ! equals u if solution is unique
c                                          e is solution is equal to minimum
c                                          & if both are true
      real              vd        ! view direction
      real              prot      ! paper rotation: 0.00 --> +y up
c                                 ! paper rotation:-pi/2 --> -x up
c
            do 85 n = 1, nread
c             plot the p and t axes in small net for the set of 
c             "neighboring solutions"
              if (uniq) then
                if ( (dnstrng(uq(n)) .ne. 'e') .and. 
     *               (uq(n) .ne. '&') ) goto 85
              endif
              if (ipt(n) .eq. 0) then
c               plot p and t
                if ( (dnstrng(uq(n)) .eq. 'e') .or. 
     *               (uq(n) .eq. '&') ) then
c                 this solution was just as good as the best
                  call tpplot1(cx2, cy2, dan(n), ddn(n), 90., sizbst,
     &            pi, psymb, rad, rmax2, san(n), tsymb, vd,
     &            ain1, az1, syml1, ain2, az2, syml2, prot)

                else
                  call tpplot1 (cx2, cy2, dan(n), ddn(n), 90., sizrst,
     &            pi, psymb, rad, rmax2, san(n), tsymb, vd,
     &            ain1, az1, syml1, ain2, az2, syml2, prot)
                endif
              else if (ipt(n) .eq. 1) then
c               plot p only
                if ( (dnstrng(uq(n)) .eq. 'e') .or. 
     *               (uq(n) .eq. '&') ) then
c                 this solution was just as good as the best
                  call tpplot1 (cx2, cy2, dan(n), ddn(n), 90., sizbst,
     &            pi, psymb, rad, rmax2, san(n), 0., vd,
     &            ain1, az1, syml1, ain2, az2, syml2, prot)
                else
                  call tpplot1 (cx2, cy2, dan(n), ddn(n), 90., sizrst,
     &            pi, psymb, rad, rmax2, san(n), 0., vd,
     &            ain1, az1, syml1, ain2, az2, syml2, prot)
                endif
              else if (ipt(n) .eq. 2) then
c               plot t only
                if ( (dnstrng(uq(n)) .eq. 'e') .or. 
     *               (uq(n) .eq. '&') ) then
c                 this solution was just as good as the best
                  call tpplot1 (cx2, cy2, dan(n), ddn(n), 90., sizbst,
     &            pi, 0., rad, rmax2, san(n), tsymb, vd,
     &            ain1, az1, syml1, ain2, az2, syml2, prot)
                else
                  call tpplot1 (cx2, cy2, dan(n), ddn(n), 90., sizrst,
     &            pi, 0., rad, rmax2, san(n), tsymb, vd,
     &            ain1, az1, syml1, ain2, az2, syml2, prot)
                endif
              endif                
              if (pltpl) then
c
c
c compute auxiliary plane orientation
c
                call auxpln (ddn(n), dan(n), san(n), dd2, da2, sa2)
c
                if (vd .ne. 999.) then
c
c convert nodal planes to horizontal view looking at azimuth of vd degrees
c
                  call flippl ( +1, dan(n), ddn(n), vd, pi, rad)
                  call flippl ( +1, da2, dd2, vd, pi, rad)
                endif
c
c plot nodal planes
c
                call plotpl (cx1, cy1, dan(n), pi, rad, rmax1, 
     &          ddn(n) - 90., prot)
                call plotpl (cx1, cy1, da2, pi, rad, rmax1, dd2 - 90.,
     *          prot)
              end if

85          continue
      return
      end
