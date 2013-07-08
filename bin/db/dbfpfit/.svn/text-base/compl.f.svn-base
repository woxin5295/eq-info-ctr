      logical function compl (solns, nsol, dd, da, sa, aerr, mxslns)
c
c this function compares a "new" fault plane solution (dd, da, sa) with a list of other fault plane solutions 
c  and checks for any of the following conditions
c
c   1. the "new" solution is similar to one of the solutions in solns
c   2. the compliment of the "new" solution is similar to one of the solutions in solns
c   3. the "new" solution is similar to the compliment of one of the solutions in solns
c
c if any one of the above conditions is true, function compl returns with a value .true. 
c otherwise, function compl returns with the value .false.
c
c  solutions are similar if all three pairs of corresponding angles differ by less than aerr.
c
      real              aerr                            ! allowable difference between corresponding angles of complimentary planes
      real              da                              ! dip angle of new plane 
      real              dd                              ! dip direction angle of new plane 
      integer           mxslns                          ! maximum # of multiple solutions permitted
      integer           nsol                            ! number of planes stored in array solns
      real              sa                              ! slip angle of new plane 
      real              solns(mxslns,3)                 ! array of planes (dd, da, sa) to test new plane against
c
      integer           j                               ! loop index
      real              aux1(3)                         ! dip direction, angle, and rake of auxilliary plane of new plane
      real              aux2(3)                         ! dip direction, angle, and rake of auxilliary plane of prrevious planes
c
      compl = .false.
c
      call auxpln (dd, da, sa, aux1(1), aux1(2), aux1(3))
c
      do 40 j = 1, nsol
c
c compare new solution with each solution on list
c
        if (abs(dd - solns(j, 1)) .le. aerr .and.
     &abs(da - solns(j, 2)) .le. aerr .and.
     &rdiff(sa, solns(j, 3)) .le. aerr)  then
          compl = .true.
          return
        end if
c
c     compare compliment of "new solution" with each solution on list
c
        if (abs(solns(j, 1) - aux1(1)) .le. aerr .and. 
     &abs(solns(j, 2) - aux1(2)) .le. aerr .and.
     &rdiff(solns(j, 3), aux1(3)) .le. aerr) then
          compl = .true.
          return
        end if
c
        call auxpln (solns(j, 1), solns(j, 2), solns(j, 3), aux2(1),
     & aux2(2), aux2(3))
c
c     compare "new solution" with compliment of each solution on list
c
        if (abs(dd - aux2(1)) .le. aerr .and. 
     &abs(da - aux2(2)) .le. aerr .and.
     &rdiff(sa, aux2(3)) .le. aerr) then
          compl = .true.
          return
        end if
40    continue
c
      return
      end
c
c
c
c
c

