      subroutine shrflt1 (dpdir, dip, rake, tm)
c
c    this subroutine calculates the moment-tensor representation of a 
c    shear fault, given its dip direction, dip, and slip angles.
c
c  method:
c    the moment tensor is first expressed in a coordinate system with 
c    the z axis normal to the fault plane and the x axis in the slip 
c    direction:
c
c                    (0.  0.  1.)
c                    (0.  0.  0.)
c                    (1.  0.  0.)
c
c    this coordinate system is then rotated through the euler angles 
c    phi = -rake, theta = -dip, and psi = strike - pi,
c    (conventions of goldstein, classical mechanics, sec 4-4) which 
c    results in a (south, east, up) orientation of the (x, y, z)
c    axes, respectively.  a permutation then converts this to the 
c    order (up, south, east).  the strength of the double-couple is
c    taken as unity; the calculated moment tensor components must be 
c    multiplied by the factor:
c
c         mu*a*s
c
c    where:
c         mu is the rigidity modulus of the medium
c         a is the fault area, and
c         s is the mean dislocation across the fault.
c           (note:  if the mean dislocation velocity is used instead,
c           the result will be the moment-rate tensor.)
c
c    written by bruce r. julian on 7 april, 1977.
c    modified to use dip direction 10/23/86  j. c. lahr
c
      real              dip      ! (input) fault dip angle in radians
      real              rake     ! (input) fault rake angle in radians
      real              strike   ! (input) fault strike angle in radians
      real              tm(6)    ! (output) seismic moment tensor arranged 
c                                  in the following order:
                                 ! (r, r)         i.e. (up, up)
                                 ! (r, theta)     i.e. (up, south)
                                 ! (theta, theta) i.e. (south, south)
                                 ! (r, phi)       i.e. (up, east)
                                 ! (theta, phi)   i.e. (south, east)
                                 ! (phi, phi)     i.e. (east, east)
      real              a11      !  transformation matrix 
      real              a21      !  transformation matrix 
      real              a31      !  transformation matrix 
      real              a13      !  transformation matrix 
      real              a23      !  transformation matrix 
      real              a33      !  transformation matrix 
      real              cd       !  cos(dip)
      real              cl       !  cos(rake)
      real              cs       !  cos(strike)
      real              dpdir    !  dip direction
      real              sd       !  sin(dip)
      real              sl       !  sin(rake)
      real              ss       !  sin(strike)
c 
c  calculate components of orthogonal transformation matrix   
c  from fault-oriented to (south, east, up) coordinate system 
c
c     subtract 90 degrees to convert dip direction to strike
      strike = dpdir - 1.57079633
      ss = sin(strike)
      cs = cos(strike)
      sd = sin(dip)
      cd = cos(dip)
      sl = sin(rake)
      cl = cos(rake)
      a11 = -cs*cl - cd*sl*ss
      a21 =  ss*cl - cd*sl*cs
      a31 = sd*sl
      a13 = ss*sd
      a23 = cs*sd
      a33 = cd
c      
c  transform moment tensor (0,   0,   1,         
c                           0,   0,   0,         
c                           1,   0,   0)         
c                                                
c  and permute axes to (up, south, east) order   
c
      tm(1) = 2*a31*a33
      tm(2) = a11*a33 + a31*a13
      tm(3) = 2*a11*a13
      tm(4) = a21*a33 + a31*a23
      tm(5) = a11*a23 + a21*a13
      tm(6) = 2*a21*a23
c
      return
      end
c
c
c
c

