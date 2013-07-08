      subroutine pexcf1 (coef, i, mxstat, u)
c index reversed   10/17/86 j. c. lahr
c
c
c calculates coefficients for determining the far-field radiation pattern of p waves from the moment-rate tensor components of a
c point source in an infinite, homogeneous, elastic medium.  the radiation pattern is normalized; to obtain particle amplitudes,
c multiply by
c
c    1.0/(4.0*pi*rho*(v**3)*r),
c
c     where:
c          rho is the density in the source region,
c          v is the p-wave speed in the source region, and
c          r is the geometric spreading factor
c            (for a homogeneous medium, this is equal to the distance
c            to the observation point.)
c
c reference:
c         aki, keiiti, and paul g. richards, quantitative seismology,
c         freeman, san francisco, 1980, equation 49.1, page 118.
c
c written by bruce julian
c
      real              coef(6, mxstat) ! (output) excitation coefficients
      integer           i               ! (input) index of station
      integer           mxstat          ! (input) maximum # of stations permitted
      real              u(3)            ! (input) unit vector in ray direction
c
      coef(1, i) = u(1)*u(1)
      coef(2, i) = 2.*u(1)*u(2)
      coef(3, i) = u(2)*u(2)
      coef(4, i) = 2.*u(3)*u(1)
      coef(5, i) = 2.*u(2)*u(3)
      coef(6, i) = u(3)*u(3)
c
      return
      end
c
c
c
c
c

