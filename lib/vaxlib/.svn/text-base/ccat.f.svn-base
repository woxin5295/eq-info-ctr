c COMPUTE CORRECTED ARRIVAL TIME
c GIVEN PARAMETERS FROM A HYPOELLIPSE ARCHIVE PHASE FILE, COMPUTE
c THE ARRIVAL TIME CORRECTED FOR SATELLITE DELAY.
c THIS IS NECESSARY BECAUSE THE SATELLITE DELAY IS NOT ON THE
c ARCHIVE PHASE RECORD.
      subroutine ccat(atime, cat, cptt, elvdy, ot, res, stdly, vpvs)
c             ATIME   (INPUT) ACTUAL ARRIVAL TIME, NOT CORRECTED FOR SAT
c DELAY
      real atime
c             CAT     (OUTPUT) ARRIVAL TIME CORRECTED FOR SATELLITE DELA
cY
      real cat
c             CAT1    APPROXIMATE ARRIVAL TIME CORRECTED FOR SATELLITE D
cLY
      real cat1
c             CPTT    (INPUT) COMPUTED P TRAVEL TIME
      real cptt
c             ELVDY   (INPUT) ELEVATION DELAY
      real elvdy
c             OT      (INPUT) ORIGIN TIME
      real ot
c             RES     (INPUT) RESIDUAL (OBSERVED - COMPUTED)
      real res
c             STDLY    (INPUT) STATION DELAY
      real stdly
c             VPVS    (INPUT) VPVS RATIO (SET = 1.0 FOR P ARRIVALS)
c
c     GET OBSERVED ARRIVAL TIME, CORRECTED FOR SATELLITE DELAY
c     APPROXIMATE CORRECTED ARRIVAL TIME = COMPUTED TT + 
c                    ORIGIN TIME + ELVDY + STATION DELAY + RESIDUAL 
c     PRINT *, 'ATIME, CAT, CPTT, ELVDY, OT, RES, STDLY, VPVS'
c     PRINT *, ATIME, CAT, CPTT, ELVDY, OT, RES, STDLY, VPVS
      real vpvs
c     PRINT *, 'APPROX CORRECTED ARRIVAL TIME = ', CAT1
c     THE DIFFERENCE ATIME - CAT1 SHOULD BE N*0.27, WHERE N IS
c     THE NUMBER OF SATELLITE HOPS ON THE PHONE LINE.
# 31 "ccat.for"
  100 cat1 = ((((cptt * vpvs) + ot) + (elvdy * vpvs)) + stdly) + res
# 35 "ccat.for"
      hop = (atime - cat1) / 0.27
c     CORRECT ARRIVAL TIME FOR NHOP HOPS
# 36 "ccat.for"
      nhop = iround(hop)
c     PRINT *, 'FINAL OBSERVED ARRIVAL TIME CORRECTED FOR ', NHOP
c     PRINT *, ' HOPS = ', CAT
# 38 "ccat.for"
      cat = atime - (nhop * 0.27)
# 41 "ccat.for"
      return 
      end
