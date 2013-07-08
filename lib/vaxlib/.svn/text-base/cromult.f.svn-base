        subroutine cromult(dd1, da1, dd2, da2, dd3, da3) 
c cross multiply two vectors given in polar coordinates 
        dimension v1(3), v2(3), v3(3) 
        logical flip 

c convert down-dip direction of first plane to vector 
c	print *, 'tr pl = ', dd1, da1
        call polvec(dd1, da1, v1) 
c	print *, 'p vector = ', v1
c convert down-dip direction of second plane to vector 
c	print *, 'tr pl = ', dd2, da2
        call polvec(dd2, da2, v2) 
c	print *, 't vector = ', v2
c compute null vector from cross product 
        call crossprod(v1, v2, v3) 
c	print *, 'cross product = ', v3
c convert null vector from vector to polar coordinates 
        call vecpol(flip, dd3, da3, v3)
c	print *, 'b dip dir and plunge = ', dd3, da3
        return
        end

