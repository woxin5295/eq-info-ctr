	subroutine crossprod(a, b, c)
c compute cross product of axb
	dimension a(3), b(3), c(3)
c compute cross product
        c(1) =  a(2)*b(3) - b(2)*a(3)
        c(2) = -a(1)*b(3) + b(1)*a(3)
        c(3) =  a(1)*b(2) - b(1)*a(2)

	return
	end
