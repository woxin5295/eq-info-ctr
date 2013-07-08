	subroutine tenxvec(tensor, avector, outvec)
c  multiply a vector with a tensor
	dimension tensor(3,3), avector(3), outvec(3)

	do 20 i = 1, 3
	outvec(i) = 0.0
	  do 10 j = 1, 3
	    outvec(i) = outvec(i) + tensor(i, j)*avector(j)
10	  continue
20	continue

	return
	end
