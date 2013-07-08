c
c   error handler (11/22/93 jcl)
c
        integer function ieee_bombout(sig, code, context)
        integer sig, code, context(5)
	print *, 'An ieee math error has occurred!'
        call abort()
        end

