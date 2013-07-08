c
c  PROGRAM TO STRIP COMMENT FROM END OF CHARACTER VARIABLE.
c  A COMMENT BEGINS WITH A ';'
c
c  ARRAY - A CHARACTER VARIABLE
      character *(*)function stpcom(array)
c
c  GET LENGTH OF ARRAY
c
      character array*(*)
# 12 "stpcom.for"
      n = len(array)
      stpcom = array
c
c  FIND THE POSITION OF THE FIRST ; CHARACTER FROM THE LEFT
c
# 14 "stpcom.for"
      if (n .eq. 0) return 
# 18 "stpcom.for"
      do 10 i = 1, n
      if (array(i:i) .eq. ';') goto 20
c
c  NO ;'S FOUND, SO RETURN
c
# 20 "stpcom.for"
   10 continue
c
c  BLNAK OUT ; AND ALL TO THE RIGHT OF IT
c
# 24 "stpcom.for"
      return 
# 28 "stpcom.for"
   20 stpcom(i:n) = ' '
      return 
      end
