      subroutine csort(cx, ix, n)
c
c  indirect sort routine from meissner & organick, p.352
c  stores ascending sort order of cx in array ix, leaving cx unchanged
c
      integer           ix(*)                           ! (output) pointer array to sorted order
      character*(*)     cx(*)                           ! (input) array to be sort
      integer           n                               ! (input) number of elements to be sorted
c
      integer           i                               ! loop index
      integer           j                               ! loop index
      integer           next                            ! index into cx
c
      do 10 i = 1, n
        ix(i) = i
10    continue
c
      do 40 j = 1, n - 1
        next = ix(j + 1)
        do 20 i = j, 1, -1
          if (cx(next) .gt. cx(ix(i))) goto 30
          ix(i + 1) = ix(i)
20      continue
30    ix(i + 1) = next
40    continue
c
      return
      end
c
c
c
c
c

