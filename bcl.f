      subroutine bcl(m, x, y)

      integer x, y
      integer m(x, y)
      integer i, j, k
      integer sr
      complex ar(16)
      ar = (/ -1, -1, -1, 0, -1, 1, 0, -1, 0, 1, 1, -1, 1, 0, 1, 1 /)

      do 20 i = 2, x - 1
         do 10 j = 2, y - 1
            if (m(i,j) .EQ. 1) then
               sr = 0
               sr = sr + m(i - 1, j - 1)
               sr = sr + m(i - 1, j)
               sr = sr + m(i - 1, j + 1)
               sr = sr + m(i, j - 1)
               sr = sr + m(i, j + 1)
               sr = sr + m(i + 1, j - 1)
               sr = sr + m(i + 1, j)
               sr = sr + m(i + 1, j + 1)
               if (sr .LT. 2) then
                  m(i,j) = 0
               endif
               if (sr .EQ. 2) then
                  m(i,j) = 1
               endif
               if (sr .EQ. 3) then
                  m(i,j) = 1
               endif
               if (sr .GT. 3) then
                  m(i,j) = 0
               endif
            endif
            if (m(i,j) .EQ. 0) then
               sr = 0
               sr = sr + m(i - 1, j - 1)
               sr = sr + m(i - 1, j)
               sr = sr + m(i - 1, j + 1)
               sr = sr + m(i, j - 1)
               sr = sr + m(i, j + 1)
               sr = sr + m(i + 1, j - 1)
               sr = sr + m(i + 1, j)
               sr = sr + m(i + 1, j + 1) 
               if (sr .EQ. 3) then
                  m(i,j) = 1
               endif
            endif               
   10    continue
   20 continue

      end