c 	  Fortran subroutine for the game of life in R
c	  Written by Fabien Fivaz

      subroutine life(m, x, y)

	  integer x,y
	  interger m(x, y)
	  integer i,j,k
	  integer sr
	  real ar(16)
	  parameter(ar = (-1,-1,-1,0,-1,1,0,-1,0,1,1,-1,1,0,1,1))

      do 40 i = 2, x-1
	     do 30 j = 2, y-1
	        if (mat(i,j) .EQ. 1) then
	           sr = 0
	           do 10 k = 1, 16
	              sr = sr + mat(i+ar(k),j+ar(k+1))
   10          continue
	           if (sr .LT. 2) then
	              mat(i,j) = 0
	           endif
	           if (sr .EQ. 2) then
	              mat(i,j) = 1
	           endif
	           if (sr .EQ. 3) then
	              mat(i,j) = 1
	           endif
	           if (sr .GT. 3) then
	              mat(i,j) = 0
	           endif
	        endif
            if (mat(i,j) .EQ. 0) then
               sr = 0
	           do 20 k = 1, 16
	              sr = sr + mat(i+ar(k),j+ar(k+1))
   20          continue
               if (sr = 3) then
                  mat(i,j) = 1
               endif
            endif               
   30    continue
   40 continue