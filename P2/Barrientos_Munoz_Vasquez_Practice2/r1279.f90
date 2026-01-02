!*******************************************************************************
!	SUBROUTINE ir1279vector(random_vector, no_of_rand)
!
!	Modified by Matteo Palassini
!
!	USE	 r1279block
!	IMPLICIT NONE
!	INTEGER	 no_of_rand, i, random_vector(*)
!	REAL	 ran2	
!	
!	do i = 1, no_of_rand 
!	    ioffset = iand(ioffset + 1, 2047)
!	    irand(ioffset) = (irand(index1(ioffset))*irand(index2(ioffset)))
!	    random_vector(i) = lshift(irand(ioffset), -1)
!	end do
!	
!	END
!	
!*******************************************************************************
FUNCTION r1279()

    IMPLICIT NONE
    INCLUDE "r1279block.h"
    REAL    r1279, inv_max
    REAL    INV_MAXINT 
    PARAMETER (INV_MAXINT = 1.0/2147483647.)

    ioffset = iand(ioffset + 1, 2047)
    irand(ioffset) = (irand(index1(ioffset))*irand(index2(ioffset)))
    r1279 = ishft(irand(ioffset), -1) * INV_MAXINT

END 
