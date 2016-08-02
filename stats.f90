MODULE stats
    IMPLICIT NONE

CONTAINS

!------------------------------
!Compute the mean of the array
!------------------------------
    REAL FUNCTION MEAN(arr,start,finish)
        IMPLICIT NONE
        REAL, DIMENSION(:),INTENT(IN) :: arr
        INTEGER, INTENT(IN) :: start, finish
        INTEGER i
        REAL accum
        DO i = start, finish
            accum = accum + arr(i)
        ENDDO
        MEAN = accum /(1 + finish - start) 
    END FUNCTION MEAN


END MODULE stats
