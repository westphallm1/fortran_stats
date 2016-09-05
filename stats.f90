MODULE stats
    USE heaps
    IMPLICIT NONE
    INTEGER, PARAMETER :: MEDIAN_WIDTH = 5
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

!-----------------------------
!Compute median of array
!Uses heapsort w/ maxheap
!-----------------------------
    REAL FUNCTION MEDIAN(arr,start,finish)
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(IN) :: arr
        REAL, DIMENSION(:), ALLOCATABLE :: heap
        INTEGER, INTENT(IN) :: start, finish
        INTEGER i
        ALLOCATE(heap(start:finish))
        IF((ALLOCATED(heap))) THEN
            MEDIAN = 5
        ELSE
            MEDIAN = -1
        ENDIF
    END FUNCTION MEDIAN

END MODULE stats
