PROGRAM main
    USE stats 
    USE heaps
    IMPLICIT NONE
    REAL, DIMENSION(:),ALLOCATABLE :: vals,heap
    INTEGER :: nitems,i
    REAL :: mean_val

    CALL READ_ARR(vals,nitems)
    ALLOCATE(heap(1:nitems))
    CALL HEAP_SORT(vals,heap,1,nitems)
    WRITE(*,*) heap
    if(ALLOCATED(vals)) DEALLOCATE(vals)


CONTAINS
!------------------------------------------------
!reads in a 1d float array from the command line
!------------------------------------------------
    SUBROUTINE READ_ARR(arr, n)
      IMPLICIT NONE
      REAL, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: arr
      INTEGER, INTENT(OUT) :: n
      INTEGER :: i
      CHARACTER(30) :: arg
      REAL :: arg_value
      n = COMMAND_ARGUMENT_COUNT()
      IF (n .EQ. 0) THEN
          CALL GET_COMMAND_ARGUMENT(0,arg)
          WRITE(*,*) "usage: ", arg, " <list of numbers>"
          CALL EXIT(1)
      ENDIF
      ALLOCATE(arr(1:n))
      DO i = 1, n
         CALL GET_COMMAND_ARGUMENT(i,arg)
         READ(arg,*) arg_value
         arr(i:i) = arg_value 
      ENDDO 
    END SUBROUTINE READ_ARR
END PROGRAM MAIN
