PROGRAM main
    USE stats
    IMPLICIT NONE
    INTEGER,PARAMETER :: MAX_ITEMS = 100
    REAL, DIMENSION(MAX_ITEMS) :: vals
    INTEGER :: nitems
    REAL :: mean_val

    CALL READ_ARR(vals,nitems)
    mean_val = MEAN(vals,1,nitems)
    WRITE(*,*) mean_val



CONTAINS
!------------------------------------------------
!reads in a 1d float array from the command line
!------------------------------------------------
    SUBROUTINE READ_ARR(arr, n)
      IMPLICIT NONE
      REAL, DIMENSION(:), INTENT(OUT) :: arr
      INTEGER, INTENT(OUT) :: n
      INTEGER :: i
      CHARACTER(30) :: arg
      REAL :: arg_value
      n = COMMAND_ARGUMENT_COUNT()
      IF (n .EQ. 0) THEN
          WRITE(*,*) "usage: a.out <list of values>"
          CALL EXIT(1)
      ENDIF
      DO i = 1, n
         CALL GET_COMMAND_ARGUMENT(i,arg)
         READ(arg,*) arg_value
         arr(i:i) = arg_value 
      ENDDO 
    END SUBROUTINE READ_ARR
END PROGRAM MAIN
