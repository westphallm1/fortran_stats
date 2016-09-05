MODULE heaps
    IMPLICIT NONE
CONTAINS
    !-------------------
    !Max heap
    !-------------------
    SUBROUTINE HEAP_ADD(heap,start,nitems,val)
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(OUT) :: heap 
        INTEGER, INTENT(IN) :: start,nitems
        REAL, INTENT(IN) :: val
        REAL :: tmp
        INTEGER :: i, next
        i = nitems+1
        heap(i) = val
        !bubble up the inserted value to the highest possible position
        DO WHILE (i .GT. 1) 
            next = i/2
            tmp = heap(next)
            IF(heap(i) .GT. heap(next)) THEN
                heap(next) = heap(i)
                heap(i) = tmp
            ENDIF
            i = next
        ENDDO
    END SUBROUTINE HEAP_ADD

    SUBROUTINE HEAP_REMOVE(heap,start,nitems,val)
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(OUT) :: heap 
        INTEGER, INTENT(IN) :: start,nitems
        REAL, INTENT(OUT) :: val
        REAL :: tmp
        INTEGER :: i, next
        val = heap(start)
        tmp = heap(start)
        heap(start) = heap(nitems)
        heap(nitems) = tmp 
        i = start
        DO WHILE(i .LE. nitems/2)
           next = i * 2 
           IF((next .LT. nitems).AND.(heap(i) .LT. heap(next))) THEN
               tmp = heap(next)
               heap(next) = heap(i)
               heap(i) = tmp
           ELSE IF((next+1 .LT. nitems) .AND.(heap(i) .LT. heap(next+1))) THEN
               next = next + 1
               tmp = heap(next)
               heap(next) = heap(i)
               heap(i) = tmp 
           ENDIF
           i = next
        ENDDO
   END SUBROUTINE HEAP_REMOVE

   !-----------------
   !Insert all elements of an array into a heap and then remove them
   !-----------------
   SUBROUTINE HEAP_SORT(arr,heap,start,finish)
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(IN)  :: arr
        INTEGER, INTENT(IN) :: start,finish
        REAL, DIMENSION(:), INTENT(OUT) :: heap 
        REAL :: val
        INTEGER :: i,nitems
        nitems = 0 
        !Insert every element of the array into a heap
        DO i =start,finish
            CALL HEAP_ADD(heap, start, nitems, arr(i))
            nitems = nitems + 1 
        ENDDO
        DO WHILE (nitems .GT. 0)
            CALL HEAP_REMOVE(heap, start, nitems, val)
            nitems = nitems - 1
        ENDDO
   END SUBROUTINE HEAP_SORT
END MODULE heaps
