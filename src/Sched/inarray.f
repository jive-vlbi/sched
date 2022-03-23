C ======================================
      LOGICAL FUNCTION INARRAY(ELEM, ARRAY, LENGTH)
C
C Check if element is in array
C Original: Cormac Reynolds 2017-06-22 (used by chkdbfq.f)
C
      INTEGER   LENGTH, I
      CHARACTER ELEM*(*), ARRAY(LENGTH)*(*)
C
      INARRAY = .FALSE.
      DO I = 1, LENGTH
        IF( ARRAY(I) .EQ. ELEM ) INARRAY = .TRUE.
      END DO
C
      RETURN
      END
