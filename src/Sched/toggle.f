      SUBROUTINE TOGGLE( VAL, ISCN, KEY1, KEY2, UNSET, KEYVAL, KC, KI )
C
C     Routine for SCHED for cases of pairs of logical inputs that
C     can be used to turn some function on and off.  The variables
C     should have been set to UNSET before each read.
C
C     If the value of KEY1 has been set (VALUE=0.D0), VAL is true.
C     If the value of KEY2 has been set, VAL is false.
C     If the value of KEY1 was set to a non-zero value, VAL is false.
C     If the value of KEY2 was set to a non-zero value, VAL is true.
C     If both are set to anything, someone is confused so stop.
C     The default is false.
C     After scan 1, default to the previous scan.
C
      LOGICAL        VAL(*)
      INTEGER        ISCN, KI(*), KEYPTR
      CHARACTER      KEY1*(*), KEY2*(*), KC(*)*(*)
      CHARACTER      OKEY1*8, OKEY2*8
      DOUBLE PRECISION   DVAL1, DVAL2, KEYVAL(*), UNSET
C -------------------------------------------------------------------
C
      DVAL1 = KEYVAL( KEYPTR( KEY1, KC, KI ) )
      DVAL2 = KEYVAL( KEYPTR( KEY2, KC, KI ) )
C
      IF( DVAL1 .NE. UNSET .AND. DVAL2 .NE. UNSET ) THEN
         OKEY1 = KEY1
         OKEY2 = KEY2
         CALL ERRLOG( 'TOGGLE: You can''t set both ' // OKEY1 //
     1           ' and ' // OKEY2 // '!' )
C
      ELSE IF( DVAL1 .EQ. UNSET .AND. DVAL2 .EQ. UNSET ) THEN
         IF( ISCN .EQ. 1 ) THEN
            VAL(ISCN) = .FALSE.
         ELSE
            VAL(ISCN) = VAL(ISCN-1)
         END IF
C
      ELSE IF( DVAL1 .NE. UNSET ) THEN
         VAL(ISCN) = DVAL1 .EQ. 0.D0
C
      ELSE IF( DVAL2 .NE. UNSET ) THEN
         VAL(ISCN) = DVAL2 .NE. 0.D0
C
      END IF
C
      RETURN
      END
