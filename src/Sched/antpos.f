      SUBROUTINE ANTPOS( KSTA, UPIN, SDEC, SAZ, SEL, SHA, TAX1, TAX2 )
C
C     Routine for SCHED subroutine SLEW that determines where an
C     antenna is actually pointed taking into account the hardware
C     limits.  Mostly, it decides where to go to get this information
C     depending on mount type.
C
      INCLUDE 'sched.inc'
C
C     The "S" parameters are the source location.
C     The "T" parameters are the telescope position.
C     TAX1 and TAX2 are the first and second axis coordinates in
C     degrees (HA is converted).  They will be used later by SLEW 
C     with the rates to get slew times.
C
C     Az, El, X, Y, and Dec are in degrees.  Ha is in hours.
C
      INTEGER           KSTA
      REAL              SAZ, SEL, SHA, SDEC
      REAL              TAX1, TAX2
      CHARACTER         UPIN*1, UP*1
C -----------------------------------------------------------------
C
      IF( MOUNT(KSTA) .EQ. 'ALTAZ' ) THEN
         CALL MTALTAZ( KSTA, UP, SAZ, SEL, TAX1, TAX2 )
      ELSE IF( MOUNT(KSTA) .EQ. 'EQUAT' ) THEN
         CALL MTEQUAT( KSTA, UP, SEL, SHA, SDEC, TAX1, TAX2 )
      ELSE IF( MOUNT(KSTA) .EQ. 'XYNS' ) THEN
         CALL MTXYNS( KSTA, UP, SAZ, SEL, TAX1, TAX2 )
      ELSE IF( MOUNT(KSTA) .EQ. 'XYEW' ) THEN
         CALL MTXYEW( KSTA, UP, SAZ, SEL, TAX1, TAX2 )
      END IF
C
      IF( UPIN .NE. UP .AND. SEL .NE. 90.0 .AND. UPIN .NE. 'H' ) THEN
         CALL ERRLOG( 'ANTPOS: Consistency check fails '//
     1       UP//' '//UPIN//' '//STATION(KSTA)//' '//MOUNT(KSTA) )
      END IF
C
      RETURN
      END
