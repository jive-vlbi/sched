      SUBROUTINE ANTPOS( KSTA, ISCN, UPIN, SDEC, SAZ, SEL, SHA, 
     1                   TAX1, TAX2 )
C
C     Routine for SCHED subroutine SLEW that determines where an
C     antenna is actually pointed taking into account the hardware
C     limits.  Mostly, it decides where to go to get this information
C     depending on mount type.  The geometry routines need to be run
C     earlier to get SAZ, SEL, and SHA.  WRAP needs to have been run
C     to be sure SAZ is on the desired wrap.  Note SAZ can, and usually
C     is, on a larger range than 0-360.
C
      INCLUDE 'sched.inc'
C
C     The "S" parameters are the input source location.
C     The "T" parameters are the output telescope position.
C     TAX1 and TAX2 are the first and second axis coordinates in
C     degrees (HA is converted).  They will be used later by SLEW 
C     with the rates to get slew times.
C
C     Az, El, X, Y, and Dec are in degrees.  Ha is in hours.
C
C     UPIN is the UP flag from SCHGEO (called by STAGEO which also
C     calls SLEW, which calls this routine).
C     ISCN is the scan number - used only for the consistency check
C     message.
C
      INTEGER           KSTA, ISCN
      REAL              SAZ, SEL, SHA, SDEC
      REAL              TAX1, TAX2
      CHARACTER         UPIN*1, UP*1, TEXT*100
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
      IF( UPIN .NE. UP .AND. SEL .NE. 90.0 .AND. UPIN .NE. 'H' .AND.
     1     UPIN .NE. 'W' ) THEN
         TEXT = ' '
         WRITE( TEXT, '( A, I4, A, 1X, A, 1X, A, 1X, A )' )
     1           'ANTPOS: Consistency check fails on scan ', ISCN,
     2       UP, UPIN, STATION(KSTA), MOUNT(KSTA)
      END IF
C
      RETURN
      END
