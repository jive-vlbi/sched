      SUBROUTINE SUNWARN( ISCN, DLIM, LINE, IUNIT )
C
C     Routine for SCHED that writes a warning in the operator schedule
C     or the summary file when a source is closer than DLIM to the
C     sun.  GETSUN must have been run earlier to have calculated
C     the sun distance SUNDIS which is in SCHED.INC.
C
C     LINE is the line counter for the print routines.
C
      INCLUDE 'sched.inc'
C
      INTEGER   ISCN, IUNIT, LINE
      REAL      DLIM
C --------------------------------------------------------------------
C     Check the target source.
C
      IF( SUNDIS(SRCNUM(ISCN)) .LT. DLIM ) THEN
         WRITE( IUNIT, '( A, F6.1, A )' ) 
     1      '        Source only ', SUNDIS(SRCNUM(ISCN)),
     2      ' degrees from the Sun.'
         LINE = LINE + 1
      END IF
C
C     Now check the VLA phasing source.  
C
      IF( IVLAPHS(ISCN) .NE. 0 .AND. IVLAPHS(ISCN) .NE. SRCNUM(ISCN) )
     1     THEN
         IF( SUNDIS(IVLAPHS(ISCN)) .LT. DLIM ) THEN
            WRITE( IUNIT, '( A, F6.1, A )' ) 
     1      '        VLA phasing source only ', SUNDIS(IVLAPHS(ISCN)),
     2      ' degrees from the Sun.'
            LINE = LINE + 1
         END IF
      END IF
C
C     The dop source doesn't matter.
C
      RETURN
      END
