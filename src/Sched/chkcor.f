      SUBROUTINE CHKCOR
C
C     Routine to check correlator parameters common to all 
C     correlators.  The code was extracted from SOCDEF and CORDEF.
C
      INCLUDE 'sched.inc'
C
      INTEGER    LEN1
      LOGICAL    MISCOR
C -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKCOR starting.' )
      MISCOR = .FALSE.
C
C     Check the weighting function.  I'm not sure this is actually
C     being used.
C
      IF( CORWTFN .NE. 'UNIFORM' .AND. CORWTFN .NE. 'HANNING' .AND.
     1    CORWTFN .NE. 'QANNING' .AND. CORWTFN .NE. 'ZEROPAD' ) THEN
         CALL WLOG( 1, '        Unrecognized correlator weighting'//
     1      ' function: ' // CORWTFN )
      END IF
C
C     Type of media for distribution of correlator output.
C     Most is now done by Internet (FTP or NONE).
C     Remove EXABYTE.  Long ago, removed 9 track tape.
C     Probably add flash memory and hard drives eventually.
C     Never really got into CD or DVD.
C
      IF( CORTAPE(1:3) .NE. 'DAT' .AND. 
     1    CORTAPE(1:4) .NE. 'NONE' .AND.
     2    CORTAPE(1:4) .NE. 'DISK' .AND.
     3    CORTAPE(1:3) .NE. 'FTP' ) THEN
         MISCOR = .TRUE.
         CALL WLOG( 1, '        Unrecognized correlator distribution'
     1    //' media: '// CORTAPE )
      END IF
C
      IF( CORDFMT(1:4) .NE. 'FITS' .AND.
     1    CORDFMT(1:5) .NE. 'MARK4' ) THEN
         MISCOR = .TRUE.
         CALL WLOG( 1, '        Unrecognized correlator distribution'
     1    //' format: '// CORDFMT )
      END IF
C
C     Check that a shipping address was provided for the media that 
C     need to be shipped.
C
      IF( LEN1( CORSHIP(1) ) .EQ. 0 .AND.  
     1          CORTAPE(1:3) .EQ. 'DAT' ) THEN
         MISCOR = .TRUE.
         CALL WLOG( 1, '        Missing distribution tape shipping'//
     1         ' address.' )
      END IF
C
C     Deal with case where parameters weren't provided.
C
      IF( MISCOR ) THEN
         CALL WLOG( 2, ' ' )
         CALL WLOG( 1, 'CHKCOR:  A required correlator parameter '//
     1      'was not provided or had a bad value.' )
C
C        Allow plotting, but not output files.
C
         IF( PLOT ) THEN
            MISSING = .TRUE.
         ELSE
            CALL ERRLOG( 'Fix the correlator info and try again.' )   
         END IF
      END IF
C
      RETURN
      END
