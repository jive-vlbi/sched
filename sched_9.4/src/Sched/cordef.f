      SUBROUTINE CORDEF( CHPOL )
C
C     Deal with correlator setup parameters for the non-VLBA 
C     correlators.  Mainly, this is just setting the defaults if 
C     values have not been set.
C
      INCLUDE 'sched.inc'
C
      INTEGER       LEN1
      LOGICAL       MISCOR
      CHARACTER     CHPOL*(*)
C --------------------------------------------------------------------      
      IF( DEBUG ) CALL WLOG( 0, 'CORDEF starting.' )
      MISCOR = .FALSE.
C
      IF( CORAVG .EQ. 0.0 ) THEN
         CORAVG = 2.0
      END IF
C
      IF( CORCHAN .EQ. 0 ) THEN
         CORCHAN = 16
      END IF
C
      IF( CORNANT .EQ. 0 ) THEN
         CORNANT = NSTA
      END IF
C
      IF( CHPOL(1:2) .NE. 'ON ' .AND. CHPOL(1:3) .NE. 'OFF' ) THEN
         CHPOL = 'ON'
         CORPOL = .TRUE.
      END IF
C
      IF( CORWTFN .NE. 'UNIFORM' .AND. CORWTFN .NE. 'HANNING' .AND.
     1    CORWTFN .NE. 'QANNING' .AND. CORWTFN .NE. 'ZEROPAD' ) THEN
         CALL WLOG( 1, '        Unrecognized correlator weighting'//
     1      ' function: ' // CORWTFN )
      END IF
C
      IF( CORTAPE(1:3) .NE. 'DAT' .AND. 
     1    CORTAPE(1:4) .NE. 'NONE' .AND.
     2    CORTAPE(1:3) .NE. 'FTP' ) THEN
         MISCOR = .TRUE.
         CALL WLOG( 1, '        Unrecognized correlator distribution'
     1    //' media: '// CORTAPE )
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
         CALL WLOG( 1, 'CORDEF:  A required correlator parameter '//
     1      'not provided for a non-VLBA correlator.' )
C
C        Allow plotting, but not output files.
C
         IF( PLOT ) THEN
            MISSING = .TRUE.
         ELSE
            CALL ERRLOG( 'Add correlator info and try again.' )   
         END IF
      END IF
C
      RETURN
      END
