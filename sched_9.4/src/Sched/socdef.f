      SUBROUTINE SOCDEF( CHPOL )
C
C     A routine for SCHED, called by GETCOR, that checks that the
C     required correlator parameters have been specified for the
C     VLBA correlator in Socorro.  SCHED intentionally forces the
C     user to think about the values of these parameters rather than
C     just setting reasonable defaults.  This is for both the original
C     hardward correlator and the DiFX software correlator.
C
      INCLUDE 'sched.inc'
C
      INTEGER    LEN1
      LOGICAL    MISCOR
      CHARACTER  CHPOL*(*), PRTPOL*3
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SOCDEF starting.' )
      MISCOR = .FALSE.
C
      IF( CORAVG .EQ. 0.0 ) THEN
         MISCOR = .TRUE.
         CALL WLOG( 1, '        Correlator average time missing.' )
      END IF
C
      IF( CORCHAN .EQ. 0 ) THEN
         MISCOR = .TRUE.
         CALL WLOG( 1, '        Number of correlator spectral '//
     1        'channels not specified' )
      END IF
C
      IF( CORNANT .EQ. 0 ) THEN
C        Don't make this one fatal.         MISCOR = .TRUE.
         CORNANT = NSTA
         CALL WLOG( 1, '        Had to set the number of antennas '//
     1      'to be correlated to the number in the schedule.' )
      END IF
C
      IF( CHPOL(1:2) .NE. 'ON ' .AND. CHPOL(1:3) .NE. 'OFF' ) THEN
         MISCOR = .TRUE.
         PRTPOL = CHPOL
         CALL WLOG( 1, '        Invalid polarization spec: ' // 
     1         PRTPOL )
      END IF
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
         CALL WLOG( 1, 'GETCOR:  Correlator parameters are '//
     1      'required for VLBI' )
         CALL WLOG( 1, '         observations to be processed in '//
     1          'Socorro.' )
         CALL WLOG( 1, '         The defaults are only used for '//
     1          'projects to be processed elsewhere.' )
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
C
      RETURN
      END
