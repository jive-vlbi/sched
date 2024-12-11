      SUBROUTINE CORDEF( CHPOL )
C
C     Deal with correlator setup parameters for the non-VLBA 
C     correlators.  Mainly, this is just setting the defaults if 
C     values have not been set.  It also checks the correlator
C     settings.
C
      INCLUDE 'sched.inc'
C
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
