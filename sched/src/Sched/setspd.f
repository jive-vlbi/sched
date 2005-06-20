      SUBROUTINE SETSPD( KS ) 
C
C     Set the tape speeds, if not already specified, based on the
C     recording format and the sampler rate.  Also warn if specified
C     speeds are non-standard.  Note that the validity of the format
C     will be checked in CHKSET.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    KS
C  -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETSPD starting.' )
C
C     Now get the tape speeds for each type of recording.
C     The VLBA and Mark III/IV formats use slightly different speeds
C     for thick tapes for historical reasons.
C
C     Mark III, Mark IV, and VLBA formats.
C
      IF( FORMAT(KS) .EQ. 'MARKIII' .OR.
     1    FORMAT(KS)(1:4) .EQ. 'MKIV' .OR.
     2    FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
C
C        High density tapes are same speed for all.
C
         IF( SPEEDH(KS) .EQ. 0.0 ) THEN
            SPEEDH(KS) = 160.0 * TBPS(KS) / 8.0
         END IF
C
C        Low density are different for VLBA control software.
C  
         IF( SPEEDL(KS) .EQ. 0.0 ) THEN
            IF( FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
               SPEEDL(KS) = 266.66 * TBPS(KS) / 8.0
            ELSE
               SPEEDL(KS) = 270.00 * TBPS(KS) / 8.0
            END IF
         END IF
C
C     S2 - the tape length will be set to the tape time in seconds.
C
      ELSE IF( FORMAT(KS)(1:2) .EQ. 'S2' ) THEN
         IF( SPEEDL(KS) .EQ. 0.0 ) SPEEDL(KS) = 6.3
         IF( SPEEDH(KS) .EQ. 0.0 ) SPEEDH(KS) = 4.2
C
      END IF
C
      RETURN
      END



