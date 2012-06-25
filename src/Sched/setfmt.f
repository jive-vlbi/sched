      SUBROUTINE SETFMT( KS, TRKBPS, OK )
C
C     Routine for SCHED called by SETFORM that sets the format 
C     for VLBA and MKIV setups.  The parameters that it tries
C     to set are FORMAT, SPEEDUP, FANOUT, and TAPEMODE.
C
C     KS is the setup group.
C     TRKBPS is the track bit rate to try to match.
C     OK is output indicating if format actually got set.
C
      INCLUDE        'sched.inc'
      INCLUDE        'schset.inc'
C
      INTEGER        KS
      REAL           TRKBPS
      LOGICAL        OK
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETFMT starting' ) 
      OK = .FALSE.
      TBPS(KS) = 0
C
C     Recall that the base FORMAT name has been set by now.
C     Call individual routines to deal with specific formats.
C     If the FORMAT etc have already been set, these routines
C     will just check that TRKBPS is SAMPRATE / FANOUT(KS)
C     and return OK.
C
      IF( FORMAT(KS) .EQ. 'S2' ) THEN
         CALL FMTS2( KS, OK ) 
      ELSE IF( FORMAT(KS) .EQ. 'MARKIII' ) THEN
         CALL FMTMKIII( KS, OK ) 
      ELSE IF( FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
         CALL FMTVLBA( KS, TRKBPS, OK )
      ELSE IF( FORMAT(KS)(1:4) .EQ. 'MKIV' ) THEN
         CALL FMTMKIV( KS, TRKBPS, OK )
      ELSE IF( FORMAT(KS)(1:3) .EQ. 'LBA' .OR. 
     1         FORMAT(KS)(1:6) .EQ. 'MARK5B' .OR.
     2         FORMAT(KS)(1:4) .EQ. 'VDIF' ) THEN
         FANOUT(KS) = 1.0
         SPEEDUP(KS) = 1.0
         TAPEMODE(KS) = 1
         OK = .TRUE.
      ELSE
C
C        Format NONE or MARKII get here.
C        I don't think they need to do anything, but maybe I should
C        set some dummy values.  Use values that would be appropriate
C        for VLBA/MKIV.
C
C         IF( TAPEMODE(KS) .EQ. 0 ) TAPEMODE(KS) = 1
C         FANOUT(KS) = MIN( 4.0, SAMPRATE(KS) / 8.0 )
C         FANOUT(KS) = MAX( 1.0, FANOUT(KS) )
C         SPEEDUP(KS) = 1.0
C
C        Changing my mind.  For format NONE, in the past, TAPEMODE=0
C        Just leave it as is and set FANOUT and SPEEDUP to 0.
C        Need to make sure FANOUT is not being used to indicate that
C        the format is set.
C
         FANOUT(KS) = 0.0
         SPEEDUP(KS) = 0.0
C
         OK = .TRUE.
      END IF
C
      RETURN
      END
