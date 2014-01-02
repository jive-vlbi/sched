      SUBROUTINE SDOPINCR
C
C     Get defaults for DOPINCR (kHz).  Use 15.625 kHz for RDBE_DDC.
C     Use DOPINCR=32000.D0,16000.D0 for RDBE_PFB.  For all else
C     use 10kHz.  Worry about the cases where more than one are
C     present.  Note NSETUP was set in the call to DEFSET above.
C     STASCN is set in GETSTA called by SCHIN.  While DOPINCR can
C     be scan dependent, use a default that is the same for the
C     whole observation.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISCN, ISTA, KS
      LOGICAL    GOTDDC, GOTPFB, GOTOTHER, ANYDOP
C -------------------------------------------------------------------
      GOTDDC = .FALSE.
      GOTPFB = .FALSE.
      GOTOTHER = .FALSE.
      ANYDOP = .FALSE.
      DO ISCN = SCAN1, SCANL
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               KS = NSETUP(ISCN,ISTA)
               IF( DBE(KS) .EQ. 'RDBE_DDC' ) THEN
                  GOTDDC = .TRUE.
               ELSE IF( DBE(KS) .EQ. 'RDBE_PFB' ) THEN
                  GOTPFB = .TRUE.
               ELSE
                  GOTOTHER = .TRUE.
               END IF
            END IF
            IF( DOPCAL(ISCN) ) ANYDOP = .TRUE.
         END DO
      END DO
C
C     Only worry about this if there was some Doppler setting.
C
      IF( ANYDOP ) THEN
C
C        Complain if the PFB was used as the offset DOPINCR(ISCN,2)
C        cannot be set reliably without knowing the FIRSTLO, and
C        that could vary in global runs and by scan.  In principle,
C        some cases could be handled, but that is too much trouble.  
C        Don't default DOPINCR for the PFB.  It shouldn't be used for
C        DOPPLER anyway.
C
         IF( GOTPFB ) THEN
          DO ISCN = SCAN1, SCANL
           IF( DOPCAL(ISCN) .AND. DOPINCR(ISCN,1) .EQ. 0.D0 ) THEN
C
C           Issue warnings and die.
C
            CALL WLOG( 1, 'DEFAULTS:  You are asking for DOPPLER '//
     1                    'setting with an observation that' )
            CALL WLOG( 1, '           includes stations using ' //
     1                    'the RDBE_PFB and without '//
     2                    'setting DOPINCR.' )
            CALL WLOG( 1, '           Using DOPPLER with the PFB '//
     1                    ' is a bad idea and doing it without ' )
            CALL WLOG( 1, '           DOPINCR is not allowed. '//
     1                    'It is too complicated to set defaults' )
            CALL WLOG( 1, '           as the required value of ' //
     1                    'DOPINCR(2) depends on the LO setup. ' )
            CALL ERRLOG( 'Change DBE or set DOPINCR ' )
C     
           END IF
          END DO
         ELSE IF( GOTDDC .AND. GOTOTHER ) THEN
            DO ISCN = SCAN1, SCANL
               IF( DOPINCR(ISCN,1) .EQ. 0.D0 ) THEN
                  DOPINCR(ISCN,1) = 250.D0
                  DOPINCR(ISCN,2) = 0.D0
               END IF
            END DO
         ELSE IF( GOTDDC ) THEN
            DO ISCN = SCAN1, SCANL
               IF( DOPINCR(ISCN,1) .EQ. 0.D0 ) THEN
                  DOPINCR(ISCN,1) = 15.625D0
                  DOPINCR(ISCN,2) = 0.D0
               END IF
            END DO
         ELSE  !  (GOTOTHER)
            DO ISCN = SCAN1, SCANL
               IF( DOPINCR(ISCN,1) .EQ. 0.D0 ) THEN
                  DOPINCR(ISCN,1) = 10.D0
                  DOPINCR(ISCN,2) = 0.D0
               END IF
            END DO
         END IF
      END IF
C
      RETURN
      END
