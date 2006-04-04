      SUBROUTINE CHKSC1
C
C     Routine for SCHED called by the main routine that checks scan 
C     parameters before SCHOPT.  The main scan checker CHKSCN is
C     called after SCHOPT and all scan details are known.  But the
C     scan optimization routines can get confused by some problems
C     so catch them here.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    KSCN, LSCN, KSTA, KS, LEN1
      LOGICAL    GOTERR, AUTOWARN
C ----------------------------------------------------------------------
      GOTERR = .FALSE.
C
C     Loop over scans.
C
      DO KSCN = 1, NSCANS
C
C        Look for attempts to use ephemeris or satellite at stations
C        where this shouldn't be done.  Usually this is for stations
C        for which we don't know how to specify position rates.
C
         DO KSTA = 1, NSTA
            IF( STASCN(KSCN,KSTA) ) THEN
               IF( PLANET(SRCNUM(KSCN)) .OR. SATEL(SRCNUM(KSCN)) ) THEN
                  IF( CONTROL(STANUM(KSTA)) .NE. 'VLBA' .AND.
     1                .NOT. VLBADAR(STANUM(KSTA)) .AND.
     2                CONTROL(STANUM(KSTA)) .NE. 'VLA' ) THEN
                     GOTERR = .TRUE.
                     CALL WLOG( 1, 'CHKSC1: Trying to use ephemeris '//
     1                    'or satellite position for '//SCNSRC(KSCN) )
                     CALL WLOG( 1, '        at '//STATION(STANUM(KSTA)) 
     1                    //' but SCHED doesn''t know how.' )
                  END IF
               END IF
            END IF
         END DO
C
      END DO
C
C     If got an error, quit.
C
      IF( GOTERR ) THEN
         CALL ERRLOG( 'CHKSC1: Errors found.' )
      END IF
C
C     Look for FORMAT='NONE' on recording scans.  This format should
C     be allowed on non-recording scans.
C
      IF( .NOT. ( NOSET .OR. NOTAPE ) ) THEN
         DO KSCN = 1, NSCANS
            DO KSTA = 1, NSTA
               IF( STASCN(KSCN,KSTA) ) THEN
                  KS = NSETUP(KSCN,KSTA)
                  IF( FORMAT(KS) .EQ. 'NONE' .AND. .NOT. NOREC(KSCN) ) 
     1                THEN
                     WRITE( MSGTXT, '( A, A )' ) 
     1                     'CHKSC1:  *****  Format NONE specified ',
     2                     'for a recording scan.  Not allowed!'
                     CALL WLOG( 1, MSGTXT )
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A )' )
     1                  '                Scan and setup information:'
                     CALL WLOG( 1, MSGTXT )
                     MSGTXT = ' '
                     CALL PRTSCN( KSCN )
                     CALL PRTSET( KS, ILOG )
                     CALL WLOG( 1, ' ' )
                     CALL WLOG( 1, 'CHKSC1:  See the error message '//
     1                     'before the scan and setup information.' )
                     CALL ERRLOG( 
     1                     'CHKSC1: Switch to a recording format.' )
                  END IF
               END IF
            END DO
C     C  
         END DO
      END IF
C
C     Check that there are at least some record scans if using a
C     recording format.
C
      IF( VLBITP ) THEN
         DO KSCN = 1, NSCANS
            IF( .NOT. NOREC(KSCN) ) GO TO 100
         END DO
         CALL WLOG( 1, 'CHKSC1:   This schedule claims to involve ' //
     1       'recording but' )
         CALL WLOG( 1, '          there are no recording scans.' )
         CALL WLOG( 1, '          Check OBSTYPE and RECORD/NORECORD' )
         CALL ERRLOG( 'Fix key file and try again.' )
      END IF
  100 CONTINUE
C
C     Check for improper use of automatic tape allocation.
C
      IF( .NOT. ( NOTAPE .OR. MARK2 ) ) THEN
C
C        Warn against non-use of AUTOTAPE=2 (autoallocation) for 
C        observations with VLBA antennas to be correlated in Socorro.
C
         IF( CORREL(1:4) .EQ. 'VLBA' .OR. CORREL(1:7) .EQ. 'SOCORRO' )
     1       THEN
            AUTOWARN = .TRUE.
            DO KSTA = 1, NSTA
               IF( STANAME(KSTA)(1:4) .EQ. 'VLBA' .AND. AUTOWARN ) THEN
                  IF( ( .NOT. AUTOALOC(KSTA) .OR. .NOT. AUTOREV(KSTA) )
     1                .AND. .NOT. TWOHEAD ) THEN
                     CALL WLOG( 0, ' ' )
                     CALL WLOG( 0, 
     1                 'CHKSC1: *** Shouldn''t you be using ' //
     2                 'AUTOTAPE=2 (VLBA stations, VLBA correlator).' )
                     AUTOWARN = .FALSE.
                  END IF
               END IF
            END DO
C
C        Don't use automatic allocation for correlators that don't
C        want it.  Make this a fatal error.
C
         ELSE IF( CORREL(1:4) .EQ. 'JIVE' ) THEN
            DO KSTA = 1, NSTA
               IF( AUTOALOC(KSTA) .OR. AUTOREV(KSTA) ) THEN
                  CALL WLOG( 1, ' ' )
                  CALL WLOG( 1, 'CHKSC1: *** Automatic tape ' //
     1                   'allocation cannot be used for ***' )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 3A )' ) 
     1               '        *** observations to be processed at ',
     2               CORREL(1:LEN1(CORREL)), '  *** '
                  CALL WLOG( 1, MSGTXT )
                  CALL ERRLOG( 'CHKSC1: Change AUTOTAPE or CORREL' )
               END IF
            END DO
C
C        Some correlators will take it, but it is not a real natural
C        for them. 
C
         ELSE IF( CORREL(1:8) .EQ. 'HAYSTACK' ) THEN
            AUTOWARN = .TRUE.
            DO KSTA = 1, NSTA
               IF( ( AUTOALOC(KSTA) .OR. AUTOREV(KSTA) ) .AND. 
     1                    AUTOWARN ) THEN
                  CALL WLOG( 1, ' ' )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 3A )' ) 
     1               'CHKSC1: *** Autoallocate requested with ',
     2               'processing at ', CORREL(1:LEN1(CORREL))
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 2A )' ) 
     1               '            This is allowed, but will ',
     2               'cause cause some complications'
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 2A )' ) 
     1               '            for correlation.  ',
     2               'It does help VLBA operations.'
                  CALL WLOG( 1, MSGTXT )
                  AUTOWARN = .FALSE. 
               END IF
            END DO
C
C        For others correlators, don't prohibit it, but warn that it
C        might cause problems.  For one example, Haystack is willing
C        to deal with autoallocation, but has to jump through some 
C        hoops to do it.
C
         ELSE 
            AUTOWARN = .TRUE.
            DO KSTA = 1, NSTA
               IF( ( AUTOALOC(KSTA) .OR. AUTOREV(KSTA) ) .AND. 
     1                    AUTOWARN ) THEN
                  CALL WLOG( 0, ' ' )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 3A )' ) 
     1               'CHKSC1: *** Autoallocate requested with ',
     2               'processing at ', CORREL(1:LEN1(CORREL))
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 2A )' ) 
     1               '            Please be sure that this is ',
     2               'allowed.'
                  CALL WLOG( 1, MSGTXT )
                  AUTOWARN = .FALSE. 
               END IF
            END DO
         END IF
C
C
      END IF
C
C     Warn users if they are setting FREQ and BW in the scans and
C     they switch setup files without setting changing those
C     parameters.  This is a fairly benign warning so don't worry
C     about lots of complications such as what stations are in 
C     what scans.  Issue the warning if there is a setup file change
C     without a change in specified FREQ or BW.  Note that changing
C     either to zero constitutes being a change so is ok as desired.
C     Also only worry about the first channel even though there are
C     scenarios that would get past this.  Only warn once.
C
      DO KSCN = 2, NSCANS
         LSCN = KSCN - 1
         IF( SETNUM(KSCN) .NE. SETNUM(LSCN) ) THEN
            IF( ( FREQ(1,KSCN) .EQ. FREQ(1,LSCN) .AND. 
     1            FREQ(1,LSCN) .NE. 0.0 ) .OR. 
     2          ( BW(1,KSCN) .EQ. BW(1,LSCN) .AND. 
     3            BW(1,LSCN) .NE. 0.0 ) ) THEN
               CALL WLOG( 1, 'WARNING:  See note in sched.runlog ' //
     1            'about frequency and bandwidth changes.' )
               CALL WRTMSG( 'CHKSC1', 'freqbw' )
               GO TO 200
            END IF
         END IF
      END DO
  200 CONTINUE
C
      RETURN
      END
