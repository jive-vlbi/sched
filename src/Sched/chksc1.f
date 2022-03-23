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
      INTEGER    KSCN, LSCN, ISTA, KS
      LOGICAL    GOTERR, HAVEPL, HAVESA
C ----------------------------------------------------------------------
      GOTERR = .FALSE.
C
C     Loop over scans.
C
      HAVEPL = .FALSE.
      HAVESA = .FALSE.
      DO KSCN = 1, NSCANS
C
C        Look for attempts to use ephemeris or satellite at stations
C        where this shouldn't be done.  Usually this is for stations
C        for which we don't know how to specify position rates.
C
C        Also look for attempts to look for both planets and
C        satellites in the same schedule.  This doesn't seem to work
C        for unknown reasons.  By planets here, I mean something using
C        the ephemeris.  If doing satellites, you can also do a planet
C        by treating it like a satellite and getting the info from the
C        bsp file.
C
         DO ISTA = 1, NSTA
            IF( STASCN(KSCN,ISTA) ) THEN
               IF( PLANET(SRCNUM(KSCN)) .OR. SATEL(SRCNUM(KSCN)) ) THEN
                  IF( CONTROL(STANUM(ISTA)) .NE. 'VLBA' .AND.
     1                .NOT. VLBADAR(STANUM(ISTA)) .AND.
     2                CONTROL(STANUM(ISTA)) .NE. 'VLA' ) THEN
                     GOTERR = .TRUE.
                     CALL WLOG( 1, 'CHKSC1: Trying to use ephemeris '//
     1                    'or satellite position for '//SCNSRC(KSCN) )
                     CALL WLOG( 1, '        at '//STATION(STANUM(ISTA)) 
     1                    //' but SCHED doesn''t know how.' )
                  END IF
               END IF
               IF( PLANET(SRCNUM(KSCN)) ) HAVEPL = .TRUE.
               IF( SATEL(SRCNUM(KSCN)) ) HAVESA = .TRUE.
C
            END IF
         END DO
C
      END DO
C
      IF( HAVEPL .AND. HAVESA ) THEN
         CALL WLOG( 1, 'CHKSC1:  Trying to use ephmeris and '//
     1     'satellites in the same schedule' )
         CALL WLOG( 1, 'CHKSC1:  That might cause a crash that '//
     1     'looks like an inability to read a bsp file.' )
      END IF
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
            DO ISTA = 1, NSTA
               IF( STASCN(KSCN,ISTA) ) THEN
                  KS = NSETUP(KSCN,ISTA)
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
                     CALL PRTSCN( KSCN, 'CHKSC1' )
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
         CALL WLOG( 1, ' ' )
         CALL WLOG( 1, 'CHKSC1:   *** This schedule uses formats ' //
     1       'that suggest it should     ***' )
         CALL WLOG( 1, '          *** be recording but there are ' //
     1       'no recording scans.        ***' )
         CALL WLOG( 1, '          *** Was this intended? ' //
     1       'It might be for reference pointing ***' )
         CALL WLOG( 1, '          *** If not, check OBSTYPE and ' //
     1       'RECORD/NORECORD             ***' )
         CALL WLOG( 1, ' ' )
       END IF
  100 CONTINUE
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
     1            FREQ(1,LSCN) .NE. 0.D0 ) .OR. 
     2          ( BW(1,KSCN) .EQ. BW(1,LSCN) .AND. 
     3            BW(1,LSCN) .NE. 0.D0 ) ) THEN
               CALL WLOG( 1, 'WARNING:  See note in sched.runlog ' //
     1            'about frequency and bandwidth changes.' )
               CALL WRTMSG( 0, 'CHKSC1', 'freqbw' )
               GO TO 200
            END IF
         END IF
      END DO
  200 CONTINUE
C
C     Check that HIGROUP is not used when not using OPTMODE='HIGHEL'
C
      DO KSCN = 1, NSCANS
         IF( HIGROUP(KSCN) .GE. 2 .AND. OPTMODE .NE. 'HIGHEL' ) THEN
           CALL ERRLOG( 'CHKSC1: HIGROUP used without OPTMODE=HIGHEL' )
         END IF
      END DO
C
      RETURN
      END
