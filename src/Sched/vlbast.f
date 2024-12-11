      SUBROUTINE VLBAST( ISCN, ISTA, FIRSTS, LSTOP,
     1           TSTART, TSTOP, DOSET, DOY1, DOY2, VLBAD1, VLBAD2 )
C
C     Routine for SCHED called by VLBA that writes basic scan 
C     information to a VLBA control file.  It also gets much
C     timing information that is used in the main routine.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER           ISCN, ISRC, ISTA, LCALTIME
      REAL              PDRA, PDDEC
      INTEGER           PMDOY, QUOUT, NCHAR, LEN1, I
      INTEGER           DOY1, DOY2
      LOGICAL           DOSET, PMSET, FIRSTS
      CHARACTER         TSTART*9, TSTOP*9, TRA*16, TDEC*16, TFORM*16
      CHARACTER         PMFT*9, PMVLBAD*9, PMHUMAND*16
      CHARACTER         HUMAND1*16, HUMAND2*16, VLBAD1*9, VLBAD2*9
      CHARACTER         DFMT*30
      DOUBLE PRECISION  LSTOP, PRA, PDEC, PPMTIME
      DOUBLE PRECISION  PMFRACD, FRACD1, FRACD2
      SAVE              PMSET, LCALTIME
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VLBAST: Starting.' )
C
C     Some initializations.
C
      ISRC   = SRCNUM(ISCN)
      IF( FIRSTS ) THEN
         PMSET = .FALSE.
         LCALTIME = -999
      END IF
C
C     Get various forms for start and stop time.
C
      CALL SCHDAY( STARTJ(ISCN) - TPSTART(ISCN,ISTA), VLBAD1, 
     1             HUMAND1, DOY1, FRACD1 )
      CALL SCHDAY( STOPJ(ISCN), VLBAD2, HUMAND2, DOY2, FRACD2 )
      TSTART = TFORM( FRACD1*TWOPI, 'T', 0, 2, 2, 'hms' )
      TSTOP  = TFORM( FRACD2*TWOPI, 'T', 0, 2, 2, 'hms' )
C
C     If there is a SETUP scan, it should not end at 00h00m00s
C
      IF( DOSET .OR. FIRSTS ) THEN
         IF( TSTART .EQ. '00h00m00s' ) TSTART = '00h00m01s'
      END IF
C
C     Write scan header line.
C
      WRITE( IUVBA, '( 1X, /, 4A, 3X, 2A )' ) '!* --- Scan from ',
     1     TSTART, ' to ', TSTOP, HUMAND1, ' --- *!'
C
C     Write comment.  Protect against "!" which causes problems.
C
      MSGTXT = ANNOT(ISCN)
      NCHAR = LEN1( ANNOT(ISCN) )
      DO I = 1, NCHAR
         IF( MSGTXT(I:I) .EQ. '!' ) MSGTXT(I:I) = '.'
      END DO
      IF( ANNOT(ISCN) .NE. ' ' ) THEN
         WRITE(IUVBA,'(''!*  '',A,''  *!'')') MSGTXT(1:NCHAR)
      END IF
C
C     Source information.  Do in subroutine to get satellite
C     and planet positions for specific scan.
C
      CALL SRCLOC( ISCN, ISTA, PRA, PDEC, PPMTIME, PDRA, PDDEC )
C
C     Get character form of RA and DEC that will be 
C     needed several times.
C
      TRA  = TFORM( PRA, 'T', 0, 2, 9, 'hms' )
      TDEC = TFORM( PDEC, ' ', 1, 2, 8, 'd''"' )
C
C     The qualifier is being used to distinguish setup scans from
C     real scans in monitor data.  Set to 999 on setup scan.
C
      IF( DOSET .OR. FIRSTS ) THEN
         QUOUT = 999
      ELSE
         QUOUT = QUAL(ISCN)
      END IF
C
C     Use the 12 character source name for all cases.
C
      NCHAR = LEN1( SCNSRC(ISCN) )
      WRITE(IUVBA,'( 7A, I3, 3A  )' ) 
     1      'sname=''', SCNSRC(ISCN)(1:NCHAR), '''  ra=', TRA, 
     2      '  dec=', TDEC, '  qual=', QUOUT, 
     3      '  calib=''', CALCODE(ISRC), ''''
C
C     Write proper motion information if needed.
C     A crazy user wants scan rates close to the slew rates and
C     the old format is not providing enough digits.  Increase
C     the digits, but only if needed.
C
      IF( PDRA .NE. 0.0 .OR. PDDEC .NE. 0.0  ) THEN
         CALL SCHDAY( PPMTIME, PMVLBAD, PMHUMAND, PMDOY, PMFRACD )
         PMFT = TFORM( PMFRACD*TWOPI, 'T', 0, 2, 2, 'hms' )
         IF( ABS( PDRA ) .GT. 1.D5 .OR. ABS( PDDEC ).GT. 1.D5 ) THEN
            DFMT = '( 5A, F14.2, A, F14.2 )'
         ELSE 
            DFMT = '( 5A, F10.2, A, F10.2 )'
         END IF
         WRITE( IUVBA, DFMT )
     1        ' epochd=', PMVLBAD, ' epocht=', PMFT, 
     2        ' dra=', PDRA, ' ddec=', PDDEC
         PMSET = .TRUE.
      ELSE IF( PMSET ) THEN
         WRITE( IUVBA, '( A )' ) '   dra=0.0  ddec=0.0 '
         PMSET = .FALSE.
      END IF
C
C     Write the caltime request.  Should happen on first call.
C
      IF( CALTIME(ISCN) .NE. LCALTIME ) THEN
         LCALTIME = CALTIME(ISCN)
         WRITE( IUVBA, '( A, I4 )' ) 'maxcaltime=', LCALTIME
      END IF
C
      IF( DEBUG ) CALL WLOG( 0, 'VLBAST: Ending.' )
      RETURN
      END
