      INTEGER FUNCTION GNSET( ISCN, ISTA )
C
C     Function to select the required setup group index  for the 
C     specified current scan and station.  It is used in SETEXPND 
C     to set the variable NSETUP(ISCN,ISTA) which is where most 
C     routines get the setup.  It is also used in MAKEPTG in the 
C     process of making pointing scans.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER    ISCN, ISTA, KS, ISETF, LEN1, LSTA, LKS, LNAME, LN1
      LOGICAL    GOTVLA, GOTSET
      CHARACTER  LSETFILE*80
      SAVE       LSETFILE, LKS, LSTA
      DATA       LSETFILE, LKS, LSTA  / ' ', 0, 0 /
C -------------------------------------------------------------
C     Get the setup file number for this scan.
C
      ISETF = SETNUM(ISCN)
C
C     First see if the main loop can be shortcircuited because
C     there is no change or because the station is not in the scan.
C
      IF( .NOT. STASCN(ISCN,ISTA) ) THEN
         GNSET = MAX( LKS, 1 )
      ELSE IF( SETFILE(ISETF) .EQ. LSETFILE .AND.
     1         ISTA .EQ. LSTA ) THEN
         GNSET = LKS
C
C     Loop through the setup groups looking for right one.  Do a 
C     check that the setup file was found at all.
C
      ELSE
         GNSET = 0
         GOTVLA = .FALSE.
         GOTSET = .FALSE.
         DO KS = 1, NSET
       
            IF( SETFILE(ISETF) .EQ. SETNAME(KS) ) THEN
               GOTSET = .TRUE.
               IF( STANAME(ISTA) .EQ. SETSTA(1,KS) ) THEN
                  GNSET = KS
                  GO TO 100
               END IF
C
C              For warnings later, remember if there were any VLA
C              stations in the setup.  This only needed if the setup
C              is not found so can come after the above GOTO.
C
               IF( SETSTA(1,KS)(1:3) .EQ. 'VLA' ) GOTVLA = .TRUE.
C
            END IF
C
         END DO
C
C        Warn of possible programming error.
C
         IF( .NOT. GOTSET ) THEN
            WRITE( MSGTXT, '( A, A, A )' ) 
     1         'GNSET: Setup file not in list of files - ',
     2         'programming problem.'
            CALL WLOG( 1, MSGTXT )
            LNAME = LEN1( SETFILE(ISETF) )
            CALL WLOG( 1, 'File: ' // SETFILE(ISETF)(1:LNAME) )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, A, A )' )
     1            'Scan:', ISCN, '  Station: ', STANAME(ISTA)
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 'GNSET: Report the problem.' )

         END IF
C
  100    CONTINUE
C
C        If still don't have something, die.  Comment out the special 
C        warning if the problem was at the VLA as I am trying to make
C        the various VLA names equivalent.
C
         IF( GNSET .EQ. 0 ) THEN
C            IF( STANAME(ISTA) .EQ. 'VLA' .AND. GOTVLA ) THEN
C               CALL WLOG( 1, ' ' )
C               CALL WLOG( 1, 'GNSET: For VLBI observations at the '//
C     1             'VLA, use VLA1 or VLA27 in the schedule.' )
C            END IF
            LN1 = 1
            LNAME = LEN1( SETFILE(ISETF) )
            IF( LNAME .GT. 45 ) LN1 = LNAME - 45
            WRITE( MSGTXT, '( 4A )' ) ' GNSET: No setup for ', 
     1             STANAME(ISTA), ' in ', 
     2             SETFILE(ISETF)(LN1:LNAME)
            CALL ERRLOG( MSGTXT )
         END IF
C
      END IF   ! Need new value.
C
C     Keep results.  Don't keep one from a scan that does not include
C     the station.
C
      IF( STASCN(ISCN,ISTA) ) THEN
         LSETFILE = SETFILE(ISETF)
         LSTA = ISTA
         LKS = GNSET
      END IF
C
      RETURN
      END




