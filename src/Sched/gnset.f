      INTEGER FUNCTION GNSET( ISCN, ISTA )
C
C     Function to select the required setup index (usually LS)
C     for the current scan and station.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER    ISCN, ISTA, ISET, LEN1, LSTA, LLS, LNAME, LN1
      LOGICAL    GOTVLA, GOTSET
      CHARACTER  LSETFILE*80
      SAVE       LSETFILE, LLS, LSTA
      DATA       LSETFILE, LLS, LSTA  / ' ', 0, 0 /
C -------------------------------------------------------------
C     First see if the main loop can be shortcircuited because
C     there is no change or because the station is not in the scan.
C
      IF( .NOT. STASCN(ISCN,ISTA) ) THEN
         GNSET = MAX( LLS, 1 )
      ELSE IF( SETFILE(SETNUM(ISCN)) .EQ. LSETFILE .AND.
     1         ISTA .EQ. LSTA ) THEN
         GNSET = LLS
C
C     Loop through setups looking for right one.  Do a check that
C     the setup file was found at all.
C
      ELSE
         GNSET = 0
         GOTVLA = .FALSE.
         GOTSET = .FALSE.
         DO ISET = 1, NSET
       
            IF( SETFILE(SETNUM(ISCN)) .EQ. SETNAME(ISET) ) THEN
               GOTSET = .TRUE.
               IF( STANAME(ISTA) .EQ. SETSTA(1,ISET) ) THEN
                  GNSET = ISET
                  GO TO 100
               END IF
C
C              For warnings later, remember if there were any VLA
C              stations in the setup.  This only needed if the setup
C              is not found so can come after the above GOTO.
C
               IF( SETSTA(1,ISET)(1:3) .EQ. 'VLA' ) GOTVLA = .TRUE.
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
            LNAME = LEN1( SETFILE(SETNUM(ISCN)) )
            CALL WLOG( 1, 'File: ' // SETFILE(SETNUM(ISCN))(1:LNAME) )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, A, A )' )
     1            'Scan:', ISCN, '  Station: ', STANAME(ISTA)
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 'GNSET: Report the problem.' )

         END IF
C
  100    CONTINUE
C
C        If still don't have something, die.  Add special warning
C        if the problem was at the VLA
C
         IF( GNSET .EQ. 0 ) THEN
            IF( STANAME(ISTA) .EQ. 'VLA' .AND. GOTVLA ) THEN
               CALL WLOG( 1, ' ' )
               CALL WLOG( 1, 'GNSET: For VLBI observations at the '//
     1             'VLA, use VLA1 or VLA27 in the schedule.' )
            END IF
            LN1 = 1
            LNAME = LEN1( SETFILE(SETNUM(ISCN)) )
            IF( LNAME .GT. 45 ) LN1 = LNAME - 45
            WRITE( MSGTXT, '( 4A )' ) ' GNSET: No setup for ', 
     1             STANAME(ISTA), ' in ', 
     2             SETFILE(SETNUM(ISCN))(LN1:LNAME)
            CALL ERRLOG( MSGTXT )
         END IF
C
      END IF   ! Need new value.
C
C     Keep results.  Don't keep one from a scan that does not include
C     the station.
C
      IF( STASCN(ISCN,ISTA) ) THEN
         LSETFILE = SETFILE(SETNUM(ISCN))
         LSTA = ISTA
         LLS = GNSET
      END IF
C
      RETURN
      END




