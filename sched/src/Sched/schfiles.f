      SUBROUTINE SCHFILES( DOINIT, GOTSAT, 
     1                     VALUE, KC, KI, INSCH, BLANK )
C
C     Routine for SCHED called by SCHIN that reads any in-stream
C     catalogs etc.  It also detects if the main program input
C     should come from a file (which is needed when plotting to have
C     an interactive session.).
C
      INCLUDE 'sched.inc'
C
      DOUBLE PRECISION  VALUE(*), BLANK
      INTEGER           KI(*), I1, KEYPTR, I, LEN1, IOERR
      INTEGER           INSCH, VLBOPE, IER, ISETF
      CHARACTER         KC(*)*(*), KCHAR*256, RESULT*256, OPSTAT*3
      CHARACTER         SCHEDULE*80, LASTSCH*80, NEWSET*80
      LOGICAL           DOINIT, GOTSAT, GOTONE, ADDIT, DONECOV
      LOGICAL           EXISTS
      SAVE              LASTSCH
      DATA              LASTSCH  / 'DUMMY' /
C --------------------------------------------------------------------
      GOTONE = .FALSE.
      DOINIT = .TRUE.
C
C     Switch to another unit.  Also check plot request while at it.
C
      PLOT = VALUE( KEYPTR( 'PLOT', KC, KI ) ) .EQ. 0.D0
      PUBPLOT = VALUE( KEYPTR( 'PUBPLOT', KC, KI ) ) .EQ. 0.D0
C
C     Open schedule if SCHEDULE has been set and if it is not
C     already open.  Note on RESTART, it will have been opened
C     in SCHOPEN, but LASTSCH should equal SCHEDULE so it won't
C     try again here.
C
      SCHEDULE = KCHAR( 'SCHedule', 80, .FALSE., VALUE, KC, KI )
      CALL ENVIR( SCHEDULE )
      IF( SCHEDULE .NE. ' ' .AND. SCHEDULE .NE. LASTSCH ) THEN
         IF( DEBUG ) CALL WLOG( 0, 'SCHFILES:  About to open ' //
     1                SCHEDULE )
         INSCH = IUSCH
         IER = VLBOPE( INSCH, SCHEDULE, 'TEXT', 'OLD', RESULT )
         IF( IER .NE. 1 ) THEN
            CALL WLOG( 1, RESULT )
            CALL ERRLOG( 'SCHFILES: Problem opening schedule file '//
     1                SCHEDULE )
         END IF
         LASTSCH = SCHEDULE
         DOINIT = .FALSE.
         GOTONE = .TRUE.
      ELSE IF( PLOT .AND. INSCH .EQ. 5 ) THEN
         CALL ERRLOG( 'SCHFILES: Must read input from SCHEDULE file'//
     1         ' when plotting.' )
      END IF
C
C     Instream Source catalog.
C
      I1 = KEYPTR( 'SRCCAT', KC, KI )
      IF( VALUE(I1) .NE. UNSET ) THEN
         IF( DEBUG ) CALL WLOG( 1, 'SCHFILES: Imbedded source catalog.')
         IF( GOTONE ) CALL ERRLOG( 'SCHFILES: Cannot do 2 in-stream'//
     1           ' files at the same time. ' )
         CALL SRREAD( INSCH, 'Program_input', .FALSE., .FALSE., 'i' )
         VALUE(I1) = UNSET
         DOINIT = .FALSE.
         GOTONE = .TRUE.
      END IF
C
C     Instream reference pointing instructions.
C
      I1 = KEYPTR( 'PEAKINIT', KC, KI )
      IF( VALUE(I1) .NE. UNSET ) THEN
         IF( DEBUG ) CALL WLOG( 0, 'SCHFILES: Imbedded peak inst.' )
         IF( GOTONE ) CALL ERRLOG( 'SCHFILES: Cannot do 2 in-stream'//
     1           ' files at the same time. ' )
         CALL RDPEAK( INSCH )
         VALUE(I1) = UNSET
         DOINIT = .FALSE.
         GOTONE = .TRUE.
      END IF
C
C     Instream Station catalog.  In STREAD, prohibit use of the 
C     locations file with an instream stations catalog.  I think
C     instream stations catalogs are extremely rare anyway and am
C     tempted to remove them as an option.
C
      I1 = KEYPTR( 'STACAT', KC, KI )
      IF( VALUE(I1) .NE. UNSET ) THEN
         IF( DEBUG ) CALL WLOG( 0,'SCHFILES: Imbedded station catalog.')
         IF( GOTONE ) CALL ERRLOG( 'SCHFILES: Cannot do 2 in-stream'//
     1           ' files at the same time. ' )
         CALL STREAD( INSCH, .FALSE., 0.D0 )
         VALUE(I1) = UNSET
         DOINIT = .FALSE.
         GOTONE = .TRUE.
      END IF
C
C     Get tape initialization information.
C
      I1 = KEYPTR( 'TAPEINI', KC, KI )
      IF( VALUE(I1) .NE. UNSET ) THEN
         CALL ERRLOG( 'TAPEINI sections no longer supported in SCHED' )
      END IF
C
C     Get line rest frequencies if available.
C
      I1 = KEYPTR( 'LINEINIT', KC, KI )
      IF( VALUE(I1) .NE. UNSET ) THEN
         IF( DEBUG ) CALL WLOG( 0, 'SCHFILES: Reading line freqs.' )
         IF( GOTONE ) CALL ERRLOG( 'SCHFILES: Cannot do 2 in-stream'//
     1           ' files at the same time. ' )
         CALL RFREQ( INSCH )
         VALUE(I1) = UNSET
         DOINIT = .FALSE.
         GOTONE = .TRUE.
      END IF
C
C     Get satellite information
C
      I1 = KEYPTR( 'SATINIT', KC, KI )
      IF( VALUE(I1) .NE. UNSET ) THEN
         IF( DEBUG ) CALL WLOG( 0, 'SCHFILES: Reading satellite info.' )
         IF( GOTONE ) CALL ERRLOG( 'SCHFILES: Cannot do 2 in-stream'//
     1           ' files at the same time. ' )
         IF( GOTSAT ) CALL ERRLOG( 'SCHFILES: Can only read one SATINI '
     1       // 'section.' )
         CALL SATINI( INSCH )
         VALUE(I1) = UNSET
         GOTSAT = .TRUE.
         DOINIT = .FALSE.
         GOTONE = .TRUE.
      END IF
C
C     Setup file.  Read it.  Also add it to the list SETFILE and
C     increment NSETF if reasonable.  Note that the SETFILE list
C     is made up of both of files encountered in SETINIT input
C     and of files requested with SETUP input for the scans.
C
      NEWSET = KCHAR( 'SETINIT', 80, .FALSE., VALUE, KC, KI )
      IF( NEWSET .NE. ' ' ) THEN
         IF( DEBUG ) CALL WLOG( 0, 'SCHFILES: Imbedded setup: ' //
     1             NEWSET(1:LEN1(NEWSET)) )
         IF( GOTONE ) CALL ERRLOG( 'SCHFILES: Cannot do 2 in-stream'//
     1           ' files at the same time. ' )
C
C        Determine if need to add to SETFILE.
C
         ADDIT = .TRUE.
         IF( NSETF .NE. 0 ) THEN
            DO I = 1, NSETF
               IF( SETFILE(I) .EQ. NEWSET ) THEN
                  ADDIT = .FALSE.
                  ISETF= I
               END IF
            END DO     
         END IF
         IF( ADDIT .AND. NSETF .LT. MAXSET ) THEN
            NSETF = NSETF + 1
            SETFILE(NSETF) = NEWSET
            ISETF = NSETF
         ELSE IF( ADDIT ) THEN
            CALL ERRLOG( 'SCHFILES: Too many setup files.' )
         END IF
C
C        Now read the data.
C
         CALL RDSET( NEWSET, INSCH, .FALSE., ISETF )
         I1 = KEYPTR( 'SETINIT', KC, KI ) 
         DO I = I1, I1+9
            VALUE(I) = BLANK
         END DO
         DOINIT = .FALSE.
         GOTONE = .TRUE.
      END IF
C
C     Cover letter.  Write anything between 'COVERLET /' and
C     'ENDCOVER /' to a scratch file.  Use RESULT as a convenient 
C     text string.  Use a scratch file because, in principle,
C     the input can come from in-line input rather than a file
C     so it may not be possible to go reread it.
C
      I1 = KEYPTR( 'COVERLET', KC, KI )
      IF( VALUE(I1) .EQ. 0.D0 ) THEN
         COVERLET = .TRUE.
         INQUIRE( FILE='schedcov.tmp', EXIST=EXISTS )
         IF( EXISTS ) THEN
            OPSTAT = 'OLD'
         ELSE
            OPSTAT = 'NEW'
         END IF
         IOERR = VLBOPE( ICOV, 'schedcov.tmp', 'TEXT', OPSTAT, RESULT )
         IF( IOERR .NE. 1 ) THEN
            CALL WLOG( 1, RESULT )
            CALL ERRLOG( 'SCHFILES: Cannot open '//
     1       'cover letter scratch file.  This should not happen.' )
         END IF
C
C        Now transfer the cover letter to the scratch file.
C
         DONECOV = .FALSE.
         DO WHILE( .NOT. DONECOV )
            READ( INSCH, '( A )', END = 200 ) RESULT
            MSGTXT = RESULT
            CALL UPCASE( MSGTXT )
            DONECOV = INDEX( MSGTXT, 'ENDCOVER' ) .NE. 0 .AND. 
     1                INDEX( MSGTXT, '/' ) .NE. 0
            IF( .NOT. DONECOV ) THEN
               WRITE( ICOV, '( A )' ) RESULT(1:LEN1(RESULT))
            END IF
         END DO
         GO TO 210
  200    CONTINUE
         CALL ERRLOG( 'SCHFILES: Cover letter not terminated '//
     1           'with ENDCOVER /' )
  210    CONTINUE
         VALUE(I1) = UNSET
         DOINIT = .FALSE.
         GOTONE = .TRUE.
         CLOSE( UNIT=ICOV )
      END IF
C
C     Get lists of sources for multiple phase centers.
C
      I1 = KEYPTR( 'PCENTERS', KC, KI )
      IF( VALUE(I1) .NE. UNSET ) THEN
         IF( DEBUG ) CALL WLOG( 1, 'SCHFILES: Phase center lists.')
         IF( GOTONE ) CALL ERRLOG( 'SCHFILES: Cannot do 2 in-stream'//
     1           ' files at the same time. ' )
         CALL PCREAD( INSCH )
         VALUE(I1) = UNSET
         DOINIT = .FALSE.
         GOTONE = .TRUE.
      END IF
C
C
      RETURN
      END





