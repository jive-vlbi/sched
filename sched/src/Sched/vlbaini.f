      SUBROUTINE VLBAINI( ISCN, ISTA )
C
C     Routine for SCHED called by VLBA that starts a vlba control file.
C
      INCLUDE   'sched.inc'
C
      INTEGER           ISCN, ISTA
      INTEGER           I, J, LEN1, DOY1
      CHARACTER         HUMAND1*16, VLBAD1*9, ESTART*9, TFORM*15
      CHARACTER         DISKD*6
      DOUBLE PRECISION  FRACD1, DPSTART
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VLBAINI: Starting.' )
C
C     Write identifying information and program name 
C     at top of schedule.
C
      WRITE( IUVBA, '(''!*  Schedule for '', A8,''  *!'')')
     1      STATION(STANUM(ISTA))
      WRITE( IUVBA, '(''!*  Experiment '', A, '' *!'' )' ) 
     1      EXPCODE
C
      DO I = 1, MCOVER
         J = LEN1( COVER(I) )
         IF( J .GT. 0) THEN
           WRITE( IUVBA, '( A )' ) '!* '//COVER(I)(1:J)//' *!'
         ELSE
           WRITE( IUVBA, '( A )' ) '!*  *!'
         END IF
      END DO
C
C     Give a start time for the experiment in a comment.  Give
C     the earlier of the specified start time (STARTJ - PRESCAN)
C     and the recording start time.
C
      DPSTART = MIN( STARTJ(ISCN) - PRESCAN(ISCN),
     1               STARTJ(ISCN) - TPSTART(ISCN,ISTA) )
      CALL SCHDAY( DPSTART, VLBAD1, HUMAND1, DOY1, FRACD1 )
      ESTART = TFORM( FRACD1*TWOPI, 'T', 0, 2, 2, 'hms' )
      WRITE( IUVBA, '(''!*  Start at '', A9, 5X, A, 
     1    ''  Day of year '', I4, ''   *!'')' ) 
     2     ESTART, HUMAND1, DOY1
      WRITE( IUVBA, '( A, A )' ) 'program=', EXPCODE
C
C     Ask for automatic tape allocation if desired.
C
      IF( VLBITP ) THEN
         IF( AUTOALOC(ISTA) ) THEN
            WRITE( IUVBA, '( A )' ) 'autoallocate=on'
         ELSE
            WRITE( IUVBA, '( A )' ) 'autoallocate=off'
         END IF
         IF( AUTOREV(ISTA) ) THEN
            WRITE( IUVBA, '( A )' ) 'autoreverse=on'
         ELSE
            WRITE( IUVBA, '( A )' ) 'autoreverse=off'
         END IF
      END IF
C
C     Specify the disk format.  Presume that it will be the
C     format of the drive that is at the station and that it
C     will not change.
C
      IF( VLBITP .AND. USEDISK(ISTA) ) THEN
         DISKD = DISK(STANUM(ISTA))
         CALL DWCASE( DISKD )
         WRITE( IUVBA, '( 2A )' ) 'diskformat=', DISKD
      END IF
C
C     Tell operations what can be used.
C     The media command was not at the stations as of 15 Sept 2003.
C     Don't use it unless a disk type is specified in the station
C     catalog.
C
      IF( .NOT. DISK(STANUM(ISTA)) .EQ. 'NONE' ) THEN
         IF( .NOT. VLBITP ) THEN
            WRITE( IUVBA, '( A )' ) 'media=none'
         ELSE
            IF( USETAPE(ISTA) ) THEN
               IF( USEDISK(ISTA) ) THEN
                  WRITE( IUVBA, '( A )' ) 'media=(1,tape),(2,disk)'
               ELSE
                  WRITE( IUVBA, '( A )' ) 'media=(1,tape)'
               END IF
            ELSE IF( USEDISK(ISTA) ) THEN
               WRITE( IUVBA, '( A )' ) 'media=(1,disk)'
            ELSE
               CALL ERRLOG( 'VLBAINI: VLBI schedule without '//
     1             'tape or disk?' )
            END IF            
         END IF
      END IF
C
C     A setup scan before the first scan of the experiment will
C     be made for all observations.  This allows the operators to
C     check the setup before the observation starts and insures
C     that a header will be written in the monitor data for the
C     first scan after the experiment start time.  The actual 
C     setup scan will be written later where the Mark III etc 
C     setup scans are written.  Only a message is written here.
C
      WRITE( IUVBA, '( 1X, /, A, /, A )' ) 
     1   '!* The first scan is preceeded by a setup scan *!',
     2   '!* that ends at the start time of the first scan  *!'
C
      RETURN
      END
