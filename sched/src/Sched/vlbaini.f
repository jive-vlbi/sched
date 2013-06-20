      SUBROUTINE VLBAINI( ISCN, ISTA )
C
C     Routine for SCHED called by VLBA that starts a vlba control file.
C
      INCLUDE   'sched.inc'
C
      INTEGER           ISCN, ISTA, KSTA
      INTEGER           I, J, LEN1, DOY1
      CHARACTER         HUMAND1*16, VLBAD1*9, ESTART*9, TFORM*15
      CHARACTER         DISKD*6
      DOUBLE PRECISION  FRACD1, DPSTART
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VLBAINI: Starting.' )
C
C     Get the pointer to entries in the station catalog for this 
C     schedule station ISTA.
C
      KSTA = STANUM(ISTA)
C
C     Write identifying information and program name 
C     at top of schedule.
C
      WRITE( IUVBA, '(''!*  Schedule for '', A8,''  *!'')')
     1      STATION(KSTA)
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
C     Specify the disk format, even though this line is ignored
C     by the on-line system.  Presume that it will be the
C     format of the drive that is at the station and that it
C     will not change.  But for the RDBE/MARK5C, claim it is 
C     mark5a because that is all the crd file can control.
C     Also write a warning that the VEX file probably cannot be
C     used for correlation.
C
      IF( VLBITP .AND. USEDISK(ISTA) ) THEN
         IF( DAR(KSTA)(1:4) .EQ. 'RDBE' .AND. DOMKA ) THEN
            DISKD = 'mark5a'
            WRITE( IUVBA, '( 1X, /, 2A )' ) 
     1         '!*  WARNING:  This crd file ',
     2         'can be used to record data on the Mark5a system.*!'
            WRITE( IUVBA, '( 3A )' ) '!*  WARNING:  But the VEX ',
     1         'file is for the RDBE/MARK5C data and will most ', 
     2         'likely *!'
            WRITE( IUVBA, '( 2A )' ) '!*  WARNING:  ',
     1         'not work as is for correlation. *!'
            WRITE( IUVBA, '( 3A )' ) '!*  WARNING:  The channel ',
     1         'number, frequency, and bandwidth are probably ',
     2         'different. *!'
         ELSE
            DISKD = DISK(KSTA)
         END IF
         CALL DWCASE( DISKD )
         WRITE( IUVBA, '( 1X, /, 2A )' ) 'diskformat=', DISKD
      END IF
C
C     Tell operations what can be used.  This used to distinguish
C     tape and disk, but tape has been removed.  Keep in case
C     there are other distinctions later.
C
      IF( .NOT. DISK(KSTA) .EQ. 'NONE' ) THEN
         IF( .NOT. VLBITP ) THEN
            WRITE( IUVBA, '( A )' ) 'media=(1,none)'
         ELSE
            IF( USEDISK(ISTA) ) THEN
               WRITE( IUVBA, '( A )' ) 'media=(1,disk)'
            ELSE
               CALL ERRLOG( 'VLBAINI: VLBI schedule without '//
     1             'disk specified?' )
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
