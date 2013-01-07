      SUBROUTINE FILEOPEN( ISTA, OVBA, OLOC )
C
C     Opens the station files for SCHED.  They are:
C       1.  The crd. file - VLBA style file for control of the tape 
C                           drives and, for many stations, the
C                           antennas.
C       2.  The obs. file - for antenna control where that requires
C                           a file different from the crd. file.
C                           These are no longer used, but have not
C                           been stripped out yet (Dec 2012)
C       3.  The sch. file - Something for operators or the scheduler
C                           to look at for a readable summary of
C                           scans for that station.
C
C     Called from the SCHED main routine to avoid clutter there.
C
C --------------------------------------------------------------------
C
      INCLUDE    'sched.inc'
C
      INTEGER      ISTA, LEN1, IOERR, VLBOPE
      CHARACTER    PRTFILE*80, CRDFILE*80, OPTEXT*255
      CHARACTER    CRDTYPE*4, OPSTAT*4
      LOGICAL      OVBA, OLOC, EXISTS
C --------------------------------------------------------------------
      CALL WLOG( 1, ' Processing ' // STANAME(ISTA) )
C
C     Open printed schedule file
C
      WRITE( PRTFILE, '(A,A,A)' ) EXPCODE(1:LEN1(EXPCODE)), 
     1          'SCH.', STCODE(STANUM(ISTA))
      CALL DWCASE( PRTFILE )
      INQUIRE( FILE=PRTFILE, EXIST=EXISTS )
      IF( EXISTS .AND. OVERWRIT ) THEN
         OPSTAT = 'OLD'
      ELSE IF( EXISTS ) THEN
         CALL WLOG( 1, 'FILEOPEN: '//PRTFILE//' already exists.' )
         CALL ERRLOG( 'FILEOPEN: You need to delete old output files'
     1                // ' or set OVERWRIT.' )
      ELSE
         OPSTAT = 'NEW'
      END IF
      IOERR = VLBOPE( IPRT, PRTFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( OPTEXT )
C
C     Open file for telescope control.  There are a number
C     of stations that need 2 files, one for the DAR and
C     one for the antenna.  VLBADAR is true for them.
C
      IF( CONTROL(STANUM(ISTA)) .NE. 'NONE' ) THEN
C
C        Open a VLBA type control file if needed.  CRDTYPE is the
C        part of the file name that identifies the type of file.
C        It is crd. for the primary control file and obs. for the
C        antenna control file for stations that need separate 
C        antenna and tape control files.  If this is pure VLA,
C        don't open a file.
C
         OVBA = .FALSE.
         OLOC = .FALSE.
C
         IF( ( CONTROL(STANUM(ISTA)) .EQ. 'VLBA' .OR.
     1       VLBADAR(STANUM(ISTA)) ) .AND. .NOT. VLAONLY ) THEN
            CRDTYPE = 'crd.'
            WRITE( CRDFILE, '(A,A,A)' ) EXPCODE(1:LEN1(EXPCODE)), 
     1            CRDTYPE, STCODE(STANUM(ISTA))
            CALL DWCASE( CRDFILE )
            INQUIRE( FILE=CRDFILE, EXIST=EXISTS )
            IF( EXISTS .AND. OVERWRIT ) THEN
               OPSTAT = 'OLD'
            ELSE IF( EXISTS ) THEN
               CALL WLOG( 1, 'FILEOPEN: '//CRDFILE//' already exists.' )
               CALL ERRLOG( 'FILEOPEN: You need to delete old output '
     1                // 'files or set OVERWRIT.' )
            ELSE
               OPSTAT = 'NEW'
            END IF
            IOERR = VLBOPE( IUVBA, CRDFILE, 'TEXT', OPSTAT, OPTEXT )
            IF( IOERR .NE. 1 ) CALL ERRLOG( OPTEXT )
            OVBA = .TRUE.
         END IF
C 
C        Open other types of control files.
C          Shouldn't get here because these files are no longer being 
C          used.  But there are a few "CONTROL=VSOP" stations still
C          in the station catalog so be careful.
C
         IF( CONTROL(STANUM(ISTA)) .NE. 'VLBA' .AND. 
     1       CONTROL(STANUM(ISTA)) .NE. 'VEX' ) THEN
            CRDTYPE = 'obs.'
            WRITE( CRDFILE, '(A,A,A)' ) EXPCODE(1:LEN1(EXPCODE)), 
     1            CRDTYPE, STCODE(STANUM(ISTA))
            CALL DWCASE( CRDFILE )
            INQUIRE( FILE=CRDFILE, EXIST=EXISTS )
            IF( EXISTS .AND. OVERWRIT ) THEN
               OPSTAT = 'OLD'
            ELSE IF( EXISTS ) THEN
               CALL WLOG( 1, 'FILEOPEN: '//CRDFILE//' already exists.' )
               CALL ERRLOG( 'FILEOPEN: You need to delete old output '
     1                // 'files or set OVERWRIT.' )
            ELSE
               OPSTAT = 'NEW'
            END IF
            IOERR = VLBOPE( IULOC, CRDFILE, 'TEXT', OPSTAT, OPTEXT)
            IF( IOERR .NE. 1 ) CALL ERRLOG( OPTEXT )
            OLOC = .TRUE.
         END IF
C
      END IF
C     
      RETURN
      END
