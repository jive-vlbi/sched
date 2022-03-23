      SUBROUTINE GETUIN
C
C     Get the user input for NEWSRC.
C     Open the files.
C
      INCLUDE  'rdcat.inc'
      INCLUDE  'newsrc.inc'
C
      INTEGER     IER, LEN1
      CHARACTER   YNIN*1, INLINE*256
C
C ----------------------------------------------------------------
C
      write(*,*) '--------------------------------------------------'
      write(*,*) '-------------   NEWSRC starting.------------------'
      write(*,*) '--------------------------------------------------'
      write(*,*) 'Requests for user input: '
C
C     First get the old SCHED catalog.
C
      WRITE(*,*) '  SCHED source file that is being updated:'
      READ(*,'(A)') SRCFILE
C
C     Find out whether to keep sources from the old SCHED catalog that
C     are not in the new catalog.  If not, the old SCHED catalog is
C     still used for aliases etc.
C
      WRITE(*,*)
     1    '  Keep sources that are not in the new solution (Y/N):'
      READ(*,'(A)') YNIN
      KEEPOLD = YNIN .EQ. 'Y' .OR. YNIN .EQ. 'y'
C
C     Find out what to do if a source is in the old and new catalogs.
C
      WRITE(*,*)
     1    '  If a source is in both, keep (NEW, BEST):'
      READ(*,'(A)') CHOOSE
      CALL UPCASE( CHOOSE )
C
C     Find out whether to keep fluxes from the old file.
      WRITE(*,*)
     1    '  Keep fluxes that are not in the new solution (Y/N):'
      READ(*,'(A)') YNIN
      KEEPFLUX = YNIN .EQ. 'Y' .OR. YNIN .EQ. 'y'
C
C     Get the new source catalog and its format.
C
      WRITE(*,*) '  New solution data file:'
      READ(*,'(A)') GEOFILE
      WRITE(*,*) '  Format of solution file '//
     1     '(PETROV, GSFC, GSFCAST, SCHED):'
      READ(*,'(A)') SOUFMT
      IF( SOUFMT .NE. 'PETROV' .AND. SOUFMT .NE. 'GSFCAST' .AND.
     1    SOUFMT .NE. 'GSFC' .AND. SOUFMT .NE. 'GSFCAS2' .AND.
     2    SOUFMT .NE. 'SCHED' ) THEN
         WRITE(*,*) ' Unknown format '
         STOP
      END IF
C
C     Name the output catalog, which will be in SCHED (keyin) format.
C
      WRITE(*,*) '  New SCHED catalog:'
      READ(*,'(A)') NEWFILE
C
C     New REMARK and FLUXREF if one is not there in the input file.
C
      WRITE(*,*) '  New remark to be written for each replaced source:'
      WRITE(*,*) '     Might be replaced if using SCHED type input.'
      READ(*,'(A)' ) USRREMRK
C
      WRITE(*,*) '  New FLUXREF if there is a flux but no ref in input:'
      WRITE(*,*) '     Might be replaced if using SCHED type input.'
      READ(*,'(A)' ) USRFLXRF

C
C     Some instructions to the user.
C
      WRITE(*,*) '  ** When finished, edit in desired header lines.'
C
C     Echo the user input:
C
      WRITE(*,*) ' '
      WRITE(*,*) '--------Summary of user input.------------'
      WRITE(*,*) '-- OLD SCHED catalog that is being updated:    ',
     1            SRCFILE(1:LEN1(SRCFILE))
      IF( KEEPOLD ) THEN
         WRITE(*,*) 'Will keep all sources from input.'
      ELSE
         WRITE(*,*) 'Will keep only sources in the new solution.'
      END IF
      WRITE(*,*) 'If source in both, keep ', CHOOSE
      IF( KEEPFLUX ) THEN
         WRITE(*,*) 'Will keep flux densities from old file ',
     1       'if there are none in the new file.'
      ELSE
         WRITE(*,*) 'Will keep only fluxes in the new solution.'
      END IF
      WRITE(*,*) '-- NEW input solution file:     ',
     1           GEOFILE(1:LEN1(SRCFILE))
      WRITE(*,*) 'Format of solution file: ', SOUFMT
      WRITE(*,*) '-- OUTPUT SCHED catalog:     ',
     1           NEWFILE(1:LEN1(NEWFILE))
      WRITE(*,*) 'New REMARK=', USRREMRK(1:LEN1(USRREMRK))
      WRITE(*,*) 'New FLUXREF=', USRFLXRF(1:LEN1(USRFLXRF))
      WRITE(*,*) '---------End of user input.---------'
C
C     The old catalog file will be opened in RDSRC.
C
C
C     Open the new catalog file.
C
      OPEN( UNIT=9, FILE=GEOFILE, STATUS='OLD', ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED', IOSTAT=IER )
      IF( IER .NE. 0 ) THEN
         WRITE(*,*) 'Error opening ', GEOFILE(1:LEN1(GEOFILE))
         STOP
      END IF
C
C     Make a temporary file that will be a copy of the input that has
C     been fixed for some formatting found in some files that is
C     not compatible with the parsers.
C
      OPEN( UNIT=10, FILE='TEMP.DELETE', STATUS='NEW',
     1      ACCESS='SEQUENTIAL', FORM='FORMATTED', IOSTAT=IER )
      IF( IER .NE. 0 ) THEN
         WRITE(*,*) 'Error opening TEMP.DELETE'
         STOP
      ELSE
         WRITE(*,*) 'Making temporary file without troublesome',
     1     ' characters.'
      END IF
C
C     Open the output file so that header lines can be transferred.
C
      OPEN( UNIT=12, FILE=NEWFILE, STATUS='NEW', ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED', IOSTAT=IER )
      IF( IER .NE. 0 ) THEN
         WRITE(*,*) 'Error opening ', NEWFILE(1:LEN1(NEWFILE))
         STOP
      END IF
C
C     Write information about the fluxes to the head of the output 
C     file.
C
      WRITE( 12, '( A )' ) '!  Source catalog for SCHED'
      WRITE( 12, '( A )' ) '! '
      IF( .NOT. KEEPOLD ) THEN
         WRITE( 12, '( 2A )' ) '!  Sources are from ',
     1        GEOFILE(1:LEN1(GEOFILE))
         WRITE( 12, '( 2A )' ) '!  Some aliases are from ',
     1        SRCFILE(1:LEN1(SRCFILE))
      END IF
      WRITE( 12, '( A )' ) '! '
      WRITE( 12, '( A )' ) ' '
      WRITE( 12, '( 2A )' ) '!  Flux numbers are in triplets of ',
     1     'frequency (GHz), short and long baseline flux density (Jy)'
      WRITE( 12, '( 2A )' ) '!  Flux of -9.99 means no data.  ',
     1     '-1 is unknown limit.'
      WRITE( 12, '( 2A )' ) '!  Other negative is -1*(upper limit).'
      WRITE( 12, '( A )' ) ' '
      WRITE( 12, '( A )' ) ' '
C
      IF( SOUFMT .EQ. 'SCHED' ) THEN
         WRITE( 12, '( 2A )' ) '!  Header lines from ', 
     1        GEOFILE(1:LEN1(GEOFILE))
      WRITE( 12, '( A )' ) ' '
  100 CONTINUE
         INLINE = ' '
         READ( 9, '( A )' ) INLINE
         IF( INLINE(1:1) .EQ. '!' .OR. INLINE .EQ. ' ' ) THEN
            WRITE( 12, '( A )' ) INLINE(1:LEN1(INLINE))
            GO TO 100
         END IF
C
C        Stop reading if a source line is found.
C
         CALL UPCASE( INLINE )
         IF( INDEX( INLINE, 'SOURCE' ) .NE. 0 ) GO TO 100

      END IF   
      REWIND( UNIT = 9 )
C
C
      RETURN
      END
