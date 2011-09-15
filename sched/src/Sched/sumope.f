      SUBROUTINE SUMOPE( RESTART, PDATE, MJD )
C
C     Routine for SCHED that opens the summary file and writes the
C     cover information and correlator information into it.  It also
C     calculates some correlator related parameters such as the data
C     set size.
C
C
      INCLUDE 'sched.inc'
C
      INTEGER        LEN1, VLBOPE, IOERR, NCH, MJD, I
      CHARACTER      PRTFILE*80, OPTEXT*255, OPSTAT*4
      CHARACTER      PDATE*(*)
      LOGICAL        EXISTS, RESTART
C ------------------------------------------------------------------
C
C     Construct the name of the summary file.
C
      WRITE( PRTFILE, '(A,A)' )  EXPCODE(1:LEN1(EXPCODE)), '.SUM'
      CALL DWCASE( PRTFILE )
      IF( DEBUG ) CALL WLOG( 0, 'SUMOPE: Opening ' //
     1      PRTFILE(1:LEN1(PRTFILE)) )
C
C     Find out if the .sum file already exists.
C
      INQUIRE( FILE=PRTFILE, EXIST=EXISTS )
      IF( EXISTS .AND. ( RESTART .OR. OVERWRIT ) ) THEN
         OPSTAT = 'OLD'
      ELSE IF( EXISTS ) THEN
         NCH = LEN1(PRTFILE) 
         CALL WLOG( 1, 'SUMOPE: ' //
     1       PRTFILE(1:NCH) // ' already exists.' )
         CALL ERRLOG( 
     1   'SUMOPE: You need to delete old output files or use OVERWRIT.')
      ELSE
         OPSTAT = 'NEW'
      END IF
C
C     Announce your intentions.
C
      CALL WLOG( 1, 'SUMOPE:  Writing summary file ' //
     1     PRTFILE(1:LEN1(PRTFILE)) )
C
C     OPEN summary file.
C
      IOERR = VLBOPE( ISUM, PRTFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( ' Open problem:'//OPTEXT )
C
C     Identify the schedule
C
      WRITE( ISUM, '( 20X, A, A, /, 1X, /, A, /, 1X, /, 2A, /, 2A )' )
     1      'SUMMARY FILE FOR PROJECT: ', EXPCODE,
     2      'COVER INFORMATION: ',
     3      '  Experiment: ', EXPT(1:LEN1(EXPT)),
     4      '  Exp. Code:  ', EXPCODE            
C
C     Write the date
C
      WRITE( ISUM, '( A, A )' ) '  Start ', PDATE
C
C     Give the rest of the cover information.
C
      WRITE( ISUM, '( 1X, /, 2(2X,A,/), 1X,/, 2X,A, /, 1X,/, 4(2X,A,/),'
     1      //'1X,/, 4(2X,A,/), 1X,/, 2X,A,/, 1X,/, 4(2X,A,/) )' )
     2      (COVER(I)(1:MAX(1,LEN1(COVER(I)))), I=1,MCOVER)
C
C     Write the cover letter if there is one.
C
      IF( COVERLET ) THEN
         WRITE( ISUM, '( 1X, /, A, /, 1X )' )
     1      'COVER LETTER:'
         CALL WRTCOV( ISUM )
      ELSE
         WRITE( ISUM, '( 1X, /, A )' )
     1      'No cover letter provided.'
      END IF
C
C     Derive and list the times that the schedule can be preempted
C     at some stations (Meant for PT and MK use by USNO).
C
      CALL PROTECT( ISUM )
C
C     Write the correlator information.
C
      CALL CORLST
C
C     List the stations.
C
      CALL STALST( MJD )
C
      RETURN
      END
