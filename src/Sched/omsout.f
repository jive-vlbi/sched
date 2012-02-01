      SUBROUTINE OMSOUT( RESTART )
C
C     Write information to the file to be read by OMS.
C     Do regardless of correlator to be used.
C
      INCLUDE           'sched.inc'
C
      INTEGER           LEN1, NCH, I, ISCN, VLBOPE, IOERR
      LOGICAL           EXISTS, RESTART
      CHARACTER         OMSFILE*80, TFORM*8, TIME1*8, TIME2*8
      CHARACTER         OPSTAT*4, OPTEXT*255
      DOUBLE PRECISION  START1, STOPL, START, STOP
      INTEGER           DAY1, DAY2, YEAR1, YEAR2
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'OMSOUT starting' )
C
C     Construct the name of the oms file.
C     
      WRITE( OMSFILE, '(A,A)' )  EXPCODE(1:LEN1(EXPCODE)), '.OMS'
      CALL DWCASE( OMSFILE )
      IF( DEBUG ) CALL WLOG( 0, 'OMSOUT: Opening ' //
     1      OMSFILE(1:LEN1(OMSFILE)) )
C     
C     Find out if the .oms file already exists.
C     
      INQUIRE( FILE=OMSFILE, EXIST=EXISTS )
      IF( EXISTS .AND. ( RESTART .OR. OVERWRIT ) ) THEN
         OPSTAT = 'OLD'
      ELSE IF( EXISTS ) THEN
         WRITE( MSGTXT, '( A, A, A )' )  'OMSOUT: ', 
     1       OMSFILE(1:LEN1(OMSFILE)), ' already exists.'
         CALL WLOG( 1, MSGTXT )
         CALL ERRLOG( 
     1      'OMSOUT: You need to delete old output files '//
     2      'or use OVERWRIT.')
      ELSE
         OPSTAT = 'NEW'
      END IF
C     
C     Announce your intentions.
C     
      CALL WLOG( 0, 'OMSOUT:  Writing OMS file ' //
     1     OMSFILE(1:LEN1(OMSFILE)) )
C     
C     OPEN OMS file.
      
      IOERR = VLBOPE( IOMS, OMSFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( ' Open problem:'//OPTEXT )
C     
C     If there is a cover letter, write it here:
C     
      IF( COVERLET ) THEN
         WRITE( IOMS, '( 1X, /, A, /, 1X )' ) 'COVER LETTER:'
         CALL WRTCOV( IOMS )
      ELSE
         WRITE( IOMS, '( 1X, /, A )' )
     1      'No cover letter provided.'
      END IF
C     
C     Write the program information to the file.
C     
      WRITE( IOMS, '( 1X, /, A, /, 1X )' )
     1   'BEGIN = PROJECT_INFO'
C     
      WRITE( IOMS, '( A, A )' ) '    PROJECT_CODE       = ',
     1   EXPCODE
C     
C     Rich would like PROJECT_CODE and SEGMENT_CODE but I don't
C     have those separated and I am reluctant to guess.
C     
      WRITE( IOMS, '( A, A )' ) '    TITLE              = ',
     1    EXPT(1:LEN1(EXPT))
      WRITE( IOMS, '( A, A )' ) '    PI_NAME            = ',
     1    PINAME(1:LEN1(PINAME))
      DO I = 1, 4
         NCH = LEN1( ADDRESS(I) )
         IF( NCH .GT. 0 ) THEN
            WRITE( IOMS, '( A, A )' ) '    PI_ADDRESS         = ',
     1         ADDRESS(I)(1:NCH)
         END IF
      END DO     
      WRITE( IOMS, '( A, A )' ) '    PI_PHONE           = ',
     1    PHONE(1:LEN1(PHONE))
      WRITE( IOMS, '( A, A )' ) '    PI_OBS_PHONE       = ',
     1    OBSPHONE(1:LEN1(OBSPHONE))
      WRITE( IOMS, '( A, A )' ) '    PI_EMAIL           = ',
     1    EMAIL(1:LEN1(EMAIL))
      WRITE( IOMS, '( A, A )' ) '    PI_FAX             = ',
     1    FAX(1:LEN1(FAX))
C     
C     Get the first and last times.
C     
      START1 = STARTJ(SCAN1)
      STOPL = STOPJ(SCANL)
      DO ISCN = SCAN1, SCANL
         IF( .NOT. NOREC(ISCN) ) THEN
            START1 = MIN( START1, STARTJ(ISCN) )
            STOPL  = MAX( STOPL,  STOPJ(ISCN) )
         END IF
      END DO
      CALL TIMEJ( START1, YEAR1, DAY1, START )
      CALL TIMEJ( STOPL, YEAR2, DAY2, STOP )
      TIME1 = TFORM( START, 'T', 0, 2, 2, '::@' )
      TIME2 = TFORM( STOP, 'T', 0, 2, 2, '::@' )
      WRITE( IOMS, '( A, I4, A, I3.3, A, A )' ) 
     1    '    START              = ', YEAR1, '-', DAY1, '/', TIME1
      WRITE( IOMS, '( A, I4, A, I3.3, A, A )' ) 
     1    '    STOP               = ', YEAR2, '-', DAY2, '/', TIME2
C     
      WRITE( IOMS, '( A, A )' ) '    CORRELATOR         = ', 
     1       CORREL(1:LEN1(CORREL))
      WRITE( IOMS, '( 1X, /, A, /, 1X )' )
     1   'END = PROJECT_INFO'
C     
C     Write setup information if there is a setup (there may not be
C     for planning modes).
C
      IF( .NOT. NOSET ) CALL OMSSET
C     
C     Write the station info - one per station.
C     
      CALL OMSSTA
C     
C     Write the source info - one per source.
C     
      CALL OMSSRC
C     
C     Write the correlation parameters.
C     
      CALL OMSCOR
C
      RETURN
      END
