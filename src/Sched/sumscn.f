      SUBROUTINE SUMSCN( PDATE )
C
C     Routine for SCHED called by SCHSUM that writes the main
C     summary information lines for all scans.  
C     Pulled out of SCHSUM 4 Feb 2003 RCW.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
C     STALINE is the number of stations to print on one line.
C
      INTEGER     STALINE
      PARAMETER   (STALINE=20)
C
      INTEGER     ISCN, ISTA, LEN1, LINE, ICH1, ICH2, IREP, JSUM, NSUM
      INTEGER     NREP, ISTA1, ISTA2, IPASS
      INTEGER     LSETNUM, NPASS, IT1, IT2, LTXT1, LTXT2
      INTEGER     YEAR, DAY1, DAY2
      DOUBLE PRECISION  START, STOP
      LOGICAL     SKIPPED, AUTOWARN
      CHARACTER   LINE1*132, LINE2*132
      CHARACTER   TFORM*8, SUMDAT*5
      CHARACTER   FF*1, PDATE*(*)
      CHARACTER   SUMTXT1*100, SUMTXT2*100, SUMDESC*100, EXTN*4
C-----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SUMSCN: Starting.' )
      FF = CHAR(12)
C
C     Don't allow too many stations for paper width.  NREP passes
C     will be needed for each pair of output items.
C
      NREP = ( NSTA - 1 ) / STALINE + 1
C
C     More than one complete summary can be written.  
C     What is printed is controlled by user input SUMITEM.
C     Two items from SUMITEM will be printed in each pass.
C     Figure out how many passes are going to be needed to print
C     all the summary items.  This will be NPASS.
C
      NSUM = 0
      DO JSUM = 1, 10
         IF( SUMITEM(JSUM) .NE. ' ' ) NSUM = JSUM
      END DO
      NPASS = ( NSUM - 1) / 2 + 1
C
C     Loop through the requested summary items.
C
      IF( NPASS .GE. 1 ) THEN
         DO IPASS = 1, NPASS
            IT1 = 2 * IPASS - 1
            IT2 = 2 * IPASS
            SUMTXT1 = SUMDESC( SUMITEM(IT1), LTXT1 )
            SUMTXT2 = SUMDESC( SUMITEM(IT2), LTXT2 )
            AUTOWARN = AUTOTAPE .AND. ( SUMITEM(IT1)(1:4) .EQ. 'TAPE' 
     1                 .OR. SUMITEM(IT2)(1:4) .EQ. 'TAPE' )
C
C           Loop through the passes if many stations.
C
            DO IREP = 1, NREP
               LINE = 0
               ISTA1 = ( IREP - 1 ) * STALINE + 1
               ISTA2 = MIN( NSTA, IREP*STALINE )
C
C              Loop through scans.
C
               DO ISCN = SCAN1, SCANL
C
C                 Write page header if needed.
C
                  IF( LINE .GT. LINEPG ) LINE = 0
                  IF( LINE .EQ. 0 ) THEN
C
                     WRITE( ISUM, '( 1X, /, 1X, /, A1, A, A, A, /, ' //
     1                 ' 5X, A, A, / 5X, A, A )' ) FF, 
     2                 ' SUMMARY for experiment ', EXPCODE, EXPT,
     3                 'Top item is:    ', SUMTXT1(1:LTXT1),
     4                 'Bottom item is: ', SUMTXT2(1:LTXT2)
                     LINE = 5
C
                     IF( AUTOWARN ) THEN
                        WRITE( ISUM, '( 5X, A )' ) 'Tape parameters ' //
     1                      'are only predictions for stations using' //
     2                      ' automatic tape allocation.'
                        LINE = LINE + 1
                     END IF
C
C                    "Type" entry.
C                    
                     WRITE( ISUM, '( 5X, 3A )' ) 
     1                      'TYPE top: -=> normal scan, ',
     2                      'P=>Pointing or Ta;  ',
     3                      'bottom: -=>recording, N=>not recording.'
C
                     WRITE( ISUM, '( 5X, A )' ) PDATE
                     LINE = LINE + 3
                     WRITE( ISUM, '( 2X )' )
C
                     LINE1 = 'SCAN  DAY START UT  SOURCE     TYPE  '//
     1                       'STATIONS    t => tape change '
                     LINE2 = '           STOP UT'
                     DO ISTA = ISTA1, ISTA2 
                        ICH1 = 38 + 5 * ( ISTA - ISTA1 )
                        ICH2 = ICH1 + 2
                        LINE2(ICH1:ICH2) = STCODE(STANUM(ISTA))
                     END DO
C
                     WRITE( ISUM, '( A, /, A /, 2X )' ) 
     1                  LINE1(1:LEN1(LINE1)),
     2                  LINE2(1:LEN1(LINE2))
C
C                    Clear the variables (needed for small experiments).
C
                     LINE1 = ' '
                     LINE2 = ' '
C
C                    Force setup name for first scan of page.
C
                     LSETNUM = 0
C
                  END IF
C
C                 If the scan was skipped, detect and note that fact.
C
                  SKIPPED = .TRUE.
                  DO ISTA = 1, NSTA
                     IF( STASCN(ISCN,ISTA) ) SKIPPED = .FALSE.
                  END DO
                  IF( SKIPPED ) THEN
                     WRITE( ISUM, '( 2A )' ) '  Skipping scan on ',
     1                     SCNSRC(ISCN)(1:LEN1(SCNSRC(ISCN)))
                     LINE = LINE + 1
                  ELSE
C  
C                    Now process a good scan.
C                    Get scan times.
C
                     CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
                     CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOP )
                     WRITE( LINE1(1:9), '( I4, I5 )' ) ISCN, DAY1
                     WRITE( LINE2(1:9), '( A4, I5 )' ) 
     1                    SCANTAG(ISCN), DAY2
                     LINE1(11:18) = TFORM( START, 'T', 0, 2, 2, '::@' )
                     LINE2(11:18) = TFORM( STOP, 'T', 0, 2, 2, '::@' )
                     LINE1(20:31) = SCNSRC(ISCN)
C
C                    Write setup name under source when it changes or at
C                    first scan of page.  Don't write the .set.
C
                     IF( SETNUM(ISCN) .NE. LSETNUM ) THEN
                        ICH1 = 1
                        ICH2 = LEN1(SETFILE(SETNUM(ISCN)))
                        IF( ICH2 .GE. 12 ) THEN
                           EXTN = SETFILE(SETNUM(ISCN))(ICH2-3:ICH2)
                           IF( EXTN .EQ. '.set' .OR. 
     1                         EXTN .EQ. '.SET' ) ICH2 = ICH2 - 4
                           ICH1 = ICH2 - 11
                           IF( ICH1 .LT. 1 ) ICH1 = 1
                        END IF
                        LINE2(20:31) = SETFILE(SETNUM(ISCN))(ICH1:ICH2)
                     ELSE
                        LINE2(20:31) = '-'
                     END IF
                     LSETNUM = SETNUM(ISCN)
C
C                    Get type and record/no record.
C
                     IF( POINT(ISCN) .GE. 0 .OR. PNTVLBA(ISCN) .OR. 
     1                   TANVLBA(ISCN) .OR. DOPEAK(ISCN) .GT. 0 .OR.
     2                   DOPN3DB(ISCN) .OR.
     3                   VLAMODE(ISCN) .EQ. 'IR' ) THEN
                        LINE1(34:34) = 'P'
                     ELSE
                        LINE1(34:34) = '-'
                     END IF
                     IF( NOREC(ISCN) .OR. .NOT. VLBITP ) THEN
                        LINE2(34:34) = 'N'
                     ELSE
                        LINE2(34:34) = '-'
                     END IF
C
C                    Get the scan data.  
C
                     DO ISTA = ISTA1, ISTA2
                        ICH1 = 36 + 5 * ( ISTA - ISTA1 )
                        ICH2 = ICH1 + 4
C
                        LINE1(ICH1:ICH2) = 
     1                        SUMDAT( SUMITEM(IT1), ISCN, ISTA )
                        LINE2(ICH1:ICH2) = 
     1                        SUMDAT( SUMITEM(IT2), ISCN, ISTA )
                     END DO
C
C                    Write scan information.
C
                     WRITE( ISUM, '( A, /, A )' ) 
     1                        LINE1(1:LEN1(LINE1)),
     2                        LINE2(1:LEN1(LINE2))
                     LINE = LINE + 2
C
C                    Write sun warning if needed.
C
                     IF( SUMITEM(IT1)(1:4) .NE. 'TAPE' ) THEN
                        CALL SUNWARN( ISCN, 10.0, LINE, ISUM )
                     END IF 
C
                  END IF
C
C                 Write blank line after scan.
C
                  WRITE( ISUM, '( 1X )' )
                  LINE = LINE + 1
C
               END DO
            END DO
         END DO
      END IF
C
C     Finished
C
      RETURN
      END


