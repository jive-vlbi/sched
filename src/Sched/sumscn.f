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
      LOGICAL     SKIPPED, LABFLG1, LABFLG2
      CHARACTER   LINE1*160, LINE2*160
      CHARACTER   TFORM*8, SUMDAT*6
      CHARACTER   FF*1, PDATE*(*)
      CHARACTER   SUMTXT1*100, SUMTXT2*100, SUMDESC*100, EXTN*4
      CHARACTER   PRESYM*1
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
            SUMTXT1 = SUMDESC( SUMITEM(IT1), LTXT1, LABFLG1 )
            SUMTXT2 = SUMDESC( SUMITEM(IT2), LTXT2, LABFLG2 )
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
                     WRITE( ISUM, '( 1X, /, 1X, /, A1, A, A, A ) ' )
     1                 FF, ' SCAN SUMMARY for experiment ', EXPCODE, 
     1                 EXPT(1:LEN1(EXPT))
                     LINE = 2
C
                     IF( DOSCANS(1) .GT. 0 ) THEN
                        WRITE( ISUM, '( 3A )' )
     1                     '     DOSCANS specified.  ''X'' in col 1 ',
     2                     'means scan will not be in VEX and other ',
     3                     'output files.'
                     END IF
C
                     IF( GOTPREEM ) THEN
                        WRITE( ISUM, '( 3A )' )
     1                     '     Symbol after SCAN number (based ', 
     2                     'on PREEMPT): ''+'' EXTRA scan.  ',
     3                     '''-'' Do not preempt for USNO.'
                        LINE = LINE + 1
                        IF( DOSCANS(1) .EQ. 0 ) THEN
                           WRITE( ISUM, '( 3A )' ) 
     1                        '       DOSCANS not specified so the ',
     2                        '''EXTRA'' scans will not be written ',
     3                        'to the VEX, crd, and other files.'
                           LINE = LINE + 1
                        END IF
                     END IF
C
                     IF( LABFLG1 .OR. LABFLG2 ) THEN
                        WRITE( ISUM, '( 5X, A, A, A )' )
     1                     'Flags: ',
     2                     'D=>Down, H=>Below Horizon, R=>Rises, ',
     3                     'S=>Sets, W=>Slew too long, t=>Tape Chg.'
                        LINE = LINE + 1
                     END IF
C
                     WRITE( ISUM, '( 5X, A, A, / 5X, A, A )' ) 
     3                 'Top item is:    ', SUMTXT1(1:LTXT1),
     4                 'Bottom item is: ', SUMTXT2(1:LTXT2)
                     LINE = LINE + 2
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
                        ICH1 = 39 + 6 * ( ISTA - ISTA1 )
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
C                 Get the symbol based on PREEMPT.
C 
                  IF( PREEMPT(ISCN) .EQ. 'EXTRA' ) THEN
                     PRESYM = '+'
                  ELSE IF( PREEMPT(ISCN) .EQ. 'NO' ) THEN
                     PRESYM = '-'
                  ELSE
                     PRESYM = ' '
                  END IF
C
C                 Write any comments that the user might have given.
C
                  IF( ANNOT(ISCN) .NE. ' ' ) THEN
                     WRITE ( ISUM, '( '' ---------- '',A, ' //
     1                  ' '' ----------'', /, A )' )
     1                  ANNOT(ISCN)(1:MAX(1,LEN1(ANNOT(ISCN)))), ' '
      		     LINE = LINE + 2
                  END IF
C
C                 If the scan was skipped, detect and note that fact.
C
                  SKIPPED = .TRUE.
                  DO ISTA = 1, NSTA
                     IF( STASCN(ISCN,ISTA) ) SKIPPED = .FALSE.
                  END DO
                  IF( SKIPPED .AND. NSTA .GT. 1 ) THEN
                     WRITE( ISUM, '( I4, 3A )' ) ISCN, PRESYM, 
     1                     '  Skipping scan on ',
     2                     SCNSRC(ISCN)(1:LEN1(SCNSRC(ISCN)))
                     LINE = LINE + 1
                  ELSE IF( SKIPPED .AND. NSTA .EQ. 1 ) THEN
                     WRITE( ISUM, '( I4, 4A, F8.2 )' ) ISCN, PRESYM,
     1                     '  Skipping scan on ',
     2                     SCNSRC(ISCN)(1:LEN1(SCNSRC(ISCN))),
     3                     '  at El=', EL1(ISCN,1)
                     LINE = LINE + 1
                  ELSE
C  
C                    Now process a good scan.
C                    Get scan times.
C
                     CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
                     CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOP )
                     WRITE( LINE1(1:9), '( I4, A, I4 )' ) 
     1                    ISCN, PRESYM, DAY1
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
                        ICH1 = 36 + 6 * ( ISTA - ISTA1 )
                        ICH2 = ICH1 + 5
C
                        LINE1(ICH1:ICH2) = 
     1                        SUMDAT( SUMITEM(IT1), ISCN, ISTA )
                        LINE2(ICH1:ICH2) = 
     1                        SUMDAT( SUMITEM(IT2), ISCN, ISTA )
                     END DO
C
C                    Write scan information with the indicator
C                    derived from DOSCANS.
C
                     IF( DOSCANS(1) .EQ. 0 .OR. 
     1                   ( ISCN .GE. DOSCANS(1) .AND. 
     2                   ISCN .LE. DOSCANS(2) ) ) THEN
                        WRITE( ISUM, '( A, /, A )' ) 
     1                        LINE1(1:LEN1(LINE1)),
     2                        LINE2(1:LEN1(LINE2))
                     ELSE
                        WRITE( ISUM, '( A A, /, A, A )' ) 
     1                        'X ', LINE1(1:LEN1(LINE1)),
     2                        'X ', LINE2(1:LEN1(LINE2))
                     END IF
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


