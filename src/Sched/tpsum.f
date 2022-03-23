      SUBROUTINE TPSUM
C
C     Routine for SCHED called by SCHSUM that writes out a summary
C     of the time span and data output for each station.
C
C     The name is a holdover from before Feb. 2008 when it gave a
C     summary of tape usage and tape changes.
C
      INCLUDE  'sched.inc'
C
      LOGICAL     FIRSTS, FIRSTE
      INTEGER     ISTA, ISCN, YEAR, DAY1, DAY2, NACC
      DOUBLE PRECISION  START, STOP
      REAL        GBY
      CHARACTER   TFORM*8
      CHARACTER   LSTOP*10, LSTART*10, LDAY1*3, LDAY2*3
      CHARACTER   ESTOP*10, ESTART*10, EDAY1*3, EDAY2*3
C
C ------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'TPSUM starting' )

C  GBYTES for DOSCANS is really for all scans.


C
      IF( FUZZY .AND. DOSCANS(1) .EQ. 0 ) THEN
         WRITE( ISUM, '( 1X, /, A, /, 2A, /, 2X, 3(/, 2A) )' )
     1     ' TIME RANGE OF RECORDINGS and TOTAL BYTES:',
     2     '    ''extras'' have PREEMPT=EXTRA, ''core'' has other ',
     3     'PREEMPT settings.',
     4     '              Start extras   Core start time.   ',
     5     'Core end time.    End extras       Core     Total',
     6     ' Station      Day    Time      Day    Time      ',
     7     ' Day    Time      Day    Time     GBytes    GBytes ',
     8     '                 (UT)             (UT)             ',
     9     '  (UT)             (UT)'
      ELSE IF( FUZZY .AND. DOSCANS(1) .NE. 0 ) THEN
         WRITE( ISUM, '( 1X, /, A, /, 2A, /, 2X, 3(/, 2A) )' )
     1     ' TIME RANGE OF RECORDINGS and TOTAL BYTES:',
     2     '    DOSCANS specified.  ''core'' have PREEMPT not ',
     3     'set to EXTRA. ',
     2     '             Start DOSCANS   Core start time.   ',
     3     'Core end time.    End DOSCANS      Core    DOSCANS',
     4     ' Station      Day    Time      Day    Time      ',
     5     ' Day    Time      Day    Time     GBytes    GBytes ',
     6     '                 (UT)             (UT)             ',
     7     '  (UT)             (UT)'
      ELSE IF( DOSCANS(1) .NE. 0 ) THEN
         WRITE( ISUM, '( 1X, 2(/, A), /, 2X, /, 2A, /, 2A, /, A )' )
     1     ' TIME RANGE OF RECORDINGS and TOTAL BYTES:',
     2     '    DOSCANS specified.  Summary is for that range.',
     2     '              DOSCANS start time.  DOSCANS end time.',
     3     '     DOSCANS', 
     4     ' Station          Day    Time         Day    Time   ',
     5     '     GBytes',
     6     '                     (UT)                 (UT)'
      ELSE
         WRITE( ISUM, '( 1X, /, A, /, 2X, /, 2A, /, 2A, /, A )' )
     1     ' TIME RANGE OF RECORDINGS and TOTAL BYTES:',
     2     '                Obs. start time.     Obs. end time. ',
     3      '     Total ', 
     4     ' Station          Day    Time         Day    Time   ',
     5      '     GBytes',
     6     '                     (UT)                 (UT)'
      END IF
C
      DO ISTA = 1, NSTA

C         WRITE( ISUM, '( 2X )' )
         FIRSTS = .TRUE.
         NACC = 0
         DO ISCN = SCAN1, SCANL
C     
C           Is this station involved in this scan?
C     
            IF( STASCN(ISCN,ISTA) .AND. .NOT. NOREC(ISCN) ) THEN
C     
C              Get first start time.
C     
               IF( FIRSTE .AND. .NOT. NOREC(ISCN) .AND. 
     1             DOSCANS(1) .EQ. 0 .OR. ISCN .GE. DOSCANS(1) ) THEN
                  CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
                  ESTART = TFORM( START, 'T', 0, 2, 2, '::@' )
                  ESTART(9:10) = '  '
                  WRITE( EDAY1, '(I3)' ) DAY1
                  FIRSTE  = .FALSE.
               END IF
C     
C              Get first start time of the core.
C     
               IF( FIRSTS .AND. .NOT. NOREC(ISCN) .AND. 
     1              PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
                  CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
                  LSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
                  LSTART(9:10) = '  '
                  WRITE( LDAY1, '(I3)' ) DAY1
                  FIRSTS  = .FALSE.
               END IF
C     
C              Collect last scan stop time.  Treat each scan as the
C              last, and when the loop runs out, the parameters from
C              the last scan in which the station participated will
C              be left.
C     
               IF( .NOT. NOREC(ISCN) .AND. 
     1              DOSCANS(1) .EQ. 0 .OR. ISCN .LE. DOSCANS(2) ) THEN
                  CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOP )
                  ESTOP = TFORM( STOP, 'T', 0, 2, 2, '::@' )
                  ESTOP(9:10) = '  '
                  WRITE( EDAY2, '(I3)' ) DAY2
               END IF
               IF( .NOT. NOREC(ISCN) .AND. 
     1              PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
                  CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOP )
                  LSTOP = TFORM( STOP, 'T', 0, 2, 2, '::@' )
                  LSTOP(9:10) = '  '
                  WRITE( LDAY2, '(I3)' ) DAY2
               END IF
C
               NACC = NACC + 1
            END IF
C     
         END DO
C     
C        Write data for the station.
C     
         IF( NACC .GE. 1 ) THEN
            IF( FUZZY ) THEN
               IF( DOSCANS(1) .EQ. 0 ) THEN 
                  GBY = EGBYTES(ISTA)
               ELSE
                  GBY = DGBYTES(ISTA)
               END IF
               WRITE(ISUM,'( 2X, A, T14, A,2X,A, 2X, A,2X,A, 3X,'//
     1               ' A,2X,A, 2X, A,2X,A, F8.0, F10.0 )')
     2             STANAME(ISTA), EDAY1, ESTART, LDAY1, LSTART, 
     3             LDAY2, LSTOP, EDAY2, ESTOP,
     4             TGBYTES(ISTA), GBY
            ELSE
               IF( DOSCANS(1) .EQ. 0 ) THEN 
                  GBY = TGBYTES(ISTA)
               ELSE
                  GBY = DGBYTES(ISTA)
               END IF
               WRITE(ISUM,'( 2X, A, T18, A,2X,A, 5X, A,2X,A, 3X, F8.1)')
     1             STANAME(ISTA), LDAY1, LSTART, LDAY2, LSTOP,
     2             GBY
            END IF
         ELSE
            WRITE(ISUM,'( 2X, A, A )' ) STANAME(ISTA), 
     1          '      No recorded data. '
         END IF
C     
      END DO
      WRITE( ISUM,'(2X)' )
C
      RETURN
      END
