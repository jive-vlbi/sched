      SUBROUTINE TPSUM( DOAUTO )
C
C     Routine for SCHED called by SCHSUM that writes out a summary
C     of tape usage by station.  When autoallocation is in effect,
C     the results represent a guess.  Otherwise they are real times.
C
      INCLUDE  'sched.inc'
C
      LOGICAL     DOAUTO, FIRSTS, DOTAPE, GOTSTRT 
      INTEGER     ISTA, ISCN, YEAR, DAY1, DAY2
      DOUBLE PRECISION  START, STOP
      CHARACTER   TFORM*8
      CHARACTER   LSTOP*10, LSTART*10, LDAY1*3, LDAY2*3
C
C ------------------------------------------------------------------
      IF( DOAUTO ) THEN
         WRITE( ISUM, '( 1X, 2( /, A, /, 2A), /, 2X, 3(/,A) )' )
     1     ' ESTIMATED TAPE TIMES (With autoallocation):',
     2     '        Times are approximate and assume project ',
     3     'starts at the beginning of a tape.',
     4     '        Times can be affected by actual tape length.',
     5     '        Tape change may occur during scan preceeding ',
     6     'indicated time.',
     6     '            Tape start time.            Tape end time. ',
     7     ' Station      Day    Time                Day    Time ',
     8     '                 (UT)                        (UT)'
      ELSE
         WRITE( ISUM, '( 1X, /, A, /, 2X, /, A, /, A, /, A )' )
     1     ' TAPE TIMES (Without autoallocation):',
     2     '            Tape start time.            Tape end time. ',
     3     ' Station      Day    Time                Day    Time ',
     4     '                 (UT)                        (UT)'
      END IF
C
      DO ISTA = 1, NSTA
         IF( DOAUTO .EQV. AUTOALOC(ISTA) ) THEN
            WRITE( ISUM, '( 2X )' )
            FIRSTS = .TRUE.
            DO ISCN = SCAN1, SCANL
C     
C              Is this station involved in this scan?
C     
               IF( STASCN(ISCN,ISTA) ) THEN
C     
C                 Get first start time.  Don't write tape info if
C                 the first scan is flagged for a tape change.
C     
                  DOTAPE = MOD( TPDAT(1,ISCN,ISTA), 10 ) .GT. 0
                  IF( FIRSTS ) THEN
                     GOTSTRT = .FALSE.
                     FIRSTS  = .FALSE.
C     
C                 Is there a tape change this scan?
C     
                  ELSE IF( DOTAPE ) THEN
C     
C                    Write out the data for the previous tape.
C     
                     WRITE(ISUM,'( 2X, A, T14, A,2X,A,12X,A,2X,A )')
     1                  STANAME(ISTA), LDAY1, LSTART, LDAY2, 
     2                  LSTOP
C     
                     GOTSTRT = .FALSE.
                  END IF
C     
C                 Collect start time of next tape.
C     
                  IF( .NOT. GOTSTRT .AND. .NOT. NOREC(ISCN) ) THEN
                     CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
                     LSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
                     LSTART(9:10) = '  '
                     WRITE( LDAY1, '(I3)' ) DAY1
                     GOTSTRT = .TRUE.
                  END IF
C     
C                 Collect latest scan stop time.
C     
                  IF( .NOT. NOREC(ISCN) ) THEN
                     CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOP )
                     LSTOP = TFORM( STOP, 'T', 0, 2, 2, '::@' )
                     LSTOP(9:10) = '  '
                     WRITE( LDAY2, '(I3)' ) DAY2
                  END IF
               END IF
C     
            END DO
C     
C           Write data for last tape.
C     
            WRITE(ISUM,'( 2X, A, T14, A,2X,A, 12X, A,2X,A )')
     1         STANAME(ISTA), LDAY1, LSTART, LDAY2, LSTOP
C     
         END IF
      END DO
C
      RETURN
      END
