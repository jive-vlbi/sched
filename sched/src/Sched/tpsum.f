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
      LOGICAL     FIRSTS
      INTEGER     ISTA, ISCN, YEAR, DAY1, DAY2, NACC
      DOUBLE PRECISION  START, STOP
      CHARACTER   TFORM*8
      CHARACTER   LSTOP*10, LSTART*10, LDAY1*3, LDAY2*3
C
C ------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'TPSUM starting' )
C
      WRITE( ISUM, '( 1X, /, A, /, 2X, /, 2A, /, 2A, /, A )' )
     1     ' TIME RANGE OF RECORDINGS and TOTAL BYTES:',
     2     '                Obs. start time.     Obs. end time. ',
     3      '     Total ', 
     4     ' Station          Day    Time         Day    Time   ',
     5      '     GBytes',
     6     '                     (UT)                 (UT)'
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
               IF( FIRSTS .AND. .NOT. NOREC(ISCN) ) THEN
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
               IF( .NOT. NOREC(ISCN) ) THEN
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
C        Write data for last tape.
C     
         IF( NACC .GE. 1 ) THEN
            WRITE(ISUM,'( 2X, A, T18, A,2X,A, 5X, A,2X,A, 3X, F8.1 )')
     1         STANAME(ISTA), LDAY1, LSTART, LDAY2, LSTOP,
     2         TGBYTES(ISTA)
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
