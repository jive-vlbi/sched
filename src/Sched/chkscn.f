      SUBROUTINE CHKSCN
C
C     Routine for SCHED called by the main routine that checks scan 
C     parameters after SCHOPT and all other scan adjusting routines
C     are run.  Also, collects the "frequency sets".  See also
C     CHKSC1 which is called before SCHOPT.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    KSCN, KCHAN, ISET, ISTA, LSRC
      REAL       NSRCCHG
      LOGICAL    DOPWARN
      DOUBLE PRECISION  TIME1, TIMEDUR
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKSCN starting' )
C
C     Try to protect against errors in use of DOPSRC.  Assume that if  
C     DOPSRC .NE. SOURCE, SOURCE is probably meant to be a continuum
C     calibrator.  Therefore check in that situation if DOPPLER
C     Continuum sources have channel velocities of -1.E9
C
      DOPWARN = .FALSE.
      DO KSCN = SCAN1, SCANL
         IF( SRCNUM(KSCN) .NE. IDOPSRC(KSCN) .AND. DOPCAL(KSCN) ) THEN
            DO KCHAN = 1, MSCHN(SETNUM(KSCN))
               IF( VLSR(KCHAN,SRCNUM(KSCN)) .GT. -1.E8 .AND.
     1             VLSR(KCHAN,IDOPSRC(KSCN)) .GT. -1.E8 ) THEN
                  DOPWARN = .TRUE.
               END IF
            END DO
         END IF
      END DO
      IF( DOPWARN ) THEN
         CALL WLOG( 1, 'CHKSCN:  *** WARNING ***' )
         CALL WLOG( 1, '         *** This schedule contains scans ' //
     1      'for which SOURCE and DOPSRC are different and' )
         CALL WLOG( 1, '         *** are both line sources.' //
     1              '  Is this intentional?' )
      END IF
C
C     Determine which setups were actually used.  Some had been kept
C     because they might be used in inserted pointing scans.  Here
C     is where we flag them as not used.
C
      DO ISET = 1, NSET
         USED(ISET) = .FALSE.
         DO KSCN = SCAN1, SCANL
            DO ISTA = 1, NSTA
               IF( STASCN(KSCN,ISTA) .AND. NSETUP(KSCN,ISTA) .EQ. ISET )
     1             THEN
                  USED(ISET) = .TRUE.
                  GO TO 100
               END IF
            END DO
         END DO
  100    CONTINUE
      END DO
C
C     Check for violations of the maximum number of scans per hour 
C     specified.
C
      DO ISTA = 1, NSTA
         IF( MAXSRCHR(STANUM(ISTA)) .LT. 1.D4 ) THEN
            LSRC = 0
            NSRCCHG = 0
            DO KSCN = SCAN1, SCANL
                IF( STASCN(KSCN,ISTA) ) THEN
                   IF( LSRC .EQ. 0 ) TIME1 = STARTJ(KSCN)
                   TIMEDUR = STOPJ(KSCN) - TIME1
                   IF( SRCNUM(KSCN) .NE. LSRC ) NSRCCHG = NSRCCHG + 1.0
                   LSRC = SRCNUM(KSCN)
                END IF
            END DO
C
C           Abort with info if there are too many changes.
C
            IF( NSRCCHG / TIMEDUR .GT. MAXSRCHR(STANUM(ISTA)) * 24.0 ) 
     1           THEN
               CALL WLOG( 1, 'CHKSCN: ***************************' //
     1                '****************' )
               CALL WLOG( 1, '    TOO MANY SOURCES PER HOUR AT ' //
     1               STANAME(ISTA) )
               MSGTXT = '  '
               WRITE( MSGTXT, '( A, F5.1 )' )
     1               '    OBSERVATORY WILL NOT ALLOW MORE THAN', 
     2               MAXSRCHR(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               MSGTXT = '  '
               WRITE( MSGTXT, '( A, F5.1 )' )
     1               '    YOU HAVE SPECIFIED ', 
     2               NSRCCHG / ( TIMEDUR * 24.D0 )
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, 
     1             '    REDUCE NUMBER OF SCANS FOR THIS STATION' )
C               Switch to this line if we want this to stop SCHED.
C               CALL ERRLOG( 
C     1             '    REDUCE NUMBER OF SCANS FOR THIS STATION' )
               CALL WLOG( 1, 'CHKSCN: ***************************' //
     1                '****************' )
            END IF
         END IF
      END DO
C
C     Get the "frequency sets".  
C
      IF( .NOT. NOSET ) CALL GETFSET
C
C     Get the "pcal sets".
C
      IF( .NOT. NOSET ) CALL GETPSET
C
      RETURN
      END




