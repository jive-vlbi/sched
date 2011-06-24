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
      INTEGER    ISCN, KCHAN, ISET, ISTA, LSRC
      INTEGER    NEXPECT, NLATE, NNEVER, NSLATE
      REAL       NSRCCHG
      LOGICAL    DOPWARN, WARNLONG
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
      DO ISCN = SCAN1, SCANL
         IF( SRCNUM(ISCN) .NE. IDOPSRC(ISCN) .AND. DOPCAL(ISCN) ) THEN
            DO KCHAN = 1, MSCHN(SETNUM(ISCN))
               IF( VLSR(KCHAN,SRCNUM(ISCN)) .GT. -1.E8 .AND.
     1             VLSR(KCHAN,IDOPSRC(ISCN)) .GT. -1.E8 ) THEN
                  DOPWARN = .TRUE.
               END IF
            END DO
         END IF
      END DO
      IF( DOPWARN ) THEN
         CALL WLOG( 1, 'CHKSCN:  **** WARNING ****' )
         CALL WLOG( 1, '    This schedule contains scans ' //
     1      'for which SOURCE and DOPSRC are different and' )
         CALL WLOG( 1, '    are both line sources.' //
     1              '  Is this intentional?' )
      END IF
C
C     Determine which setups were actually used.  Some had been kept
C     because they might be used in inserted pointing scans.  Here
C     is where we flag them as not used.
C
      DO ISET = 1, NSET
         USED(ISET) = .FALSE.
         DO ISCN = SCAN1, SCANL
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) .AND. NSETUP(ISCN,ISTA) .EQ. ISET )
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
            DO ISCN = SCAN1, SCANL
                IF( STASCN(ISCN,ISTA) ) THEN
                   IF( LSRC .EQ. 0 ) TIME1 = STARTJ(ISCN)
                   TIMEDUR = STOPJ(ISCN) - TIME1
                   IF( SRCNUM(ISCN) .NE. LSRC ) NSRCCHG = NSRCCHG + 1.0
                   LSRC = SRCNUM(ISCN)
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
C     Do some checking that only applies when using real setups.
C
      IF( .NOT. NOSET ) THEN
C
C        Get the "frequency sets".  
C
         CALL GETFSET
C
C        Get the "pcal sets".
C
         CALL GETPSET
C
C        Check for possible data loss due to correlator resyncs and 
C        formatter reconfigures.
C
         CALL RESYNC
C
      END IF
C
C     Check for scans that are excessively long.  Setting 40min as
C     the standard for now.  Such scans put a lot of data at risk if
C     there are issues such as some that we've had with mid-scan disk
C     changes.
C
      WARNLONG = .TRUE.
      DO ISCN = SCAN1, SCANL
         IF( STOPJ(ISCN) - STARTJ(ISCN) .GT. 40.0 / 1440.D0 .AND. 
     1       WARNLONG ) THEN
            CALL WLOG( 1, 'See sched.runlog for warning on long scans.')
            CALL WRTMSG( 0, 'CHKSCN', 'warnlong' )
            WARNLONG = .FALSE.
         END IF
      END DO
C
C     Look for scans that start or, worse, end before very many antennas
C     are on-source.  Allow a one second tolerance on the start to 
C     avoid round off problems.
C
      NSLATE = 0
      DO ISCN = SCAN1, SCANL
         NEXPECT = 0
         NLATE   = 0
         NNEVER  = 0
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               NEXPECT = NEXPECT + 1
               IF( TONSRC(ISCN, ISTA) .GT. STARTJ(ISCN) + ONESEC ) 
     1             NLATE = NLATE + 1
               IF( TONSRC(ISCN, ISTA) .GT. STOPJ(ISCN) )
     1             NNEVER = NNEVER + 1
            END IF
         END DO
C
C        Warn of an individual scan with few antennas ever reaching the 
C        source.
C
         IF( NNEVER .GT. NEXPECT/2 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, 2A )' )
     1         'CHKSCN:  WARNING - Scan ', ISCN, ' had fewer than ', 
     2         'half the antennas on source by the stop time!'
            CALL WLOG( 1, MSGTXT )
         END IF
C
C        Accumulate the number of scans with most antennas not on source
C        at the start.
C
         IF( NLATE .GT. NEXPECT/2 ) NSLATE = NSLATE + 1
      END DO
C
C     Tell the user about the number of scans with late arrival by most
C     antennas.
C
         IF( NSLATE .GT. 0 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, 2A )' )
     1         'CHKSCN: ',  NSLATE, ' scans had more than half the ',
     2         'antennas arrive on-source after the start time.'
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A )' )
     1         '              This could be normal if using duration ',
     2         'scheduling with small gaps.'
            CALL WLOG( 1, MSGTXT )
         END IF
C
C     Check some VLA issues.
C
      CALL VLASCHK
C
      RETURN
      END
