      SUBROUTINE TIMES( VALUE, KC, KI, START, STOP, DAY, YEAR )
C
C     Routine for SCHED called by SCHIN after all scans have been
C     read.  It uses SATTIM to establish the actual time (before
C     optimization) of all scans.  It also sets up any LST scheduling.
C
      INCLUDE 'sched.inc'
C
      INTEGER           YEAR(*), DAY(*), KI(*), ISCN, I1, KEYPTR
      INTEGER           ISTA
      DOUBLE PRECISION  STOP(*), START(*), VALUE(*), SLA_DAT
      CHARACTER         KC(*)*(*), LSTNAM*8
C ---------------------------------------------------------------------
C     LST scheduling. This can be done for any station. 
C     Station catalog must already be read.  Default LSTSTA is VLA.  
C     This must come before the call to SATTIM.
C
      I1 = KEYPTR( 'LST', KC, KI )
      LST = VALUE(I1) .NE. UNSET
      IF( LST ) THEN
         IF( VALUE(I1) .EQ. 0.D0 ) THEN
            LSTNAM = 'VLA'
         ELSE
            WRITE( LSTNAM, '(A8)' ) VALUE(I1)
            CALL UPCASE( LSTNAM )
         END IF
         DO ISTA = 1, MSTA
            IF( LSTNAM .EQ. STATION(ISTA) ) THEN
               LSTSTA = ISTA
               GO TO 50
            END IF
         END DO
         CALL ERRLOG( 'TIMES: LST specified but station not in catalog')
50       CONTINUE
      END IF
C
C     Set start and stop times (in Julian days) and check inputs.
C     But before setting them, determine which are adjustable.
C     Here are the meanings of DURONLY.  This was added long after the
C     coding so this is what I deduce.  Fix if wrong.
C     DURONLY(ISCN) = 0     Default -- probably means an error.
C     DURONLY(ISCN) = 1     Duration (or dwell) only set.
C     DURONLY(ISCN) = 2     Start time only set (how to stop?).
C     DURONLY(ISCN) = 3     Start and duration set.
C     DURONLY(ISCN) = 4     Stop time only set.
C     DURONLY(ISCN) = 5     Duration and stop time set.
C     DURONLY(ISCN) = 6     Start and Stop set.
C     DURONLY(ISCN) = 7     Start, Stop, and duration set.
C
      DO ISCN = 1, NSCANS
         DURONLY(ISCN) = 0
         IF( DUR(ISCN) .GT. 0.D0 ) DURONLY(ISCN) = 1
         IF( START(ISCN) .NE. UNSET ) DURONLY(ISCN) = DURONLY(ISCN) + 2
         IF( STOP(ISCN) .NE. UNSET ) DURONLY(ISCN) = DURONLY(ISCN) + 4
         CALL SATTIM( ISCN, START(ISCN), STOP(ISCN), 
     1                DAY(ISCN), YEAR(ISCN) )
      END DO
C
C     Now that we have the time, set IATUTC if it wasn't set by user.
C
      IATUTC = SLA_DAT( STARTJ(1) )
C
      RETURN
      END
