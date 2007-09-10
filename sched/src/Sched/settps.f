      SUBROUTINE SETTPS( ISCN, LASTISCN )
C
C     Routine for SCHED to set TPSTART - how long before the scan
C     start time to start the recording.  Recorders may be started
C     early to help the correlator sync up and/or to prevent the
C     recorder from stopping for too short a time between scans.
C
C     For now, if VEX is used, all stations should start their tapes
C     at the same time.  That is why this code was moved out of
C     TPSCH which only knows about one station.
C
C     Do not change TPSTART later if DOVEX.  If not DOVEX, TPSTART
C     may be reduced later at tape turnarounds or changes.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           ISCN, ISTA, LASTISCN(MAXSTA), KS, DAY, YEAR
      LOGICAL           FIRSTS, PRESWARN, SGWARN
      DOUBLE PRECISION  LSTOPJ(MAXSTA), SCNGAP(MAXSTA)
      DOUBLE PRECISION  TPMIN, TOL, TIMED
      CHARACTER         TIMECH*8, TFORM*8
      PARAMETER         ( TOL = 0.1D0 / 86400.D0 )
      DATA              PRESWARN, SGWARN / .TRUE., .TRUE. /
C  -------------------------------------------------------------------
C     Initialize.
C
      DO ISTA = 1, NSTA
         TPSTART(ISCN,ISTA) = 0.D0
      END DO
      TPMIN = 1.D0
C
C     Loop through the stations finding TPSTART assuming it can
C     vary by station.
C
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) .AND. .NOT. NOSET .AND. 
     1       RECORDER(STANUM(ISTA)) .NE. 'S2' ) THEN
C
C           Flag first scan for station.  Get setup.  Get last stop.
C           Note that we will not try to keep recorder going through
C           NORECORD scans, even if short.
C
            FIRSTS = LASTISCN(ISTA) .EQ. 0
            KS = NSETUP( ISCN, ISTA )
            IF( FIRSTS ) THEN
               LSTOPJ(ISTA) = STOPJ(ISCN) - 0.5
            ELSE
               LSTOPJ(ISTA) = STOPJ(LASTISCN(ISTA)) 
            END IF
            SCNGAP(ISTA) = STARTJ(ISCN) - LSTOPJ(ISTA)
C
C           Get TPSTART based on PRESTART and MINPAUSE.
C           First just based on PRESTART.
C
            TPSTART(ISCN,ISTA) = MIN( PRESTART(ISCN), SCNGAP(ISTA) )
C
C           Don't stop for short gaps.  If FIRSTS, lastiscn(ista)
C           is zero, so should test the latter after the former.
C           Use a small tolerance in the tests to avoid roundoff
C           differences between machines.  TOL is about 0.1 second.
C
            IF( .NOT. FIRSTS ) THEN
               IF( SCNGAP(ISTA) + TOL .GT. TPSTART(ISCN,ISTA) .AND.
     1             SCNGAP(ISTA) + TOL .LT. TPSTART(ISCN,ISTA) +
     2                MINPAUSE(ISCN) * SPEEDUP(KS) .AND. 
     3             .NOT. NOREC(LASTISCN(ISTA)) .AND.
     4             .NOT. NOREC(ISCN) ) THEN
C
                  TPSTART(ISCN,ISTA) = SCNGAP(ISTA)
C
               END IF
            END IF
C
C           Record the smallest TPSTART for this scan that was 
C           forced by the preceeding stop time.
C
            IF( TPSTART(ISCN,ISTA) .EQ. SCNGAP(ISTA) )
     1          THEN
               TPMIN = MIN( TPMIN, TPSTART(ISCN,ISTA) )
            END IF
C
         END IF
C
      END DO
C
C     Set TPMIN for case where no TPSTARTS were forced by 
C     the scan gap.
C
      IF( TPMIN .EQ. 1.D0 ) TPMIN = PRESTART(ISCN)
C
C     Now have TPSTART.  If necessary, synchronize the recorder starts.
C     VEX requires this for now.
C
      IF( DOVEX ) THEN
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. .NOT. NOSET .AND. 
     1          RECORDER(STANUM(ISTA)) .NE. 'S2' ) THEN
C
C              Actually set the time to prestart the recorders.  This
C              will set TPSTART to the same value for all stations
C              in the scan.
C
               TPSTART(ISCN,ISTA) = TPMIN
C
C              Warn if synchronizing the starts will cause some
C              short recorder stoppages that could be a problem on the
C              correlator.  This can only happen if the 
C              stop times of the previous scan are
C              staggered (probably implies subarraying in use).
C              
C
               IF( SGWARN .AND.
     1             SCNGAP(ISTA) .GT. TPMIN .AND.
     2             SCNGAP(ISTA) .LT. TPMIN +
     3                   MINPAUSE(ISCN) * SPEEDUP(KS) .AND.
     4             .NOT. NOREC(LASTISCN(ISTA)) .AND. 
     5             .NOT. NOREC(ISCN) ) THEN
C
                  SGWARN = .FALSE.
                  CALL WLOG( 0, 'SETTPS: **** WARNING ****' )
                  CALL WLOG( 0, '    Synchronized recorder starts '//
     1              'for VEX have left some short recording stops.' )
                  CALL WLOG( 0, '    This may cause problems at ' //
     1              'the correlator.' )
                  CALL TIMEJ( STARTJ(ISCN), YEAR, DAY, TIMED )
                  TIMECH = TFORM( TIMED, 'T', 0, 2, 2, '::@' )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I3, A, A, A, A )' )
     1               '    Problem first encountered at ', DAY,
     2               '/', TIMECH, ' at ', STCODE(STANUM(ISTA))
                  CALL WLOG( 0, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 2A )' ) 
     1               '    Note VEX file contains all ',
     2               'stations, not just VEX stations.'
                  CALL WLOG( 0, MSGTXT )
C
               END IF
            END IF
         END DO
      END IF
C     
C     Warn users if some short gaps were filled.  Don't confuse
C     users by giving this message after the warning above about
C     synchronized starts.
C
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) .AND. (.NOT. NOSET) .AND. 
     1       RECORDER(STANUM(ISTA)) .NE. 'S2' .AND. 
     2       PRESWARN .AND. SGWARN ) THEN
            IF( TPSTART(ISCN,ISTA) .GT. PRESTART(ISCN) .AND.
     1          TPSTART(ISCN,ISTA) .GT. 0.D0 ) THEN
               CALL WRTMSG( 'SETTPS', 'taperunning' )
               PRESWARN = .FALSE.
            END IF
         END IF
      END DO
C
      RETURN
      END
 
