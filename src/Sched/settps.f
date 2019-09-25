      SUBROUTINE SETTPS( ISCN, LASTISCN )
Cf2py intent(in) ISCN, LASTISCN
C
C     Routine for SCHED to set TPSTART - how long before the scan
C     start time to start the recording.  Recorders may be started
C     early to help the correlator sync up and/or to prevent the
C     recorder from stopping for too short a time between scans.
C
C     If VEX is used, all stations should nominally start their 
C     recordings at the same time ("start" in the SCHED section 
C     of the VEX file).  That is why this code was moved out of 
C     TPSCH which only knows about one station.
C
C     However, while the nominal start time  is common to all 
C     antennas, the new VLBA system (~2012), and DiFX, believe the 
C     station dependent start time offset and don't start recording 
C     or correlation until that time.  That basically obsoletes much 
C     of the hoop jumping related to dealing with tape start times.
C     Also the disk systems don't mind short recording stoppages - 
C     in fact the Mark5C always stops between scans.
C
C     To preserve the common start time, do not change TPSTART later 
C     if DOVEX (which it is for all recording observations).  If not 
C     DOVEX, TPSTART may be reduced later at tape turnarounds or changes.
C
C     Jan 09:  The disk systems don't mind very short stops so don't
C     worry about that aspect any more.  That was an issue related
C     to spin up and spin down of the tapes.
C
C     Try to prevent excessively long recording scans. Feb. 26, 2010 RCW
C     Code was added to insert gaps, but that was a can of worms.
C     Comment out that code (we may want it again later), but keep a stern
C     warning if a scan longer than an hour is encountered.  Mar. 7, 2010.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           ISCN, ISTA, KSTA, LASTISCN(MAXSTA), KS
      INTEGER           DAY, YEAR
      LOGICAL           FIRSTS, PRESWARN, SGWARN
      DOUBLE PRECISION  LSTOPJ(MAXSTA), SCNGAP(MAXSTA)
      DOUBLE PRECISION  TPMIN, TOL, LASTGAP(MAXSTA)
      DOUBLE PRECISION  TIMED
C      CHARACTER         TIMECH*8
      CHARACTER          TFORM*8, CSTART*8
      PARAMETER         ( TOL = 0.1D0 / 86400.D0 )
      DATA              PRESWARN, SGWARN / .TRUE., .TRUE. /
      SAVE              LASTGAP
C  -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'SETTPS starting' )
C
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
               LASTGAP(ISTA) = STARTJ(ISCN)
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
     2                MINPAUSE(ISCN) .AND. 
     3             .NOT. NOREC(LASTISCN(ISTA)) .AND.
     4             .NOT. NOREC(ISCN) ) THEN
C
                  TPSTART(ISCN,ISTA) = SCNGAP(ISTA)
C
               END IF
C
C              Stub the following break insertion, but keep a
C              strong warning.  The break insertion opened too big
C              a can of worms, especially with non-NRAO systems.
C              In fact, change from doing it if ALLDISK set to only
C              doing it for stations with MARK5A systems.
C              
C              Original comment: 
C              Do a station dependent break if the record scan
C              has been too long.  This will propagate to all
C              stations in the VEX case when starts are aligned.
C              Only do this if all stations are using disk to
C              prevent short stoppages for other systems that might
C              not like it.  Make the inserted stoppage 3 seconds
C              to prevent it being zeroed in SCH24.
C
C              The maximum record scan is a bit arbitrary, but
C              set the limit at 1.1 hours for now.
C
               IF( ALLDISK .AND. DISK(STANUM(ISTA)) .EQ. 'MARK5A' ) 
     1               THEN
                  IF( TPSTART(ISCN,ISTA) .EQ. SCNGAP(ISTA) ) THEN
                     IF( STARTJ(ISCN) - LASTGAP(ISTA) .GT. 
     1                   1.01D0 / 24.D0 ) THEN
C
C                        Don't actually do it.
C
C                        TPSTART(ISCN,ISTA) = TPSTART(ISCN,ISTA) -
C     1                     3.0D0 * ONESEC
C
C                       If one station gets a forced gap, update 
C                       the LASTGAP for all as they will all get
C                       one when the start times are aligned.  With
C                       the stub, keep this to trigger additional
C                       warnings if the long scans keep up, but to
C                       not trigger too many warnings.
C
                        DO KSTA = 1, NSTA
                           LASTGAP(KSTA) = STARTJ(ISCN)
                        END DO
C
C                       Tell the user about it.
C
                        MSGTXT = ' '
                        CALL TIMEJ( STARTJ(ISCN), YEAR, DAY, TIMED )
                        CSTART = TFORM( TIMED, 'T', 0, 2, 2, '::@' )
                        WRITE( MSGTXT, '( A, I5, 4A )' )
     1                   'SETTPS: By scan ', ISCN, ' at ', CSTART, 
     2                   ' there has been over an hour of continuous',
     3                   ' recording on a Mark5A system.'
                        CALL WLOG( 1, MSGTXT )
                        MSGTXT = ' '
                        CALL WLOG( 1,
     1                   '        This puts excessive amounts of '//
     2                   'data at risk if there are playback problems.')
                        CALL WLOG( 1,
     1                   '        Use GAP, or reduce PRESTART or '//
     2                   'MINPAUSE to add a gap.' )
                     END IF
                  ELSE
                     LASTGAP(ISTA) = STARTJ(ISCN)
                  END IF
               END IF
            END IF
C
C           Record the smallest TPSTART for this scan that was 
C           forced by the preceeding stop time.  Comment out the
C           allowance for the cases where a short stoppage was inserted.
C           Put that back if gap insertion is resumed.
C
            IF( TPSTART(ISCN,ISTA) .EQ. SCNGAP(ISTA) ) THEN
C
C     1          TPSTART(ISCN,ISTA) + 3.0 * ONESEC .EQ. 
C     2          SCNGAP(ISTA) )  THEN
C
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
C              Jan 09.  Disks don't mind, so don't worry about this.
C              Keep the code available in case it is needed again.
C
C               IF( SGWARN .AND.
C     1             SCNGAP(ISTA) .GT. TPMIN .AND.
C     2             SCNGAP(ISTA) .LT. TPMIN +
C     3                   MINPAUSE(ISCN) .AND.
C     4             .NOT. NOREC(LASTISCN(ISTA)) .AND. 
C     5             .NOT. NOREC(ISCN) ) THEN
C
C                  SGWARN = .FALSE.
C                  CALL WLOG( 0, 'SETTPS: **** WARNING ****' )
C                  CALL WLOG( 0, '    Synchronized recorder starts '//
C     1              'for VEX have left some short recording stops.' )
C                  CALL WLOG( 0, '    This may cause problems at ' //
C     1              'the correlator.' )
C                  CALL TIMEJ( STARTJ(ISCN), YEAR, DAY, TIMED )
C                  TIMECH = TFORM( TIMED, 'T', 0, 2, 2, '::@' )
C                  MSGTXT = ' '
C                  WRITE( MSGTXT, '( A, I3, A, A, A, A )' )
C     1               '    Problem first encountered at ', DAY,
C     2               '/', TIMECH, ' at ', STCODE(STANUM(ISTA))
C                  CALL WLOG( 0, MSGTXT )
C                  MSGTXT = ' '
C                  WRITE( MSGTXT, '( 2A )' ) 
C     1               '    Note VEX file contains all ',
C     2               'stations, not just VEX stations.'
C                  CALL WLOG( 0, MSGTXT )
C
C               END IF
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
               CALL WRTMSG( 0, 'SETTPS', 'taperunning' )
               PRESWARN = .FALSE.
            END IF
         END IF
      END DO
C
      RETURN
      END
 
