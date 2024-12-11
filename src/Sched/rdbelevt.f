      SUBROUTINE RDBELEVT
C
C     The RDBE takes a finite amount of time to set power levels.  This 
C     routine checks that adequate time is available to do this operation.
C     It was more important when the times were longer.
C
C     There are two level setting operations.  The first is to set the analog
C     power level before the sampler.  That is a "set and remember" operation.
C     It is done only once per project just before the first scan that uses
C     a particular setup in terms of RF band, external LOs etc - items that
C     can affect the power level in the 512 MHz.  The on-line system will 
C     try to start this operation 5 seconds before the scan start, but will
C     delay the start by the required amount to allow 5 seconds if necessary.
C     This was originally 15 seconds when this routine was written, so was
C     rather more important than now. The antenna catalog parameter TLEVSET 
C     can be used to be sure this time is provided.
C
C     If this operation is not done properly, the whole experiment at that
C     band (setup file) can be messed up pretty badly.  After the first time,
C     it is still a good idea to allow time any time there is a setup change
C     because the set will need to be redone if the schedule is restarted.
C     Note that different dopplar shifts for different sources typically keep
C     the same setup and so don't need to redo the set.
C
C     The second set happens at the beginning of each scan.  It is to set
C     the thresholds for the final resampling to 2 bits.  That operation
C     is allocated 2 seconds (absorbed in the 5 seconds for the first time).  
C     This used to be 5 seconds. So every scan should have that much of a 
C     gap.  To be sure this time is added for DWELL scheduled projects, use 
C     the antenna catalog parameter MINSETUP.
C
C     Craig Walker  Oct. 23, 2012.
C     Changed to reflect faster times, May 22, 2013
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISTA, ISCN, LSCN, KS, LASTSET, LEN1
      LOGICAL    SETSEEN(MSET), WARNMSG
      LOGICAL    TRWARN, TSWARN2
      DOUBLE PRECISION   TSAMPLEV, TRESAMP, STARTS, TOL
      PARAMETER          ( TSAMPLEV = 5.D0 )
      PARAMETER          ( TRESAMP = 2.D0 )
      PARAMETER          ( TOL = 0.1 )
      DATA       TRWARN, TSWARN2, WARNMSG / 3*.TRUE. /
      SAVE       TRWARN, TSWARN2, WARNMSG
C
C     The following are for use if a code segment below if reactivated.
C
C      LOGICAL    TRWARN, TSWARN1, TSWARN2
C      DATA       TRWARN / .TRUE. /
C      SAVE       TSWARN1
C ---------------------------------------------------------------------
C     Loop over stations.  Do this operation on a per station basis except
C     don't repeat the warnings too often. Only worry about RDBE stations.
C
      DO ISTA = 1, NSTA
         IF( DAR(STANUM(ISTA))(1:4) .EQ. 'RDBE' ) THEN
C
C           Loop over scans.  Look for setup changes.  Assume for the
C           purposes of this check that a change of setup group
C           means a change in the RF of the IF and so requires a new
C           analog level.  That is not strictly true but is close enough
C           for the current purpose given that this just issue warnings.
C
C           Also check that the scan gap is more than the resampler
C           leveler needs.
C
C           Initializations:
C
            LSCN = 0
            LASTSET = 0
            DO KS = 1, NSET
               SETSEEN(KS) = .FALSE.
            END DO
C
C           Loop over scans, looking only at scans using this station.
C
            DO ISCN = SCAN1, SCANL
               IF( STASCN(ISCN,ISTA) ) THEN
C
C                 Get the start time that will be used by the on-line
C                 system.  It is the "start" plus the start offset as
C                 written to the VEX file.  That will be the later of
C                 the tape start time ("start" in the Vex file) and 
C                 when the antenna gets to source (TONSRC - which, when
C                 later than the start time, shows up as an offset from
C                 the tape start time in the $SCHED section).
C
C                 Note that the main SCHED would allow TPSTART to be 
C                 station dependent within a scan, but the VEX part of
C                 SCHED will not allow that.  All stations must start
C                 their "tape recording" at the same time and that time
C                 is specified by "start=".  The VEX file also records
C                 the expected start time of good data via an offset.
C                 Wide band disk recordings are not started until that 
C                 expected good data time is reached, and that can be 
C                 station dependent.  That is the time that matters 
C                 regarding the setting of levels.
C
                  STARTS = MAX( TONSRC(ISCN,ISTA), 
     1                     STARTJ(ISCN) - TPSTART(ISCN,ISTA) ) 
C
                  KS = NSETUP(ISCN,ISTA)
                  IF( KS .NE. LASTSET ) THEN
C
C                    First check if there was time for an analog level 
C                    reset when there is a setup change.  Only worry
C                    about the first time the setup is seen.  There
C                    doesn't seem to be any reasonable way to deal
C                    with restarts and still allow frequency referencing.
C
                     IF( LSCN .NE. 0 ) THEN
                       IF( STARTS - STOPJ(LSCN) .LT. 
     1                     ( TSAMPLEV - TOL ) * ONESEC ) THEN
C
C                         Issue a description of the problem.
C
                          IF( WARNMSG .AND. .NOT. SETSEEN(KS) ) THEN
                             CALL WRTMSG( 1, 'RDBELEVT', 
     1                            "Level_Settings" )
                             WARNMSG = .FALSE.
                          END IF
C
C                         Issue the specific warning which is different
C                         the first time than later because it will always
C                         be a problem the first time, but usually not later.
C
                          IF( SETSEEN(KS) ) THEN
C
C                            Don't do this one, but save the code in case
C                            we decide to change that.
C
C                             IF( TSWARN1 ) THEN
C
C                               Issue a mild warning
C
C                                MSGTXT = ' '
C                                WRITE( MSGTXT, '( 4A, I4 )' ) 
C     1                             'RDBELEVT: Excessively short scan ',
C     2                             'gap at ', STANAME(ISTA), 
C     3                             ' at scan ', ISCN
C                                CALL WLOG( 1, MSGTXT )
C                                MSGTXT = ' '
C                                WRITE( MSGTXT, '( 10X, 2A, I4, A )' ) 
C     1                             'after switch back to use of a ', 
C     2                             'setup group, ', KS, 
C     3                             ', that was seen earlier.'
C                                CALL WLOG( 1, MSGTXT )
C                                CALL WLOG( 1, 
C     1                           '          This could degrade data '//
C     2                           'if the on-line system is restarted.' )
C                             END IF
C                             WARNMSG = .FALSE.
C                             TSWARN1 = .FALSE.
                          ELSE
                             IF( WARNMSG ) THEN
                                CALL WRTMSG( 1, 'RDBELEVT', 
     1                               "Level_Settings" )
                                WARNMSG = .FALSE.
                             END IF
                             WARNMSG = .FALSE.
                             IF( TSWARN2 ) THEN
C
C                               Strong warning
C
                                MSGTXT = ' '
                                WRITE( MSGTXT, '( 2A, I4, A )' ) 
     1                             'RDBELEVT: Excessively short scan ',
     2                             'gap for the first scan (', ISCN, ')'
                                CALL WLOG( 1, MSGTXT )
                                MSGTXT = ' '
                                WRITE( MSGTXT, '( 10X, 4A )' ) 
     1                             'with setup file',
     2                             SETNAME(KS)(1:LEN1(SETNAME(KS))), 
     3                             ' at ', STANAME(ISTA)
                                CALL WLOG( 1, MSGTXT )
                                CALL WLOG( 1, 
     1                           '          This will cause loss of '//
     2                           'data at the start of this scan.' )
                             END IF
                             TSWARN2 = .FALSE.
                          END IF
                       END IF
                     END IF
C
                  END IF
C
C                 Always check if there is time for the resampling level set.
C                 If no gap is set, both one of the above warnings, and
C                 this one might be triggered.
C
                  IF( LSCN .NE. 0 ) THEN
                     IF( STARTS - STOPJ(LSCN) .LT. TRESAMP * ONESEC )
     1                  THEN
                       IF( WARNMSG ) THEN
                          CALL WRTMSG( 1, 'RDBELEVT', 
     1                         "Level_Settings" )
                          WARNMSG = .FALSE.
                       END IF
                       WARNMSG = .FALSE.
                       IF( TRWARN ) THEN
C
C                         Mild warning
C
                          WRITE( MSGTXT, '( 2A, I4, 2A )' ) 
     1                       'RDBELEVT: Excessively short scan ',
     2                       'gap for scan', ISCN, 
     3                       ' without setup change at ', STANAME(ISTA)
                          CALL WLOG( 1, MSGTXT )
                          CALL WLOG( 1, '          The first few '//
     1                       'of the scan may be corrupted.' )

                       END IF
                       TRWARN = .FALSE.
                     END IF
                  END IF
C
C                 Keep track of the last setup and scan for this station
C                 and keep track of whether this setup has been seen before.
C
                  LASTSET = KS
                  LSCN = ISCN
                  SETSEEN(KS) = .TRUE.
               END IF
            END DO
         END IF
      END DO
C
      RETURN
      END
