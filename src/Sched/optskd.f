      SUBROUTINE OPTSKD( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
Cf2py intent(in) LASTISCN, KSCN, ISCN
Cf2py intent(out) ADJUST, KEEP, DONE
C
C     Routine for optimization mode of sched that decides what to
C     do with the next input scan.  It is used for OPTMODE=SCANS.
C     It will not decide to leave it out of the list, so it is
C     not necessary to create a new scan list.  But it may flag the
C     scan not to be used, which the rest of SCHED understands.  It
C     does that by setting all STASCN entries .FALSE.
C
C
      INCLUDE 'sched.inc'
C
      INTEGER           LASTISCN(MAXSTA), KSCN, ISCN
      LOGICAL           KEEP, ADJUST, DONE
C
      INTEGER           NGOOD, NPRIO, SKIP(MAXSRC), ISTA, ISRC
      INTEGER           MISSED
      DOUBLE PRECISION  TAPPROX, TBEGIN, T_AVAIL, LASTTIME
      LOGICAL           OKSTA(MAXSTA)
      SAVE              SKIP, MISSED
C ---------------------------------------------------------------------
      DONE = .FALSE.
C
C     KSCN is the count of input scans for this optimization mode.
C     Quit when all have been processed.
C
      IF( KSCN .GT. NSCANS ) THEN
         DONE = .TRUE.
         GO TO 999
      END IF
C
C     Copy the input scan to the output if necessary.
C
      IF( KSCN .NE. ISCN ) THEN
         CALL SCNDUP( ISCN, KSCN, .TRUE., 'OPTSKD' )
      END IF
C
C     Determine whether to use input scan time.  Only do this for
C     scans for which no fixed time was specified (used DUR or DWELL
C     so DURONLY = 1).  By this rule, the first scan will keep
C     it's start time.  (The old version also allowed adjustments
C     if just the stop time was specified).
C
      ADJUST = DURONLY(ISCN) .EQ. 1 
C
C     Initialize stuff for skipping scans - the mode initialized
C     with OPSKIP.  This causes a source to be skipped OPSKIP times 
C     before it is used.  If it is in a favored elevation window,
C     set by OPELPRIO, every pass will be taken.  Initialize SKIP
C     so a source is taken the first pass.
C     Also initialize the counter of missed scans.
C
      IF( ISCN .EQ. 1 ) THEN
         DO ISRC = 1, MAXSRC
            SKIP(ISRC) = OPSKIP
         END DO
         MISSED = 0
      END IF
C
C     Get the time for which to do the geometric calculations.
C
      IF( .NOT. ADJUST ) THEN
C
C        If the times are not going to be adjusted, use the start
C        time of the scan.
C
         TAPPROX = STARTJ(ISCN)
C
      ELSE
C
C        Otherwise, get the most recent stop time for any antenna 
C        scheduled to be in this scan.   
C        Use the nominal start time if this is the first scan for
C        all participating stations.  Also, make sure if only some 
C        antennas are new, they don't set the start time.
C
         TAPPROX = TFIRST
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               IF( LASTISCN(ISTA) .NE. 0 ) THEN
                  TAPPROX = MAX( TAPPROX, 
     1                 STOPJ(LASTISCN(ISTA)) + GAP(ISCN) )
               END IF
            END IF
         END DO
      END IF
C
C     Loop through stations to see how many are up.
C     Also see how many are in prefered elevation ranges.
C     Get TBEGIN which is an estimated start time for the scan
C     could start taking into account slews, which TAPPROX does not.
C     It can be earlier than TAPPROX if the station that set
C     that time is not selected.  If no stations had a previous
C     time, use the experiment start time.
C
      NGOOD = 0
      NPRIO = 0
      TBEGIN = TFIRST
      DO ISTA = 1, NSTA
         OKSTA(ISTA) = .FALSE.
         IF( STASCN(ISCN,ISTA) ) THEN
            CALL STAGEO( ISCN, ISTA, TAPPROX, LASTISCN(ISTA),  
     1          LASTTIME, T_AVAIL, 'OPTSKD' )
            IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     1          UP2(ISCN,ISTA) .EQ. ' ' .AND. 
     2          EL1(ISCN,ISTA) .GT. OPMINEL(ISCN) .AND.
     3          EL2(ISCN,ISTA) .GT. OPMINEL(ISCN) ) THEN
               OKSTA(ISTA) = .TRUE.
               NGOOD = NGOOD + 1
               IF( LASTISCN(ISTA) .NE. 0 ) THEN
                  T_AVAIL = MAX( T_AVAIL, 
     1                  STOPJ(LASTISCN(ISTA)) + GAP(ISCN) )
                  TBEGIN = MAX( TBEGIN, T_AVAIL )
               END IF
               IF( ( EL1(ISCN,ISTA) .GT. OPELPRIO(1) .AND.
     1               EL1(ISCN,ISTA) .LT. OPELPRIO(2) ) .OR.
     2             ( EL1(ISCN,ISTA) .GT. OPELPRIO(3) .AND.
     3               EL1(ISCN,ISTA) .LT. OPELPRIO(4) ) ) 
     4                NPRIO = NPRIO + 1
            END IF
         END IF
      END DO
C
C     If there were no stations that were up and had a previous
C     scan, use the same default as TAPPROX.  If this is because
C     all stations are down, this scan will be rejected so it
C     doesn't matter.  If it is because all good stations are 
C     on their first scan, use the experiment start time.
C     As for TAPPROX, if the scan time is not to be adjusted,
C     use the specified scan start time.
C
      IF( .NOT. ADJUST .OR. TBEGIN .EQ. TFIRST ) TBEGIN = TAPPROX
C
C     Get source number for this scan.  This just saves nested
C     subscripts later.
C
      ISRC = SRCNUM(ISCN)
C
C     Determine whether to accept the scan.  If OPMIAN was not
C     set, require at least one good antenna.
C
      KEEP = ( NGOOD .GE. MAX( OPMIAN(ISCN), 1 ) .AND. 
     1       ( SKIP(ISRC) .GE. OPSKIP .OR. NPRIO .GE. 1 ) )
C
C     The following deactivates the minimum number of antennas
C     and the skip ability for pointing and antenna temperature
C     observations.  This allows a single antenna (eg MK) to be
C     doing pointing while the rest of the array is doing 
C     regular observing with OPMIAN set or skipping turned on.
C
C     The deactivation is not done if the OBSTYPE indicates that
C     this is single dish observing.
C
C
      IF( OBSTYP .NE. 'NONE' .AND. OBSTYP .NE. 'PTVLBA' ) THEN
         KEEP = KEEP .OR. ( NGOOD .GE. 1 .AND. 
     1       ( PNTVLBA(ISCN) .OR. TANVLBA(ISCN) .OR. DOPN3DB(ISCN) ) )
      END IF
C
C     Only keep the scan if a sufficient number of preceeding scans
C     have been missed.  This allows for low priority sources.
C
      KEEP = KEEP .AND. MISSED .GE. OPMISS(ISCN)
C
      IF( KEEP ) THEN
C
         SKIP(ISRC) = 0
C
C        Set the start time to TBEGIN if we are going to adjust scan
C        times.  Otherwise keep the original.
C
         IF( ADJUST ) THEN
            STARTJ(ISCN) = TBEGIN
            STOPJ(ISCN)  = TBEGIN + DUR(ISCN)
         END IF
C
C        Set which stations will be in the scan.  Take only ones that 
C        are up unless told to take all.
C
         DO ISTA = 1, NSTA
            STASCN(ISCN,ISTA) = OKSTA(ISTA) .OR. OPNOSUB
         END DO
C
         MISSED = 0
      ELSE 
C
C        Increment the skip counter if the scan is not kept.
C
         IF( NGOOD .GE. OPMIAN(ISCN) ) THEN
            SKIP(ISRC) = SKIP(ISRC) + 1
         END IF
C
C        Turn off all stations for the scan.
C
         DO ISTA = 1, NSTA
            STASCN(ISCN,ISTA) = .FALSE.
         END DO
C
         MISSED = MISSED + 1
      END IF
C
  999 CONTINUE
C
      RETURN
      END
