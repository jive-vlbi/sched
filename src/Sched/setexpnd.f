      SUBROUTINE SETEXPND
C
C     Routine for SCHED, called by GETSET, that takes setup groups 
C     specified for more than one antenna and expands them out to
C     one group per antenna.  This allows an input setup file specified
C     with only generic parameters to apply to many antennas and
C     yet allows SCHED to make the ultimate information in the setup
C     to be station dependent after utilizing the data from the
C     frequency catalog.
C
C     Also, get ISETSTA, the pointer to the station catalog entry of the
C     station of the final setup group.
C
C     After SETSTDEF, there should be no default stations like "VLBA".
C     This was a change in Dec. 2004.  Now each station has it's own
C     setup.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER      KS, IS, KSO, JS
      INTEGER      LSTA, LSCN, KSI, GNSET
      LOGICAL      USEIT
C  --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETEXPND: Starting.' )
C
C     Eliminate stations that don't use the setup and expand a
C     defaulted station (blank) if that was given.  Note that
C     it is possible that the setup group will not be used at all.
C
      DO KS = 1, NSET
         CALL SETSTDEF( KS )
      END DO
C
C     Going backwards through both the setup groups and the stations,
C     expand each setup file to one group per station, eliminating 
C     unused groups.  Put them at the back of the setup file arrays 
C     to avoid overwrites.  Later, move them up.  MSET is maximum 
C     number of setup groups.
C
      KSO = MSET + 1
C
      DO KS = NSET, 1, -1
         DO IS = MANT, 1, -1
            IF( SETSTA(IS,KS) .NE. ' ' ) THEN
               KSO = KSO - 1
C
C              Check that we will not end up with too many setup groups
C              and that unexpanded ones, including the current one,
C              won't be overwritten.
C
               IF( KSO .LE. KS ) THEN
                  CALL WLOG( 1, 'SETEXPND: ' //
     1              'There are too many setup groups after '//
     2              'creating new ones ' )
                  CALL WLOG( 2, '          ' //
     1              'for each station in the input setups.' )
                  CALL ERRLOG( 'SETEXPND: ' //
     1              'You need fewer setups or a bigger MSET in SCHED.' )
               END IF
C
C              Copy setup to single station version at end of pack.
C
               CALL SETCOP( KS, KSO )
               SETSTA(1,KSO) = SETSTA(IS,KS)
C
            END IF
         END DO
      END DO
C
C     Check that we got something.
C
      IF( KSO .EQ. MSET + 1 ) THEN
         CALL ERRLOG('SETEXPND: None of the specified setups are used.')
      END IF
C
C     Now copy the new ones to the front of the list.
C     NSET will be set to the highest value seen by SETCOP.
C
      NSET = 0
      DO KS = 1, MSET - KSO + 1
         KSI = KS + KSO - 1
         CALL SETCOP( KSI, KS )
         SETSTA(1,KS) = SETSTA(1,KSI)
      END DO
C
C     Fill in the NSETUP array with the setup group used for each
C     scan/station.
C
      DO LSTA = 1, NSTA
         DO LSCN = 1, NSCANS
            NSETUP(LSCN,LSTA) = GNSET(LSCN,LSTA)
         END DO
      END DO
C
C     Get ISETSTA.  
C
C     This used to be a bit tricky because of the possible
C     defaulting.  As of Dec 2004, each station has a setup so
C     it should no longer be a problem.  Keep the legacy code
C     in case it is needed some day.
C
C     Do it by looping through the schedule stations
C     and then the scans looking for a match.  Allow NSETUP
C     to make the final match determination.  Make a simple check
C     here to avoid looping through scans when there is an 
C     obvious mismatch.  Note that, for some default cases, ISETSTA
C     will be for the last matching station.
C
      DO KS = 1, NSET
         ISETSTA(KS) = 0
         DO LSTA = 1, NSTA
            IF( SETSTA(1,KS) .EQ. STANAME(LSTA) ) THEN
               ISETSTA(KS) = STANUM(LSTA)
            END IF
         END DO
C
C        Now look for a station to use for a default like VLBA.
C        Use a VLBA station that is not explicitly specified in another
C        group from the same setup file.
C
         IF( ISETSTA(KS) .EQ. 0 .AND. 
     1       SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
C
            write(*,*) 'setexpnd:  Should not get here'
            DO LSTA = 1, NSTA
               IF( STANAME(LSTA)(1:4) .EQ. 'VLBA' ) THEN
                  USEIT = .TRUE.
                  DO JS = 1, NSET
                     IF( JS .NE. KS .AND. 
     1                   ISETNUM(JS) .EQ. ISETNUM(KS) .AND.
     2                   SETSTA(1,JS) .EQ. STANAME(LSTA) ) THEN
                        USEIT = .FALSE.
                     END IF
                  END DO
                  IF( USEIT ) THEN
                     ISETSTA(KS) = STANUM(LSTA)
                     GO TO 50
                  END IF
               END IF
            END DO
         END IF
   50    CONTINUE
C
C        Complain if could not get a station number.  I think this
C        is always a programming problem.  A station not in the
C        station list should not have survived the earier paring
C        down of the station list.
C
         IF( ISETSTA(KS) .EQ. 0 ) THEN
            CALL WLOG( 1, 'SETEXPND: Did not get station number. ' //
     1        'Programming problem.' )
            CALL WLOG( 1, '          Station: ' // SETSTA(1,KS) )
            CALL ERRSET( KS )
         END IF
C
      END DO
C
      RETURN
      END





