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
      INTEGER      KS, IS, KSO 
C     INTEGER      JS
      INTEGER      LSTA, ISCN, KSI, GNSET
C      LOGICAL      USEIT
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
                  CALL WLOG( 1, '          ' //
     1              'for each station in the input setups.' )
                  WRITE( MSGTXT, '( A, I5, A )' )
     1              '          Maximum is ', MSET, 
     2              ' which is setups times stations.'
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
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
         DO ISCN = 1, NSCANS
            NSETUP(ISCN,LSTA) = GNSET(ISCN,LSTA)
         END DO
      END DO
C
C     Initialize LISTKS to KS.  This will eventually be the setup
C     group that is actually printed in the SUM file for the collection
C     of groups that are basically the same, just for different 
C     stations.
C
      DO KS = 1, NSET
         LISTKS(KS) = KS
      END DO
C
C     Get ISETSTA.  
C
C     This used to be a bit tricky because of the possible
C     defaulting.  As of Dec 2004, each station has a setup so
C     it should no longer be a problem.  Keep the legacy code
C     (commented) in case it is needed some day.
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
C
C        The following defaulting is no longer used.
C
C        Now look for a station to use for a default like VLBA.
C        Use a VLBA station that is not explicitly specified in another
C        group from the same setup file.
C
C         IF( ISETSTA(KS) .EQ. 0 .AND. 
C     1       SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
C
C            write(*,*) 'setexpnd:  Should not get here'
C            DO LSTA = 1, NSTA
C               IF( STANAME(LSTA)(1:4) .EQ. 'VLBA' ) THEN
C                  USEIT = .TRUE.
C                  DO JS = 1, NSET
C                     IF( JS .NE. KS .AND. 
C     1                   ISETNUM(JS) .EQ. ISETNUM(KS) .AND.
C     2                   SETSTA(1,JS) .EQ. STANAME(LSTA) ) THEN
C                        USEIT = .FALSE.
C                     END IF
C                  END DO
C                  IF( USEIT ) THEN
C                     ISETSTA(KS) = STANUM(LSTA)
C                     GO TO 50
C                  END IF
C               END IF
C            END DO
C         END IF
C   50    CONTINUE
C
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





