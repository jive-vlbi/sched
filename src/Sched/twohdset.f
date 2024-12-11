      SUBROUTINE TWOHDSET
C
C     Routine for SCHED that warns the user of "twohead" mode
C     and sets the HEADMODE, which determines the positioning
C     of the heads as a function of pass in VLBA and MKIV systems.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER   ISTA 
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'TWOHDSET: starting' )
C
C     Let the user know that 2 head mode is being used.
C     Warn the user if any stations can't do it.  Those sations will
C     only record half the data.  Such stations should not now exist
C     after the switch to Mark5.  This will need further modification
C     soon for Mark5C (comment June 2009).
C
      IF( TWOHEAD ) THEN
         CALL WLOG( 1, 'TWOHDSET: Using 64 track mode for wide ' //
     1       'bandwidth.' )
      END IF
      IF( FASTTRK ) THEN
         CALL WLOG( 1, 'TWOHDSET:  Using 16 Mbps per track record '//
     1     'rate.  Mark IV format on disk only.' )
      END IF
C
C     Set the head mode (HEADMODE) to use for each station.  This
C     determines the pattern of head positions both in terms of
C     sequence and actual offsets.  All VLBA observations, and all
C     MarkIV single head observations use the same mode.  MarkIV
C     two head modes use a different mode, for which the number of
C     head positions is different from the single pass case.
C
C     For the Mark IV 2 head mode, set the number of passes per
C     head position to an appropriately low value, overriding
C     the default which is normally 14.  The tape initialization
C     parameters were rearranged to have a set for every station
C     in order to make this possible without messing up other
C     stations.
C
C     This is not relevant to disk recordings, but it doesn't hurt
C     to set it.
C
      DO ISTA = 1, NSTA
         IF( HEADMODE(ISTA) .EQ. ' ' ) THEN
            IF( TWOHEAD .AND. NHEADS(STANUM(ISTA)) .GT. 1 .AND.
     1          ( RECORDER(STANUM(ISTA)) .EQ. 'MKIV' .OR.
     2            RECORDER(STANUM(ISTA)) .EQ. 'VLBA4' ) ) THEN
               HEADMODE(ISTA) = 'MKIV2H'
            ELSE
               HEADMODE(ISTA) = 'VLBA14'
            END IF
         END IF
      END DO
      DO ISTA = 1, NSTA
         IF( HEADMODE(ISTA) .EQ. 'MKIV2H' ) THEN
            NHDPOS(ISTA) = 6
         END IF
      END DO
C
      RETURN
      END
      
