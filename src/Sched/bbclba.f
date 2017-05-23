      SUBROUTINE BBCLBA( KS )
C
C     Routine for SCHED, called by SETBBC, that sets the BBC
C     assignments for Australian LBA systems.  
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
      INCLUDE    'schfreq.inc'
C
      INTEGER    ICH, JCH, KS, MMBBC
      LOGICAL    GOTMATCH(MCHAN)
C -------------------------------------------------------------------  
      IF( SDEBUG ) CALL WLOG( 0, 'BBCLBA: Starting.' )
C     Determine the highest numbered BBC already assigned.
C     Usually if any are to be defaulted, all will be so this will
C     be zero.
C
      MMBBC = 0
      DO ICH = 1, NCHAN(KS)
         MMBBC = MAX( MMBBC, BBC(ICH,KS) )
         GOTMATCH(ICH) = .FALSE.
      END DO
C
C     Loop through channels assigning the channel to the
C     same BBC as a previous one with the same FREQREF and IFCHAN,
C     if there is one (can be different sideband), or to a new BBC.
C     Note that LBA BBCs can each provide 2 *USB* sidebands with contiguous
C     frequencies or a conventional LSB/USB in 16 MHz mode. In 64 MHz
C     mode, dual sideband is not possible. Use GOTMATCH to record
C     whether a given BBC already has been assigned to a frequency.
C
      DO ICH = 1, NCHAN(KS)
         IF( BBC(ICH,KS) .EQ. 0 .AND. ICH .GT. 1 ) THEN
            DO JCH = 1, ICH-1
               IF( (FREQREF(ICH,KS) .EQ. FREQREF(JCH,KS) .OR.
     1          (FREQREF(ICH,KS) - BBFILT(ICH,KS)) .EQ. FREQREF(JCH,KS))
     2             .AND. (IFCHAN(ICH,KS) .EQ. IFCHAN(JCH,KS) )
     3             .AND. ( ABS(BBFILT(ICH,KS) - 64.0) > 1E-3)
     4             .AND. .NOT. GOTMATCH(JCH) 
     5             .AND. .NOT. GOTMATCH(ICH) ) THEN
                     BBC(ICH,KS) = BBC(JCH,KS)
                     GOTMATCH(JCH) = .TRUE.
                     GOTMATCH(ICH) = .TRUE.
                     GO TO 100
               END IF
            END DO
  100       CONTINUE
         END IF
C
C        If BBC is still 0, assign the next one.
C
         IF( BBC(ICH,KS) .EQ. 0 ) THEN
            BBC(ICH,KS) = MMBBC + 1
         END IF
C
C        Determine new highest assigned BBC.
C
         MMBBC = MAX( MMBBC, BBC(ICH,KS) )
C
      END DO
C
      RETURN 
      END
