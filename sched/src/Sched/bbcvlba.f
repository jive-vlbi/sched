      SUBROUTINE BBCVLBA( KS )
C
C     Routine for SCHED, called by SETBBC, that sets the BBC
C     assignments for full VLBA systems.  These are systems
C     for which all BBCs can see all 4 IFs.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
      INCLUDE    'schfreq.inc'
C
      INTEGER    ICH, JCH, KS, MMBBC
C -------------------------------------------------------------------  
C     Determine the highest numbered BBC already assigned.
C     Usually if any are to be defaulted, all will be so this will
C     be zero.
C
      MMBBC = 0
      DO ICH = 1, NCHAN(KS)
         MMBBC = MAX( MMBBC, BBC(ICH,KS) )
      END DO
C
C     Loop through channels assigning the channel to the
C     same BBC as a previous one with the same FREQREF and IFCHAN,
C     if there is one (can be difference sideband), or to a new BBC.
C
      DO ICH = 1, NCHAN(KS)
         IF( BBC(ICH,KS) .EQ. 0 .AND. ICH .GT. 1 ) THEN
            DO JCH = 1, ICH-1
               IF( FREQREF(ICH,KS) .EQ. FREQREF(JCH,KS) .AND.
     1             IFCHAN(ICH,KS) .EQ. IFCHAN(JCH,KS) ) THEN
                  BBC(ICH,KS) = BBC(JCH,KS)
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
