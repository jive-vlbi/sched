      SUBROUTINE GETFSET
C
C     Routine for SCHED that gets the frequency sets.
C     These are unique combinations of setup group, frequency, and
C     bandwidth after the effects of Doppler.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISCN, ISTA, IFS, KS, ICH, JF
      LOGICAL    MATCH, FSMATCH
C ----------------------------------------------------------------------
      NFSET = 0
C
C     Loop through scans and stations accumulating the frequency sets.
C
      DO ISCN = SCAN1, SCANL
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               IF( NFSET .EQ. 0 ) THEN
C
C                 Deal with first data.
C
                  NFSET = 1
                  FSETI(ISCN,ISTA) = 1
                  FSETKS(NFSET) = NSETUP(ISCN,ISTA)
                  FSETSCN(NFSET) = ISCN
                  FSSAME(NFSET) = NFSET
               ELSE
C
C                 No longer first, look for a previous match.
C
                  FSETI(ISCN,ISTA) = 0
C
C                 Get the current setup.
C
                  KS = NSETUP(ISCN,ISTA)
C
C                 Now compare with previous fsets.
C                  
                  DO IFS = 1, NFSET
C
C                    Compare setup group.
C
                     MATCH = FSETKS(IFS) .EQ. KS
C
C                    Compare frequency and bandwidth.
C
                     IF( MATCH ) THEN
                        DO ICH = 1, NCHAN(KS)
                           MATCH = MATCH .AND. 
     1                        FREQ(ICH,ISCN) .EQ. FREQ(ICH,FSETSCN(IFS))
                           MATCH = MATCH .AND.
     1                        BW(ICH,ISCN) .EQ. BW(ICH,FSETSCN(IFS))
                        END DO
                     END IF
C
C                    Deal with a match.
C
                     IF( MATCH ) THEN
                        FSETI(ISCN,ISTA) = IFS
                     END IF
                  END DO
C
C                 Need a new set - FSETI was not set above.
C
                  IF( FSETI(ISCN,ISTA) .EQ. 0 ) THEN
                     NFSET = NFSET + 1
                     IF( NFSET .GT. MFSET ) THEN
                        CALL ERRLOG( 
     1                      'GETFSET: Too many frequency sets.')
                     END IF
                     FSETI(ISCN,ISTA) = NFSET
                     FSETKS(NFSET) = KS
                     FSETSCN(NFSET) = ISCN
C
C                    Also set the pointer to the lowest numbered
C                    setup that has the same frequency parameters.
C                    Loop backwards so FSSAME is the smallest match.
C
                     FSSAME(NFSET) = NFSET
                     IF( NFSET .GE. 2 ) THEN
                        DO JF = NFSET - 1, 1, -1
                          IF( FSMATCH( NFSET, JF ) ) THEN
                              FSSAME(NFSET) = JF
                           END IF
                        END DO
                     END IF     
C
                  END IF
C
               END IF
            END IF
         END DO
      END DO
C
      RETURN
      END
