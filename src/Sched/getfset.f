      SUBROUTINE GETFSET
C
C     Routine for SCHED that gets the frequency sets.
C
C     A setup group specifies most of the information about the 
C     configuration at a station.  But SCHED has scan dependent
C     variables that can impose variations on what is in the setup.
C     Those are FREQ, BW, DOPPLER, PCAL, CRDFREQ, CRDBW, CRDDOP, and
C     PCAL.  A frequency set is a unique combination of these 
C     parameters.  There will be a different frequency set for each
C     different combination of these parameters is found.
C
C     Until August 2013, SCHED also had pulse cal sets.  Those
C     are being merged into the frequency sets to ease some bookkeeping
C     issues at the time the ability to set the legacy system BBCs
C     used for reference pointing separately from the RDBE channels
C     was added.  RCW.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISCN, ISTA, IFS, KS, ICH, JF, KSCN
      LOGICAL    MATCH, FSMATCH
      CHARACTER  GFSPCAL*4
C ----------------------------------------------------------------------
      NFSET = 0
C
C     Loop through scans and stations accumulating the frequency sets.
C
      DO ISCN = SCAN1, SCANL
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
C
C              Get the current setup.
C
               KS = NSETUP(ISCN,ISTA)
C
C              Get the PCAL state for this scan/station.  If it was 
C              specified in the schedule, take that.  If not, use the
C              value from the setup group.  Keep in a local variable
C              for now.
C
               IF( PCAL(ISCN) .NE. ' ' ) THEN
                  GFSPCAL = PCAL(ISCN)
               ELSE
                  GFSPCAL = SPCAL(KS)
               END IF
C
C              Deal with first data scan.
C
               IF( NFSET .EQ. 0 ) THEN
                  NFSET = 1
                  FSETI(ISCN,ISTA) = 1
                  FSETKS(NFSET) = KS
                  FSETSCN(NFSET) = ISCN
                  FSSAME(NFSET) = NFSET
                  FSPCAL(NFSET) = GFSPCAL
               ELSE
C
C                 No longer first, initialize FSETI, the pointer
C                 to a frequency set for each scan/station.
C
                  FSETI(ISCN,ISTA) = 0
C
C                 Look for a previous match.
C                 Compare with previous fsets.
C                  
                  DO IFS = 1, NFSET
C
C                    Compare setup group.  Note this will only 
C                    match for the same station as it is one group
C                    per station.  There will be separate frequency
C                    groups for each station, but details will not
C                    all be listed for matching sets - like the setup
C                    groups.
C
                     MATCH = FSETKS(IFS) .EQ. KS
C
C                    Compare frequency and bandwidth specified with 
C                    a scan.  The Doppler setting has already been 
C                    done and the results are in FREQ.  Note that 
C                    having both zero is a match, probably the most
C                    common kind.  Same with BW.  Recall the FREQ and BW
C                    scan inputs are not station dependent.
C
                     KSCN = FSETSCN(IFS)
                     IF( MATCH ) THEN
                        DO ICH = 1, NCHAN(KS)
                           MATCH = MATCH .AND. 
     1                        FREQ(ICH,ISCN) .EQ. FREQ(ICH,KSCN)
                           MATCH = MATCH .AND.
     1                        BW(ICH,ISCN) .EQ. BW(ICH,KSCN)
                        END DO
C
C                       Compare the VLBA legacy system freq and bw.
C                       used for pointing.  The Doppler setting
C                       for the legacy system has already been done
C                       and the results are in CRDFREQ.
C
                        MATCH = MATCH .AND. CRDNCH(ISCN) .EQ.
     1                        CRDNCH(KSCN)
                        IF( CRDNCH(ISCN) .GE. 1 ) THEN
                           DO ICH = 1, CRDNCH(ISCN)
                              MATCH = MATCH .AND. 
     1                          CRDFREQ(ICH,ISCN) .EQ. CRDFREQ(ICH,KSCN)
                              MATCH = MATCH .AND. 
     1                          CRDBW(ICH,ISCN) .EQ. CRDBW(ICH,KSCN)
                           END DO
                        END IF
C
C                       Compare the pcal state.
C
                        MATCH = MATCH .AND. GFSPCAL .EQ. FSPCAL(IFS)
C
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
                     FSPCAL(NFSET) = GFSPCAL
C
C                    Also set the pointer to the lowest numbered
C                    frequency set that has the same frequency parameters.
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
