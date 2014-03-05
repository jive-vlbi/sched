      SUBROUTINE GETFSET
C
C     Routine for SCHED that gets the frequency sets.
C
C     A setup group specifies most of the information about the 
C     configuration at a station.  But SCHED has scan dependent
C     variables that can impose variations on what is in the setup.
C     Those are FREQ, BW, DOPPLER, PCAL, CRDFREQ, CRDBW, and CRDDOP.
C     A frequency set is a unique combination of these parameters.  
C     There will be a different frequency set for each different 
C     combination of these parameters that is found.
C
C     Until August 2013, SCHED also had pulse cal sets.  Those
C     are being merged into the frequency sets to ease some bookkeeping
C     issues.  It was done at the time the ability to set the legacy 
C     system BBCs used for reference pointing separately from the RDBE 
C     channels was added.  RCW.
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
C                       Compare the VLBA legacy system settings.
C                       First require both scans either have or don't
C                       have GOTCRD set, which means they have had
C                       CRDFREQ or CRDDOP specified.  And require that
C                       the same number of channels, CRDNCH, was specified
C                       and that, if a channel mapping, CRDSETCH, was
C                       set, perhaps based on CRDCH1, that it matches.
C                       For the channels, always require that CRDBW
C                       match and that CRDFREQ matches if GOTCRD was
C                       set.  We are allowing CRDNCH, CRDSETCH, CRDCH1,
C                       and CRDBW to be used even when CRDFREQ and 
C                       CRDDOP are not.  I believe CRDSETCH here will
C                       be equivalent to running GETCRDN and testing
C                       CRSETC.
C
C                       Note that the Doppler setting for the legacy 
C                       system has already been done and the results 
C                       are in CRDFREQ.
C
C                       FORTRAN WARNING:  The parentheses around the
C                       .EQV. below are needed or all the logical 
C                       operations earlier in the line are evaluated
C                       as the left side of the .EQV.
C
                        IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
                           MATCH = MATCH .AND.
     1                        CRDNCH(ISCN) .EQ. CRDNCH(KSCN) .AND. 
     2                        ( GOTCRD(ISCN) .EQV. GOTCRD(KSCN) )
                           IF( CRDNCH(ISCN) .GT. 0 ) THEN
                              DO ICH = 1, CRDNCH(ISCN)
                                 IF( GOTCRD(ISCN) ) THEN
                                    MATCH = MATCH .AND. 
     1                              CRDFREQ(ICH,ISCN) .EQ. 
     2                              CRDFREQ(ICH,KSCN)
                                 END IF
                                 MATCH = MATCH .AND. 
     1                             CRDBW(ICH,ISCN) .EQ. CRDBW(ICH,KSCN)
                                 MATCH = MATCH .AND. CRDSETCH(ICH,ISCN)
     1                               .EQ. CRDSETCH(ICH,KSCN)
                              END DO
                           END IF
C
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
