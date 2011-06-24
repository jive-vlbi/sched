      SUBROUTINE OPTCSPT( ISCN1, ISCN2, NSTA, POINTS, ARPTS, SCNSTA, 
     1                LSRC, MAXISC, MAXSTA, MINANT1, MINANT2,
     2                SCN1, SCN2, NS1, NS2, NS3 )
C
C     Routine for SCHED subroutine OPTCSUB that calculates the
C     quality factor for each subarray combination (ARPTS) given the
C     quality factor for each source/antenna combination (POINTS)
C
C     This is in a subroutine so that it can be repeated easily.
C
C     It is called with two input sources (scans) specified by
C     ISCN1 and ISCN2.  For each antenna, it picks which source to
C     use by which has the highest points for that antenna.  It then
C     adjusts to conform to the required minimum number of antennas
C     per subarray.  It may end up putting all stations on the same
C     source in this process.  
C
      INTEGER      NSTA, MAXSTA, MAXISC, MINANT1, MINANT2
      INTEGER      ISCN1, ISCN2, SCNSTA(MAXSTA), LSRC(MAXSTA)
      INTEGER      ISTA, N1, N2, N3, ISWITCH, NJ, NK
      INTEGER      SCN1, SCN2, NS1, NS2, NS3
      REAL         POINTS(MAXISC,MAXSTA), DIFF, MDIFF
      REAL         ARPTS, PTSUM, PTSJ, PTSK, PENALTY
C ------------------------------------------------------------------
C     Loop over antennas. 
C
      PTSUM = 0.0
      N1    = 0
      N2    = 0
      N3    = 0
      DO ISTA = 1, NSTA
C
C        Pick the best of the two sources for the antenna.
C
         IF( POINTS(ISCN1,ISTA) .GE. POINTS(ISCN2,ISTA) .AND.
     1       POINTS(ISCN1,ISTA) .GT. 0.0 ) THEN
            N1 = N1 + 1
            PTSUM = PTSUM + POINTS(ISCN1,ISTA)
            LSRC(ISTA) = ISCN1
         ELSE IF( POINTS(ISCN2,ISTA) .GT. 0.0 ) THEN
            N2 = N2 + 1
            PTSUM = PTSUM + POINTS(ISCN2,ISTA)
            LSRC(ISTA) = ISCN2
         ELSE
C
C           Leave unassigned if neither is up.
C
            N3 = N3 + 1
            LSRC(ISTA) = 0
         END IF
C
      END DO
C
C     Get the best 2 subarray combinations that satisfy the
C     minimum number of antenna constraints.  This is only
C     possible if there are enough antennas up.
C
  100 CONTINUE
         IF( N1 .LT. MINANT1 .AND. N1 + N2 .GE. MINANT1 + MINANT2 ) THEN
            MDIFF = PTSUM
            ISWITCH = 0
            DO ISTA = 1, NSTA
               IF( LSRC(ISTA) .EQ. ISCN2 .AND. 
     1             POINTS(ISCN1,ISTA) .GT. 0.0 ) THEN
                  DIFF = POINTS(ISCN2,ISTA) - POINTS(ISCN1,ISTA)
                  IF( DIFF .LT. MDIFF ) THEN
                     ISWITCH = ISTA
                     MDIFF = DIFF
                  END IF
               END IF
            END DO
C
C           If found a possible switch to make, do it.  Sometimes
C           there will be no stations where the other source is up.
C
            IF( ISWITCH .NE. 0 ) THEN
               LSRC(ISWITCH) = ISCN1
               PTSUM = PTSUM - MDIFF
               N1 = N1 + 1
               N2 = N2 - 1
               GO TO 100
            END IF
         ELSE IF( N2 .LT. MINANT2 .AND. N1 + N2 .GE. 
     1              MINANT1 + MINANT2 ) THEN
            MDIFF = PTSUM
            ISWITCH = 0
            DO ISTA = 1, NSTA
               IF( LSRC(ISTA) .EQ. ISCN1 .AND. 
     1             POINTS(ISCN2,ISTA) .GT. 0.0) THEN
                  DIFF = POINTS(ISCN1,ISTA) - POINTS(ISCN2,ISTA)
                  IF( DIFF .LT. MDIFF ) THEN
                     ISWITCH = ISTA
                     MDIFF = DIFF
                  END IF
               END IF
            END DO
            IF( ISWITCH .NE. 0 ) THEN
               LSRC(ISWITCH) = ISCN2
               PTSUM = PTSUM - MDIFF
               N1 = N1 - 1
               N2 = N2 + 1
               GO TO 100
            END IF
         END IF
C
C     If either N1 or N2 is still too small, force 
C     choice of a single source case.
C
      IF( N1 .LT. MINANT1 .OR. N2 .LT. MINANT2 ) THEN
C
         PTSUM = 0.0
         N1    = 0
         N2    = 0
         N3    = NSTA
C
      END IF    
C
C     The above selects the best subarrayed  option.  Now compare
C     this with having all scans on the same source.
C     Get the point sums for the single source cases.
C
      PTSJ = 0.0
      PTSK = 0.0
      NJ   = 0
      NK   = 0
      DO ISTA = 1, NSTA
         IF( POINTS(ISCN1,ISTA) .GT. 0.0 ) THEN
            PTSJ = PTSJ + POINTS(ISCN1,ISTA)        
            NJ = NJ + 1
         END IF
         IF( POINTS(ISCN2,ISTA) .GT. 0.0 ) THEN
            PTSK = PTSK + POINTS(ISCN2,ISTA)
            NK = NK + 1
         END IF
      END DO
C
C     Insure that inadequate arrays don't get used.
C
      IF( NJ .LT. 2 ) THEN
         NJ = 0
         PTSJ = 0.0
      END IF
      IF( NK .LT. 2 ) THEN
         NK = 0
         PTSK = 0.0
      END IF
C
C     Get best combination.  Add a penalty factor for the
C     subarray case since full arrays have advantages.
C
      PENALTY = 0.6
      IF( PTSUM * PENALTY .GT. PTSJ .AND. 
     1    PTSUM * PENALTY .GT. PTSK ) THEN
C
C        The subarrays were best and are ok.
C        No need to change N1, N2, N3, LSRC or PTSUM
C
      ELSE IF( PTSJ .NE. 0.0 .OR. PTSK .NE. 0.0 ) THEN
C
C        One of the single source options was best.
C        Install it.  At this point allow as few as 2 antennas.
C
         N1 = 0
         N2 = 0
         N3 = 0
         PTSUM = 0.0
         IF( PTSJ .GE. PTSK ) THEN
C
C           The first source was best.
C
            DO ISTA = 1, NSTA
               IF( POINTS(ISCN1,ISTA) .NE. 0.0 ) THEN
                  LSRC(ISTA) = ISCN1
                  PTSUM = PTSUM + POINTS(ISCN1,ISTA)
                  N1 = N1 + 1
               ELSE
                  LSRC(ISTA) = 0
                  N3 = N3 + 1
               END IF
            END DO
C
         ELSE
C
C           The second source was best.
C
            DO ISTA = 1, NSTA
               IF( POINTS(ISCN2,ISTA) .NE. 0.0 ) THEN
                  LSRC(ISTA) = ISCN2
                  PTSUM = PTSUM + POINTS(ISCN2,ISTA)
                  N2 = N2 + 1
               ELSE
                  LSRC(ISTA) = 0
                  N3 = N3 + 1
               END IF
            END DO
C
         END IF
C
      ELSE
C
C        No good data are available.
C
         PTSUM = 0.0
         N1    = 0
         N2    = 0
         N3    = NSTA
         DO ISTA = 1, NSTA
            LSRC(ISTA) = 0
         END DO
C         
      END IF
C
C     Now determine if the current subarrays are better than ones
C     previously tested.
C
      IF( PTSUM .GT. ARPTS ) THEN
         ARPTS = PTSUM
         DO ISTA = 1, NSTA
            SCNSTA(ISTA) = LSRC(ISTA)
         END DO
         NS1 = N1
         NS2 = N2
         NS3 = N3
         SCN1 = ISCN1
         SCN2 = ISCN2
      END IF
C
      RETURN
      END
