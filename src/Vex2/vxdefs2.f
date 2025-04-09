      SUBROUTINE VXDEFS2
C
C     Routine specific for the VEX extension of SCHED. 
C     Writes the different $sections in the VEX output, except $SCHED
C     I think this is a logical order...
C     By H.J. van Langevelde, JIVE, 301196 
C     Labeled by RCW, Nov 2011, after too much confusion.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C      
      INTEGER KS
C ----------------------------------------------------------------------
C
C     Global
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on GL def ')
      CALL VXWRGL
C
C     Experiment
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on EX def ')
      CALL VXWREX
C
C     Mode
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on MD def ')
      CALL VXWRMD2
C
C     Station
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on ST def ')
      CALL VXWRST
C
C     Sites
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on SI def ')
      CALL VXWRSI
C
C     Antennas.
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on AN def ')
      CALL VXWRAN2
C
C     DAS
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on DA def ')
      CALL VXWRDA2
C
C     Source
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on SU def ')
      CALL VXWRSU2
C
C     Frequencies.
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on FQ def ')
      CALL VXWRFQ2
C
C     IFs
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on IF def ')
      CALL VXWRIF2
C
C     BBCs
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on BB def ')
      CALL VXWRBB2
C
C     Phase cal.
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on PH def ')
      CALL VXWRPH2
C
C     Tracks  (This is needed in practice, if not in principle,
C     for OBSTYP=PTVLBA.  There is code in the routine to 
C     provide dummy data for such projects).
C      
      KS = 1
      IF ( FORMAT(KS)(1:6) .EQ. 'MARK5B') THEN
          IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on BS def ')
          CALL VXWRBS     
      ELSE
          IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on DS def ')
          CALL VXWRDS
      END IF
C
C     Extensions
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS2: Start on EXT def ')
      CALL VXWREXT
C
      RETURN
      END
