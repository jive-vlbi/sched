      SUBROUTINE VXDEFS
C
C     Routine specific for the VEX extension of SCHED. 
C     Writes the different $sections in the VEX output, except $SCHED
C     I think this is a logical order...
C     By H.J. van Langevelde, JIVE, 301196 
C     Labeled by RCW, Nov 2011, after too much confusion.
C
      INCLUDE 'sched.inc'
C ----------------------------------------------------------------------
C
C     Global
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on GL def ')
      CALL VXWRGL
C
C     Experiment
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on EX def ')
      CALL VXWREX
C
C     Mode
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on MD def ')
      CALL VXWRMD
C
C     Station
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on ST def ')
      CALL VXWRST
C
C     Procedures
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on PR def ')
      CALL VXWRPR
C
C     Sites
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on SI def ')
      CALL VXWRSI
C
C     Antennas.
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on AN def ')
      CALL VXWRAN
C
C     DAS
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on DA def ')
      CALL VXWRDA
C
C     Source
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on SU def ')
      CALL VXWRSU
C
C     Frequencies.
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on FQ def ')
      CALL VXWRFQ
C
C     IFs
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on IF def ')
      CALL VXWRIF
C
C     BBCs
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on BB def ')
      CALL VXWRBB
C
C     Phase cal.
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on PH def ')
      CALL VXWRPH
C
C     Tracks  (This is needed in practice, if not in principle,
C     for OBSTYP=PTVLBA.  There is code in the routine to 
C     provide dummy data for such projects).
C      
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on TR def ')
      CALL VXWRTR
C
C     Skip the following for single dish.  They are also not
C     needed for the RDBE, I believe, but deal with that 
C     later.  RCW  Nov 2011.
C
      IF( .NOT. OBSTYP .EQ. 'PTVLBA' ) THEN
C      
C        Head positions
C      
         IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on HP def ')
         CALL VXWRHP
C      
C        Pass order
C      
         IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on PO def ')
         CALL VXWRPO
C      
C        Roll
C      
         IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on RL def ')
         CALL VXWRRL
C
      END IF
C
      RETURN
      END
