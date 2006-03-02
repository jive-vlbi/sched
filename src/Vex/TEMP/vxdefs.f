      SUBROUTINE VXDEFS
C
C     Routine specific for the VEX extension of SCHED. 
C     Writes the different $sections in the VEX output, except $SCHED
C     I think this is a logical order...
C     By H.J. van Langevelde, JIVE, 301196 
C
      INCLUDE 'sched.inc'
C ----------------------------------------------------------------------
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on GL def ')
      CALL VXWRGL
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on EX def ')
      CALL VXWREX
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on MD def ')
      CALL VXWRMD
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on ST def ')
      CALL VXWRST
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on PR def ')
      CALL VXWRPR
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on SI def ')
      CALL VXWRSI
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on AN def ')
      CALL VXWRAN
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on DA def ')
      CALL VXWRDA
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on SU def ')
      CALL VXWRSU
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on FQ def ')
      CALL VXWRFQ
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on IF def ')
      CALL VXWRIF
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on BB def ')
      CALL VXWRBB
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on PH def ')
      CALL VXWRPH
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on TR def ')
      CALL VXWRTR
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on HP def ')
      CALL VXWRHP
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on PO def ')
      CALL VXWRPO
C
      IF (DEBUG) CALL WLOG( 1,'   VXDEFS: Start on RL def ')
      CALL VXWRRL
C
      RETURN
      END
