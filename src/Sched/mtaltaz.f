      SUBROUTINE MTALTAZ( KSTA, UP, SAZ, SEL, TAZ, TEL )
C
C     Mount type specific for SCHED for ALTAZ antennas that
C     Tests limits to see if source is up at requested position 
C     of SAZ and SEL and, if not, tells where it will actually 
C     be (TAZ, TEL).
C
C     KSTA is the station number in the station catalog.
C     SAZ and SEL are the requested AZ and EL.
C     TAZ and TEL are the actual AZ and EL.
C     UP is a single character.
C
C     Subroutine WRAP finds where in AZ the antenna should be for
C     ALTAZ antennas.  It will assure that the Az is above the
C     lower azimuth limit.  Test here if it is above just in case
C     or the weird case of an antenna with an AZ range of less than
C     360 degrees.
C
      INCLUDE 'sched.inc'
C
      INTEGER           KSTA
      REAL              SAZ, SEL, TAZ, TEL
      CHARACTER*1       UP
C ------------------------------------------------------------------
C     Complain if limits not given.
C
      IF( NAXLIM(KSTA) .NE. 1 ) THEN
         CALL ERRLOG( 'MTALTAZ: Need 1 and only 1 set of axis limits'//
     1     ' for '//STATION(KSTA) )
      END IF
C
C     Set up as if source up, then change if down.
C
      UP = ' '
      TAZ = SAZ
      TEL = SEL
C
C     Elevation limits:
C
      IF( SEL .LT. AX2LIM(1,KSTA) ) THEN
         UP = 'D'
         TEL = AX2LIM(1,KSTA)
      ELSE IF( SEL .GT. AX2LIM(2,KSTA) ) THEN
         UP = 'D'
         TEL = AX2LIM(2,KSTA)
      END IF        
C
C     Azimuth limit (WRAP insured we are above lower limit).
C
      IF( SAZ .GT. AX1LIM(2,KSTA) ) THEN
         UP = 'D'
         TAZ = AX1LIM(2,KSTA)
      END IF
C
C     Check the zenith angle limit.  Don't assume that the antenna
C     will be here - it will probably be at the AX2LIM(1,KSTA)
C
      IF( 90.0 - SEL .GT. ZALIM(KSTA) ) THEN
         UP = 'D'
      END IF
C
      RETURN
      END
