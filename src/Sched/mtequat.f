      SUBROUTINE MTEQUAT( KSTA, UP, SEL, SHA, SDEC, THA, TDEC )
C
C     Mount specific routine for SCHED that checks if a source is
C     up at an equatorial mount antenna when position (SHA,SDEC)
C     is requested.  If not, it returns flag UP and estimated actual
C     position (THA, TDEC), with both coordinates in degrees.
C
C     SHA on input is in hours, as are the HA limits.
C
C     KSTA is the station number in the station catalog.
C
C     The 140' is a primary example.  It will require three sets
C     of limits, so I should add that at some point.  For now, just
C     assume one set of limits plus the ZALIM.
C     Once there are more, require that the sets of limits adjoin, but
C     don't overlap in Dec and are in increasing order of Dec.
C
C     HA in hours, Dec in degrees.
C
      INCLUDE 'sched.inc'
C
      INTEGER         KSTA, IBOX, LOWER, UPPER
      REAL            SEL, SHA, SDEC, THA, TDEC
      REAL            DECLOW, DECHIGH
      LOGICAL         GOTT, CHECK(MAXCAT)
      CHARACTER       UP*1
      SAVE            CHECK
      DATA            CHECK  / MAXCAT*.TRUE. /
C -------------------------------------------------------------------
C     Complain if limits not given.
C
      IF( NAXLIM(KSTA) .LT. 1 .OR. NAXLIM(KSTA) .GT. 3 ) THEN
         CALL ERRLOG( 'MTEQUAT: Need between 1 and 3 sets of axis '//
     1     'limits for '//STATION(KSTA) )
      END IF
C
C     Some initializations.
C
      DECLOW  = AX2LIM(1,KSTA)
      DECHIGH = AX2LIM(2*NAXLIM(KSTA),KSTA)
C
C     The first time we see this source, check the ranges to see that
C     the boxes are in increasing Dec order and the dec boundaries
C     adjoin.  Complain if otherwise.
C
      IF( CHECK(KSTA) ) THEN
         DO IBOX = 1, NAXLIM(KSTA)
            LOWER = 2 * IBOX - 1
            UPPER = 2 * IBOX
C
            IF( AX2LIM(LOWER,KSTA) .GE. AX2LIM(UPPER,KSTA) ) THEN
               CALL ERRLOG( 'MTEQUAT: Lower limit > '//
     1              ' upper limit for dec box for '//STATION(KSTA) )
            END IF
C
            IF( IBOX .LT. NAXLIM(KSTA) ) THEN
               IF( AX2LIM(UPPER,KSTA) .NE. AX2LIM(LOWER+2,KSTA) ) THEN
                  CALL ERRLOG( 'MTEQUAT: Dec limit boxes not '//
     1                 ' adjoining for '//STATION(KSTA) )
               END IF
            END IF
         END DO
         CHECK(KSTA) = .FALSE.
      END IF
C
C     Set up as if the source is up, then change if not.
C
      UP = ' '
      THA = SHA
      TDEC = SDEC
C
C     Initializations.
C
      GOTT = .FALSE.
      DECLOW = AX2LIM(1,KSTA)
      DECHIGH = AX2LIM(2*NAXLIM(KSTA),KSTA)
C
C     Loop through boxes.
C
      DO IBOX = 1, NAXLIM(KSTA)
         LOWER = 2 * IBOX - 1
         UPPER = 2 * IBOX
C         
C        Go through Dec boxes.  Jump out once SDEC found so should
C        
         IF( SDEC .LT. AX2LIM(UPPER,KSTA) ) THEN
            IF( SDEC .LT. AX2LIM(LOWER,KSTA) ) THEN
               UP = 'D'
               TDEC = AX2LIM(LOWER,KSTA)
            END IF
            IF( SHA .LT. AX1LIM(LOWER,KSTA) ) THEN
               UP = 'D'
               THA = AX1LIM(LOWER,KSTA)
            ELSE IF( SHA .GT. AX1LIM(UPPER,KSTA) ) THEN
               UP = 'D'
               THA = AX1LIM(UPPER,KSTA)
            END IF
            IF( UP .EQ. ' ' ) GO TO 100
         END IF
C
      END DO
C
C     Was not in one of the Dec ranges.
C
      UP = 'D'
      LOWER = 2 * NAXLIM(KSTA) - 1
      UPPER = 2 * NAXLIM(KSTA)
      TDEC = AX2LIM(UPPER,KSTA)
      IF( SHA .LT. AX1LIM(LOWER,KSTA) ) THEN
         THA = AX1LIM(LOWER,KSTA)
      ELSE IF( SHA .GT. AX1LIM(UPPER,KSTA) ) THEN
         THA = AX1LIM(UPPER,KSTA)
      ELSE
         THA = SHA
      END IF     
C
C     Jump here once position located.
C
  100 CONTINUE
C
C     Check the zenith angle limit.  Don't assume that the antenna
C     will be here - it will probably be at the AX2LIM(1,KSTA)
C
      IF( 90.0 - SEL .GT. ZALIM(KSTA) ) THEN
         UP = 'D'
      END IF
C
C     Convert the output HA to degrees.
C
      THA = THA * 15
C
      RETURN
      END
