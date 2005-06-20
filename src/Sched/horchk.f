      CHARACTER*1 FUNCTION HORCHK( KSTA, HA, AZ, EL, KSRC )
C
C     Routine for sched that checks if a source is above the
C     hardware limits and the local horizon as specified in the
C     antennas file.
C
C     Returns ' ' if the source is up.
C             'D' if it is below the hardware limits.
C             'H' if it is above the hardware limits but below the
C                   horizon mask.
C
C     Note that KSTA is the station number in the station catalog.
C     Call with STANUM(ISTA) where ISTA is the station in SCHED's
C     internal schedule.  Also KSRC is the source number in the
C     catalog.
C
      INCLUDE 'sched.inc'
C
      INTEGER   KSTA, NH, I, KSRC
      REAL      HA, AZ, EL, HEL, DEC4D, AZTEST
      REAL      AZA, AZB, ELA, ELB
      REAL      TAX1, TAX2
      CHARACTER*1  UP
C -----------------------------------------------------------------
C     Check the slew limits for the various antenna types using the
C     type specific subroutines.
C
      IF( MOUNT(KSTA) .EQ. 'ALTAZ' ) THEN
         CALL MTALTAZ( KSTA, UP, AZ, EL, TAX1, TAX2 )
      ELSE IF( MOUNT(KSTA) .EQ. 'EQUAT' ) THEN
         DEC4D = DECP(KSRC) / RADDEG
         CALL MTEQUAT( KSTA, UP, EL, HA, DEC4D, TAX1, TAX2 )
      ELSE IF( MOUNT(KSTA) .EQ. 'XYNS' ) THEN
         CALL MTXYNS( KSTA, UP, AZ, EL, TAX1, TAX2 )
      ELSE IF( MOUNT(KSTA) .EQ. 'XYEW' ) THEN
         CALL MTXYEW( KSTA, UP, AZ, EL, TAX1, TAX2 )
      ELSE
C
C        Take the opportunity to check for valid mount type since
C        this is a place that will require update if any are added.
C
         CALL ERRLOG( 'HORCHK: Invalid mount type '//MOUNT(KSTA)//
     1       ' for '//STATION(KSTA) )
      END IF
C
C     Transfer the result to HORCHK.  I'm uneasy about putting 
C     HORCHK in the calls.
C
      HORCHK = UP
C
C     Check the horizon mask it the source is otherwise up.
C
C     Note that, if NHORIZ(KSTA)=1, one of first two IF conditions
C     will apply so all cases are covered.
C
      IF( HORCHK .EQ. ' ' ) THEN
C
C        Need to check azimuth in 0-360 range.  Also rigorously
C        prevent use of uninitialized variables even though the IF's
C        should include all cases.
C
         AZTEST = MOD( AZ, 360.0 )
         IF( AZTEST .LT. 0.0 ) AZTEST = AZTEST + 360.0         
         AZA = 0.0
         AZB = 360.0
         ELA = 0.0
         ELB = 0.0
C
         NH = NHORIZ(KSTA)
         IF( NH .GT. 0 ) THEN
            IF( AZTEST .LE. HORAZ(1,KSTA) ) THEN
               AZA = HORAZ(NH,KSTA) - 360.0
               AZB = HORAZ(1,KSTA)      
               ELA = HOREL(NH,KSTA)
               ELB = HOREL(1,KSTA)
            ELSE IF( AZTEST .GT. HORAZ(NH,KSTA) ) THEN
               AZA = HORAZ(NH,KSTA)
               AZB = HORAZ(1,KSTA) + 360.0
               ELA = HOREL(NH,KSTA)
               ELB = HOREL(1,KSTA)
            ELSE IF( NH .GE. 2 ) THEN
               DO I = 2, NH
                  IF( AZTEST .LE. HORAZ(I,KSTA) ) THEN
                     AZA = HORAZ(I-1,KSTA)
                     AZB = HORAZ(I,KSTA)
                     ELA = HOREL(I-1,KSTA)
                     ELB = HOREL(I,KSTA)
                     GO TO 100
                  END IF
               END DO
  100          CONTINUE
            END IF
C
C           Now interpolate to the current horizon.
C           Don't divide by zero.
C
            IF( AZA .EQ. AZB ) THEN
               HEL = ELA
            ELSE
               HEL = ELA + ( ELB - ELA ) * (AZTEST - AZA) / (AZB - AZA )
            END IF
            IF( EL .LT. HEL ) HORCHK = 'H'
         END IF
      END IF
C
      RETURN
      END

