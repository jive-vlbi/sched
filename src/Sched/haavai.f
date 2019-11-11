      SUBROUTINE HAAVAI( THAMIN, THAMAX, TBEGIN, TEND, 
     1                   TAVAIL, TUSE, NHA, IHA, SIDR )
Cf2py intent(in) THAMIN, THAMAX, TBEGIN, TEND, NHA, IHA
Cf2py intent(out) TAVAIL, TUSE
Cf2py intent(hide) SIDR
C
C     Subroutine for OPTHAS that gets the total time the source is 
C     available.
C
C     Also determine the default hour angles (TUSE) to use for even 
C     spacing of NHA scans.
C
C     Note:  HA parameters are in hours, T parameter in days.
C
C     THAMIN is the rise time MJD
C     THAMAX is the set time (start of scan that sets) MJD
C     TBEGIN is the MJD at experiment start.
C     TEND is the last MJD for the start of a scan 
C     TAVAIL is the total time the source is "up" (output)
C     TUSE is the start MJD at which to try to observe this scan.
C     NHA is the number of times the source will be observed.
C     IHA is which one of those we are dealing with.
C     SIDR is the sidereal rate (1.0027...) dUT * SIDR = dHA
C
      INTEGER           NHA, IHA
      DOUBLE PRECISION  SIDR, TBEGIN, TEND, THAMIN, THAMAX
      DOUBLE PRECISION  TUSE, TAVAIL
      DOUBLE PRECISION  TADD, TINC
C ----------------------------------------------------------------------
C     THAMIN is the last rise before the experiment end.
C     THAMAX is the first set after the experiment start.
C     There are really three distinct cases to worry about.
C        1.  Source is never up.  Trigger an abort from OPTHAS.
C        2.  Source is up once.  This includes always up.
C        3.  Source is up twice (sets, then rises)
C 
C     They are distinguished by the whether THAMIN is before or 
C     after THAMAX.  Since we prohibit runs of more than 24 hr,
C     the source cannot be up twice and not be up at both begin
C     and end.
C
      IF( THAMAX .GT. THAMIN + 1.D0 ) THEN
         TAVAIL = -1.D0
         TUSE = 0.D0
      ELSE IF( THAMAX .GE. THAMIN ) THEN
         TAVAIL = MIN( THAMAX, TEND ) - MAX( TBEGIN, THAMIN )
         TUSE = MAX( TBEGIN, THAMIN ) + ( TAVAIL / NHA ) * ( IHA - 0.5 )
      ELSE
         TAVAIL = THAMAX - TBEGIN + TEND - THAMIN
         TADD = THAMAX - TBEGIN
         TINC = TAVAIL / NHA
         TUSE = TBEGIN + TINC * ( IHA - 0.5 )
         IF( TUSE .GT. THAMAX ) THEN
            TUSE = TUSE + THAMIN - THAMAX
         END IF
      END IF
C
C     So that is a whole lot simpler than my earlier attempts to deal
C     with this in terms of hour angles which are not monotonic.  
C     However this did add some complexity to HALIM to find THAMIN
C     and THAMAX.
C
      RETURN
      END
