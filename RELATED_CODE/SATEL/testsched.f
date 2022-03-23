      PROGRAM TESTSCHED
      IMPLICIT NONE
      
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      VNORM
 
      INTEGER               STRLEN
      PARAMETER           ( STRLEN =  32 )
      INTEGER               FILEN
      PARAMETER           ( FILEN  = 128 )
C
C     Variables
C
      CHARACTER*(1)         FORMAT
      CHARACTER*(1)         CONTIN
      CHARACTER*(FILEN)     EPHEM
      CHARACTER*(FILEN)     KERNEL
      CHARACTER*(STRLEN)    ABCORR
      CHARACTER*(STRLEN)    FRAME
      CHARACTER*(STRLEN)    UTC
      CHARACTER*(STRLEN)    UTCBEG
      CHARACTER*(STRLEN)    UTCEND
      CHARACTER*(STRLEN)    OBSRVR
      CHARACTER*(STRLEN)    TARGET
      
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETBEG
      DOUBLE PRECISION      ETEND
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE  ( 6 )

      INTEGER               I
      INTEGER               MAXPTS
      INTEGER               PREC

      
      DATA                  MAXPTS   / 0 /

      SAVE
 
C
C     Get all of the required inputs.
C
C     Enter the name of a leapseconds kernel file

      KERNEL = '/home/bernoulli2/jbenson/spkfiles/naif0007.tls'
C
C     First load the leapseconds file into the kernel pool, so
C     we can convert the UTC time strings to ephemeris seconds
C     past J2000.
C
      CALL FURNSH ( KERNEL )
 
C     Enter the name of a binary SPK ephemeris file

      EPHEM = '/home/bernoulli2/jbenson/spkfiles/SDU_s04005m.mod.bsp'
C
C     Load the binary SPK file containing the ephemeris data
C     that we need.
C
      CALL FURNSH ( EPHEM )

C     Enter the name of the observing body 
      OBSRVR = 'EARTH'
      
C     Enter the name of a target body
      TARGET = 'SDU'

C     Enter the beginning UTC time
      UTCBEG = '2004 JAN 15 00:00:00 UTC'
C      UTCBEG = '2453019.5 JD'
 
C     Enter the ending UTC time
      UTCEND = '2004 JAN 15 02:00:00 UTC'
      UTCEND = '2453019.6 JD'
 
C     Enter the inertial reference frame (eg:J2000)
      FRAME = 'J2000'

C     Type of correction
      ABCORR = 'LT'

      MAXPTS = 1 
C
C     Convert the UTC time strings into DOUBLE PRECISION ETs.
C
      IF ( MAXPTS .EQ. 1 ) THEN
         CALL STR2ET ( UTCBEG, ETBEG )
      ELSE
         CALL STR2ET ( UTCBEG, ETBEG )
         CALL STR2ET ( UTCEND, ETEND )
      END IF
 
 
C
C     At each time, compute and print the state of the target body
C     as seen by the observer.  The output time will be in calendar
C     format, rounded to the nearest seconds.
C
C     DELTA is the increment between consecutive times.
C
C     Make sure that the number of points is >= 1, to avoid a
C     division by zero error.
C
      IF ( MAXPTS .GT. 1 ) THEN
         DELTA  = ( ETEND - ETBEG ) / DBLE(MAXPTS - 1)
      ELSE
         DELTA = 0.0D0
      END IF
      ET  =   ETBEG
 
C
C     Set some formatting stuff for the ET2UTC SPICELIB routine.
C
      FORMAT = 'C'
      PREC   =  0
 
C
C     Initialize the continuation flag.
C
      CONTIN = 'Y'
 
      I = 1
 
      DO WHILE ( I .LE. MAXPTS )
 
C
C        Calculate the state of the target.
C
         CALL SPKEZR ( TARGET, ET, FRAME, ABCORR, OBSRVR, STATE, LT )
 
C
C        Convert the ET (ephemeris time) into a UTC time string
C        for displaying on the screen.
C
         CALL ET2UTC ( ET, FORMAT, PREC, UTC )
 
C
C        Display the results of the state calculation.
C
         write (*,"(2X,A20,2X,E23.16,2X,E23.16,2X,E23.16)") 
     +             UTC, state(1), state(2), state(3) 
C
C        Increment the current ET by DELTA and increment the loop
C        counter.
C
         ET = ET + DLTEA
         I = I + 1
 
      END DO

      END

