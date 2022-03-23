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
      CHARACTER*20          TFORM, RAC, DECC
      CHARACTER*(FILEN)     EPHEM
      CHARACTER*(FILEN)     KERNEL
      CHARACTER*(FILEN)     BASE
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
      DOUBLE PRECISION      MJD
      DOUBLE PRECISION      R, RA, DEC, X, Y, Z, DIST, CONST
      DOUBLE PRECISION      DX, DY, DZ, VE, DRA, DDEC, VDRA, VDDEC
      DOUBLE PRECISION PI, TWOPI, RADDEG, RADHR, RADS
      PARAMETER        (PI=3.141592653589793238D0)
      PARAMETER        (TWOPI=PI*2.0D0)
      PARAMETER        (RADDEG=PI/180.0D0)
      PARAMETER        (RADHR=PI/12.0D0)
      PARAMETER        (RADS=RADDEG/3600.D0)

      INTEGER               I
      INTEGER               MAXPTS
      INTEGER               PREC
      INTEGER               LEN1
      
      DATA                  MAXPTS   / 0 /

      SAVE
 
C
C     Get all of the required inputs.
C
C     Enter the name of a leapseconds kernel file

      BASE = '/mnt/mach/mach/cwalker/code/sched/'//
     1       'RELATED_CODE/NAIF/spkfiles/'
      KERNEL = BASE(1:LEN1(BASE)) // 'naif0007.tls'
      write(*,*) KERNEL
C
C     First load the leapseconds file into the kernel pool, so
C     we can convert the UTC time strings to ephemeris seconds
C     past J2000.
C
      CALL FURNSH ( KERNEL )
 
C     Enter the name of a binary SPK ephemeris file

      EPHEM = BASE(1:LEN1(BASE)) // 'SDU_s04005m.mod.bsp'
      write(*,*) EPHEM
C
C     Load the binary SPK file containing the ephemeris data
C     that we need.
C
      CALL FURNSH ( EPHEM )

C     Enter the name of the observing body 
      OBSRVR = 'EARTH'
C      OBSRVR = '399'
      
C     Enter the name of a target body
      TARGET = 'SDU'
C      TARGET = '-29'

C     Enter the beginning UTC time

      MJD = 53019.D0
      UTCBEG = '2004 JAN 15 00:00:00 UTC'
C      WRITE( UTCBEG, '( F20.9, A )' ) MJD + 2400000.5D0, ' JD'

C     Enter the ending UTC time

      UTCEND = '2004 MAR 24 00:02:00 UTC'
C      UTCEND = '2453019.6 JD'
 
C     Enter the inertial reference frame (eg:J2000)

      FRAME = 'J2000'

C     Type of correction

      ABCORR = 'LT'

      MAXPTS = 1
C
C     Convert the UTC time strings into DOUBLE PRECISION ETs.
C     Note that STR2ET and UTC2ET give the exact same result.
C
      CALL STR2ET ( UTCBEG, ETBEG )
      write(*,'( A, A30, F20.9 )' ) ' Begin  JD, ET:  ', UTCBEG, ETBEG
      IF ( MAXPTS .GT. 1 ) THEN
         CALL STR2ET ( UTCEND, ETEND )
         write(*,'( A, A30, F20.9 )' ) ' End    JD, ET:  ', 
     1             UTCBEG, ETBEG
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
         write (*,"(2X,A20,2X,F23.3,2X,F23.3,2X,F23.3,/
     +          26X, F23.5,2X,F23.5,2X,F23.5)") 
     +             UTC, state(1), state(2), state(3),
     +             state(4), state(5), state(6)
         write (*,'(7X,F15.3,2X,F23.3)') ET, state(1)+delta*state(4)
         write(*,*)
C
C        Try to make an RA and Dec.
C
         X = STATE(1)
         Y = STATE(2)
         Z = STATE(3)
         R = SQRT(X*X+Y*Y)
         DIST = SQRT(X*X+Y*Y+Z*Z)
         IF (R.EQ.0D0) THEN
            RA = 0D0
         ELSE
            RA = ATAN2(Y,X)
         END IF
         IF( RA .LT. 0.D0 ) RA = RA + TWOPI
         IF (Z.EQ.0D0) THEN
            DEC = 0D0
         ELSE
            DEC = ATAN2(Z,R)
         END IF
C
C        Write the results in various formats.
C
         WRITE(*,*) ' Distance km, au ', DIST, DIST / 149600000.D0
         WRITE(*,*) ' Max paralax (arcsec): ', 6000.D0/(DIST*RADS)
         WRITE(*,*) ' RA, DEC radians:   ', RA, '   ', DEC
         WRITE(*,*) ' RA, DEC hr, deg:   ', 
     1         RA / RADHR, '   ', DEC / RADDEG
         RAC = TFORM( RA, 'T', 0, 2, 10, 'hms' )
         DECC = TFORM( DEC, ' ', 1, 2, 10, 'd''"' )
         WRITE(*,*) ' RA, DEC hr, deg:   ', RAC, '   ', DECC
C
C        Get the rates in the right format.
C
         DX = STATE(4)
         DY = STATE(5)
         DZ = STATE(6)
         WRITE(*,'( A, 3F10.4 )' )  ' Raw rates:', DX, DY, DZ 
         VE = SQRT(DX*DX+DY*DY)
         DRA = -1.0 * DX * SIN( RA ) + DY * COS( RA )
         DDEC = DZ * COS( DEC ) - 
     1        ( DX * COS( RA ) + DY * SIN( RA ) ) * SIN( DEC )
         WRITE(*,'( A, 3F10.4 )' )  ' VE, DRA, DDEC:', VE, DRA, DDEC
C
C        VLBI uses changes in coordinate value (time or arc sec) per day.
C        CONST converts from km/s to arcsec/day
C

         CONST = 3600.D0 * 24.D0 / ( DIST * RADS )
         IF( COS( DEC ) .NE. 0.0 ) THEN
            VDRA =  DRA * CONST / ( COS( DEC ) * 15.D0 )
         ELSE
            VDRA = 0.D0
         END IF
         VDDEC = DDEC * CONST
         WRITE(*,'( A, 2F10.4 )' )  ' VLBA DRA, DDEC:', VDRA, VDDEC
C
C        Increment the current ET by DELTA and increment the loop
C        counter.
C
         ET = ET + DELTA
         I = I + 1
 
      END DO

C
C  Use pvobs.f to get vector of station and station velocity.
C  Add planet and station vectors.
C  Use DCC2S from SLALIB to make RA,Dec.  But very simple, maybe just do it.
C  But that does not do the rates.
C
C
      END
