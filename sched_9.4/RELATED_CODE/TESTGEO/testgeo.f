      PROGRAM TESTGEO
C
C     Test routines for conversion from geodetic to geocentric and back.
C
      INTEGER           I, ND, IER
      DOUBLE PRECISION  AU
      PARAMETER         (ND=6)
      PARAMETER (AU=1.49597870D11)
      DOUBLE PRECISION  DLAT(ND), ELEV(ND), RAD, LONG, LAT, EL
      DOUBLE PRECISION  DEGRAD, PI, DLATR, CLATR, EQUATR, Z, X, Y
      DOUBLE PRECISION  LONGR, NLONG, NLAT, NEL
      CHARACTER         PDLAT*17, PCLAT*17, TFORM*17, PLONG*17
      DATA   DLAT  / 0.D0, 30.D0, 45.D0, 45.D0, 60.D0, 90.D0 /
      DATA   ELEV  / 0.D0,  0.D0,  0.D0,  1.D4,  0.D0,  0.D0 /
C -------------------------------------------------------------------  
      PI = DATAN( 1.D0 ) * 4.D0
      DEGRAD = 180.D0 / PI
C
C     Test sla_geoc.
C
      WRITE(*,*) 'Testing sla_geoc'
      DO I = 1, ND
         DLATR = DLAT(I) / DEGRAD
         PDLAT = TFORM( DLATR, ' ', 1, 3, 8, ':: ' )
         CALL SLA_GEOC( DLATR, ELEV(I), EQUATR, Z )
         CLATR = ATAN2( Z, EQUATR )
         RAD = SQRT( EQUATR**2 + Z**2 )
         Z = Z * AU
         EQUATR = EQUATR * AU
         RAD = RAD * AU
         PCLAT = TFORM( CLATR, ' ', 1, 3, 8, ':: ' )
         WRITE(*,*) PDLAT, ' ', PCLAT, ' ', ELEV(I), RAD
      END DO
C
C     Test geoid.
C
      WRITE(*,*) 'Testing geoid'
      DO I = 1, ND
         RAD = ELEV(I)
         DLATR = DLAT(I) / DEGRAD
         PDLAT = TFORM( DLATR, ' ', 1, 3, 8, ':: ' )
         CALL GEOID( RAD, DLATR )
         CLATR = DLATR
         PCLAT = TFORM( CLATR, ' ', 1, 3, 8, ':: ' )
         WRITE(*,*) PDLAT, ' ', PCLAT, ' ', ELEV(I), RAD
      END DO
C
C     Try converting Pie Town.
C
      LONG = 108.D0 + 7.D0/60.D0 + 7.24D0/3600.D0
      LAT = 34.D0 + 18.D0/60.D0 + 3.61D0/3600.D0
      LONGR = LONG / DEGRAD
      DLATR = LAT / DEGRAD
      PDLAT = TFORM( DLATR, ' ', 1, 3, 8, ':: ' )
      PLONG = TFORM( LONGR, ' ', 1, 3, 8, ':: ' )
      EL = 2371.0
      WRITE(*,*) 'Pie Town:', PDLAT, PLONG, EL
C
C     Convert with GEOXYZ
C
      NLAT = DLATR
      NLONG = LONGR
      NEL = EL
      CALL GEOXYZ( 0, NLONG, NLAT, NEL, X, Y, Z, IER ) 
      WRITE(*,*) 'geoxyz: ', X, Y, Z, IER
      PDLAT = TFORM( DLATR, ' ', 1, 3, 8, ':: ' )
      PLONG = TFORM( LONGR, ' ', 1, 3, 8, ':: ' )
      WRITE(*,*) 'chgs?:  ', PDLAT, PLONG, NEL
C
C     Go back with GEOXYZ
C
      CALL GEOXYZ( 1, NLONG, NLAT, NEL, X, Y, Z, IER )
      PDLAT = TFORM( NLAT, ' ', 1, 3, 8, ':: ' )
      PLONG = TFORM( NLONG, ' ', 1, 3, 8, ':: ' )
      WRITE(*,*) ' back:  ', PDLAT, PLONG, NEL, IER
C
C     Convert with SLA routine.
C
      CALL SLA_GEOC( DLATR, EL, EQUATR, Z )
      Z = Z * AU
      EQUATR = EQUATR * AU
      X = EQUATR * COS( LONG / DEGRAD )
      Y = -1.D0 * EQUATR * SIN( LONG / DEGRAD )
      WRITE(*,*) 'sla:   ', X, Y, Z
C
C     Convert with GEOID
C
      DLATR = LAT / DEGRAD
      RAD = EL
      CALL GEOID( RAD, DLATR )
      Z = RAD * SIN( DLATR )
      EQUATR = RAD * COS( DLATR )
      X = EQUATR * COS( LONG / DEGRAD )
      Y = -1.D0 * EQUATR * SIN( LONG / DEGRAD )
      WRITE(*,*) 'geoid: ', X, Y, Z
C
      STOP
      END
