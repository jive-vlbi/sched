      SUBROUTINE PLOTMAP( INFILE, TYPE, NSKIP, COLOR, IMAP,
     1                    LONMAX, LONMIN, LATMIN, LATMAX, LINWID )
C
C     Routine to plot GIS vector map data on the latitude/longitude
C     plot made by the PLOTSTA routine of SCHED.  Used when 
C     OBSTYP = CONFIG (configuration studies).
C     Based on a program plote00 written by Bryan Butler in
C     Jan 2002.  Items in caps are RCW.  Lower case is BJB.
C
C     The data are in e00 files.
C
C     The file types are:
C       1:  e00 files in latitude and longitude.  Up to 4 numbers per
C           line and no label information or color.
C       2:  e00 files in meters.  A conversion to lat/long is needed.
C           Also up to 4 numbers per line.
C       3.  Converted file with one long/lat pair per line.  Segment
C           headers have ID and color, in addition to the number of
C           nodes.
C
C       The reading style for 1 and 2 is different from that for 3 so
C       separate them.
C
C     23 Oct 2002  Switch to double precision for the type 2 maps
C     on the fear that we are dealing with differences of big numbers.
C     Somehow the state boundaries are coming out shifted by about
C     1.5 arc minutes from where they are supposed to be.  I have put
C     in an empirical correction that is ok in southern NM, but is
C     not globally correct.  Some other parameters need to be changed.
C     Note that the Type 2 plot is really very specific for the states
C     file that I have, I think.
C     
      INTEGER     TYPE, IMAP
      INTEGER     MAXNPT, LINWID, LWID
      PARAMETER   ( MAXNPT = 1000 )
      REAL        LONMAX, LONMIN, LATMIN, LATMAX
      REAL        XPOINT(MAXNPT), YPOINT(MAXNPT)
      DOUBLE PRECISION  XX(2), YY(2)
      DOUBLE PRECISION  PHI0, LAM0, PHI1, PHI2
      DOUBLE PRECISION  PHI0D, LAM0D, PHI1D, PHI2D
      DOUBLE PRECISION  NN, CC, RHO0
      DOUBLE PRECISION  LAT, LON, PI, THETA, RHO, RPOLE, REQU, RMEAN
      INTEGER     NPT, DUMMY(5), NPOINT
      INTEGER     NPOLY, IX, COLOR
      INTEGER     NSKIP, I, MAXLEN, ICOLOR, IPASS
      LOGICAL     DONE, DOPLOT, ALLWEST, ALLNORTH, ALLSOUTH, ALLEAST
      CHARACTER   INFILE*(*), INLINE*130, NAME*10
      CHARACTER   MSGTXT*256
C
      DATA        PHI0D, LAM0D, PHI1D, PHI2D 
     1            / 23.0D0, -96.0D0, 29.5D0, 45.5D0 /
      DATA PI / 3.141592654D0 /
      DATA RPOLE, REQU / 6356752.314D0, 6378137.0D0 /
C ----------------------------------------------------------------------
C     Open the input file
C
      OPEN ( IMAP, FILE=INFILE, STATUS='OLD', ERR=999 )
C
C     Skip known number of extra lines at start.  Properly, these
C     could be parsed, but that would be more trouble.
C
      IF( NSKIP .GE. 1 ) THEN
         DO I = 1, NSKIP
            READ ( IMAP,'(A)') INLINE
         END DO
      END IF
C
      NPOLY = 1
      MAXLEN = 0
C
C     Treat files differently if they need coordinate conversion.
C     Some are in meters, some in lat/long.  Make lat/long.
C     Also convert the the alternative longitude with positive in US.
C
      IF( TYPE .EQ. 2 ) THEN
C
C        Get some parameters needed for the coordinate conversion.
C        Can't send character string from this routine's call 
C        arguments on to next routine without fixing the length.
C
         MSGTXT = INFILE
         CALL WLOG( 0, 'PLOTMAP:  WILL APPLY EMPIRICAL CORRECTIONS TO '
     1       // MSGTXT )
C
C
         RMEAN = SQRT ((RPOLE**2 + REQU**2) / 2)
         PHI0 = PHI0D * PI / 180
         LAM0 = LAM0D * PI / 180
         PHI1 = PHI1D * PI / 180
         PHI2 = PHI2D * PI / 180
         NN = 0.5 * (SIN (PHI1) + SIN (PHI2))
         CC = COS (PHI1) * COS (PHI1) + 2 * NN * SIN (PHI1)
         RHO0 = SQRT (CC - 2 * NN * SIN (PHI0)) / NN
C
C        Read the data.
C
         DONE = .FALSE.
         DO WHILE (.NOT. DONE)
            READ ( IMAP,*) NPT, DUMMY, NPOINT
            IF (NPT .EQ. -1) THEN
               DONE = .TRUE.
            ELSE
               IF( NPOINT .GT. MAXNPT ) THEN
                  CALL ERRLOG( 'PLOTMAP: Need bigger vector arrays.' )
               END IF
               MAXLEN = MAX( MAXLEN, NPOINT )
               DO IX = 1, NPOINT-1, 2
                  READ ( IMAP,'(4(E14.7))') XX(1), YY(1), XX(2), YY(2) 
                  XX(1) = XX(1) / RMEAN
                  YY(1) = YY(1) / RMEAN
                  XX(2) = XX(2) / RMEAN
                  YY(2) = YY(2) / RMEAN
                  THETA = ATAN2 (XX(1), RHO0 - YY(1))
                  RHO = SQRT (XX(1)**2 + (RHO0 - YY(1))**2)
                  LAT = ASIN ((CC - RHO**2 * NN**2) / (2 * NN))
                  LON = LAM0 + THETA / NN
                  XPOINT(IX) = LON * 180.D0 / PI
                  YPOINT(IX) = LAT * 180.D0 / PI
                  THETA = ATAN2 (XX(2), RHO0 - YY(2))
                  RHO = SQRT (XX(2)**2 + (RHO0 - YY(2))**2)
                  LAT = ASIN ((CC - RHO**2 * NN**2) / (2 * NN))
                  LON = LAM0 + THETA / NN
                  XPOINT(IX+1) = LON * 180.D0 / PI
                  YPOINT(IX+1) = LAT * 180.D0 / PI
               END DO
               IF (MOD(NPOINT,2) .EQ. 1) THEN
                  READ ( IMAP,'(2(E14.7))') XX(1), YY(1)
                  XX(1) = XX(1) / RMEAN
                  YY(1) = YY(1) / RMEAN
                  THETA = ATAN2 (XX(1), RHO0 - YY(1))
                  RHO = SQRT (XX(1)**2 + (RHO0 - YY(1))**2)
                  LAT = ASIN ((CC - RHO**2 * NN**2) / (2 * NN))
                  LON = LAM0 + THETA / NN
                  XPOINT(NPOINT) = LON * 180.D0 / PI
                  YPOINT(NPOINT) = LAT * 180.D0 / PI
               END IF
C
C              Plot the line, if there is a chance it is in the
C              window.  Otherwise, don't.  Note lines might cross
C              the field even without points in the field so only
C              eliminate what is completely to one side or another.
C
               ALLWEST = .TRUE.
               ALLEAST = .TRUE.
               ALLNORTH = .TRUE.
               ALLSOUTH = .TRUE.
               DO IX = 1, NPOINT
                  XPOINT(IX) = -1.0 * XPOINT(IX)
                  IF( XPOINT(IX) .LT. LONMAX ) ALLWEST = .FALSE.
                  IF( XPOINT(IX) .GT. LONMIN ) ALLEAST = .FALSE.
                  IF( YPOINT(IX) .LT. LATMAX ) ALLNORTH = .FALSE.
                  IF( YPOINT(IX) .GT. LATMIN ) ALLSOUTH = .FALSE.
C
C                 Apply the empirical corrections - measured on NM
C                 bootheel.  The type 2 map is the state boundaries.
C
                  XPOINT(IX) = XPOINT(IX) - 0.027
                  YPOINT(IX) = YPOINT(IX) + 0.023
               END DO
C
               DOPLOT = .NOT. ( ALLWEST .OR. ALLEAST .OR.
     1                  ALLNORTH .OR. ALLSOUTH )
               CALL PGSCI( COLOR )
               CALL PGSLW( LINWID )
               IF( DOPLOT ) CALL PGLINE( NPOINT, XPOINT, YPOINT )
C
               NPOLY = NPOLY + 1
            END IF
         END DO
         CLOSE( IMAP )
      ELSE IF( TYPE .EQ. 1 ) THEN
C
C        File doesn't need coordinate conversion.
C
         DONE = .FALSE.
         DO WHILE (.NOT. DONE )
C
          
C           Read the vector header.
C        
            READ ( IMAP,*,END=100) NPT, DUMMY, NPOINT
C        
C           Deal with reaching the end of file unexpectedly,
C           as we will in the roads file.
C        
            GO TO 101
  100          CONTINUE
               NPT = -1
  101       CONTINUE
C        
C           Process the vector.  There can be 1 or 2 coordinates
C           (X,Y pairs) per line.
C        
            IF (NPT .EQ. -1) THEN
               DONE = .TRUE.
            ELSE
               IF( NPOINT .GT. MAXNPT ) THEN
                  CALL ERRLOG( 'PLOTMAP: Need bigger vector arrays.' )
               END IF
               MAXLEN = MAX( MAXLEN, NPOINT )
               DO IX = 1, NPOINT-1, 2
                  READ ( IMAP,'(4(E14.7))') XX(1), YY(1), XX(2), YY(2)
                  XPOINT(IX) = XX(1)
                  YPOINT(IX) = YY(1)
                  XPOINT(IX+1) = XX(2)
                  YPOINT(IX+1) = YY(2)
               END DO
               IF (MOD(NPOINT,2) .EQ. 1) THEN
                  READ ( IMAP,'(2(E14.7))') XX(1), YY(1)
                  XPOINT(NPOINT) = XX(1)
                  YPOINT(NPOINT) = YY(1)
               END IF
C        
C              Plot the line if it is not all to one side of the window.
C        
               ALLWEST = .TRUE.
               ALLEAST = .TRUE.
               ALLNORTH = .TRUE.
               ALLSOUTH = .TRUE.
               DO IX = 1, NPOINT
                  XPOINT(IX) = -1.0 * XPOINT(IX)
                  IF( XPOINT(IX) .LT. LONMAX ) ALLWEST = .FALSE.
                  IF( XPOINT(IX) .GT. LONMIN ) ALLEAST = .FALSE.
                  IF( YPOINT(IX) .LT. LATMAX ) ALLNORTH = .FALSE.
                  IF( YPOINT(IX) .GT. LATMIN ) ALLSOUTH = .FALSE.
               END DO
               DOPLOT = .NOT. ( ALLWEST .OR. ALLEAST .OR.
     1                  ALLNORTH .OR. ALLSOUTH )
               CALL PGSLW( LINWID )
               CALL PGSCI( COLOR )
               IF( DOPLOT ) CALL PGLINE( NPOINT, XPOINT, YPOINT )
C        
C              Increment count.
C        
               NPOLY = NPOLY + 1
C
            END IF
         END DO
         CLOSE( IMAP )
         NPOLY = NPOLY - 1
C   
      ELSE IF( TYPE .EQ. 3 ) THEN
C
C        File doesn't need coordinate conversion and has ID
C        information and one node per line.
C
C        Do in 2 passes.  On the second pass, only do the
C        interstates.  Without this, local frontage roads
C        are whiting out the interstate blue.
C
         DO IPASS = 1, 2
C
C           Actually we will jump out of the loop on the read "end"
C           condition.  DONE is not used.
C
            NPOLY = 1
            DONE = .FALSE.
            DO WHILE (.NOT. DONE )
C
C              Read the vector header.
C
               READ( IMAP,*,END=200) NPOINT, ICOLOR, NAME
C
C              Process the vector.  There is 1 coordinate
C              (X,Y pair) per line.  Convert to left handed system.
C
               IF( NPOINT .GT. MAXNPT ) THEN
                  CALL ERRLOG( 'PLOTMAP: Need bigger vector arrays.' )
               END IF
               MAXLEN = MAX( MAXLEN, NPOINT )
               DO IX = 1, NPOINT
                  READ( IMAP,'(2(F12.7))') XPOINT(IX), YPOINT(IX)
                  XPOINT(IX) = -1.0 * XPOINT(IX)
               END DO
C        
C              Plot the line if it is not all to one side of the window.
C        
               ALLWEST = .TRUE.
               ALLEAST = .TRUE.
               ALLNORTH = .TRUE.
               ALLSOUTH = .TRUE.
               DO IX = 1, NPOINT
                  IF( XPOINT(IX) .LT. LONMAX ) ALLWEST = .FALSE.
                  IF( XPOINT(IX) .GT. LONMIN ) ALLEAST = .FALSE.
                  IF( YPOINT(IX) .LT. LATMAX ) ALLNORTH = .FALSE.
                  IF( YPOINT(IX) .GT. LATMIN ) ALLSOUTH = .FALSE.
               END DO
               DOPLOT = .NOT. ( ALLWEST .OR. ALLEAST .OR.
     1                  ALLNORTH .OR. ALLSOUTH )
C 
C              Plot the line.
C 
               IF( ( IPASS .EQ. 1 .OR. NAME(1:2) .EQ. 'I ' ) .AND.
     1              DOPLOT ) THEN
                  IF( NAME(1:2) .EQ. 'I ' ) THEN
                     CALL PGSLW( LINWID )
                  ELSE
                     LWID = MAX( 1.0, 0.8 * LINWID )
                     CALL PGSLW( LWID )
                  END IF
                  CALL PGSCI( ICOLOR )
                  CALL PGLINE( NPOINT, XPOINT, YPOINT )
               END IF
C 
C              Increment count.
C 
               NPOLY = NPOLY + 1
C
            END DO
C
C           Jump here when reaching the end of data.
C
  200       CONTINUE
C
            IF( IPASS .EQ. 1 ) THEN
               REWIND( IMAP )
            END IF
         END DO
         CLOSE ( IMAP )
         NPOLY = NPOLY - 1
   
      END IF
C
C     Write a bit of information to screen.
C
      WRITE( MSGTXT, '( A, I7, A, I7 )' )
     1        '    Total number of vectors = ', npoly, 
     2        ', max length = ', MAXLEN
      CALL WLOG( 0, MSGTXT )
C
C     Jump point when there is a file open problem
C
      GO TO 1000
 999     MSGTXT = INFILE
         CALL WLOG( 1, 'PLOTMAP: Unable to open file: ' //
     1       MSGTXT )
1000  CONTINUE
C
      RETURN
      END


