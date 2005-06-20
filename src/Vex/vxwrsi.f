      SUBROUTINE VXWRSI
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the SI = $SITE section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
C     Huib's local variables 
C      
      INTEGER   IXX, NLINES, I, J, ISTA, ISCAT
      INTEGER   LEN1
      CHARACTER LINE*132, TFORM*15, CHRLON*11, CHRLAT*11, DEL*1
      CHARACTER TMPSTA*32
      LOGICAL   VIOLFS
C ----------------------------------------------------------------------
C
      VIOLFS = .FALSE.
      LINE = ' ' 
C
C     Site section
C
      WRITE( IVEX, '( A, A1 )' ) '$SITE', SEP
      DO IXX = 1, NSIVEX
C
C        find a station to which this refers
C
         ISTA = -1
         DO I = 1, NSTA
            IF( ISTASI(I) .EQ. IXX ) ISTA = I
         END DO
C
         IF( ISTA .LT. 0 ) 
     1        CALL ERRLOG(' VXWRSI: no station for $SITE def ')
C
         ISCAT = STANUM(ISTA)
C
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        SILINK(IXX)(1:LEN1(SILINK(IXX))),SEP       
C
C        first the simple things
C         
         WRITE( IVEX, '( 5X, A, A, A1 )' ) 'site_type = ','fixed',SEP
         TMPSTA = STATION(ISCAT)
         CALL VXSTKY(TMPSTA,.FALSE.)
         WRITE( IVEX, '( 5X, A, A, A1 )' ) 'site_name = ',
     1        TMPSTA(1:LEN1(TMPSTA)), SEP
         WRITE( IVEX, '( 5X, A, A, A1 )' ) 'site_ID = ',
     1        STCODE(ISCAT)(1:LEN1(STCODE(ISCAT))), SEP 
C
C        Position, calculated from ELEV, LONG, LAT or stored
C 
         CHRLON = TFORM( LONG(ISCAT), ' ', 1, 3, 4, ':::' )
         CHRLAT = TFORM( LAT(ISCAT), ' ', 1, 2, 4, ':::' )
         WRITE( IVEX, '( A1, 4X, A, F8.2, 1X, A, A, 1X, A, A )' )
     1        COM, 'elev=', ELEV(ISCAT), 
     2        'long=', CHRLON ,
     2        'lat=', CHRLAT 
C
C        x,y,z next:
C
         WRITE( IVEX, '( 5X, A, F12.3, 1X, A, A1, F12.3, 1X, A, A1,
     1        F12.3, 1X, A, A1 )' )
     2        'site_position =', XPOS(ISCAT), 'm', COL, YPOS(ISCAT),
     3        'm', COL, ZPOS(ISCAT), 'm', SEP
C
C        No info on site_velocity, zen_atmos or ocean load,
C        so next write horizon, fill lines to 80 columns
C
         IF( NHORIZ(ISCAT) .GT. 1 ) THEN
            IF (NHORIZ(ISCAT) .GT. 60 ) THEN
               VIOLFS = .TRUE.
               WRITE( MSGTXT, '( A, A, A, I3, A )' ) 
     1             'VXWRSI: WARNING: More than 60 points on ',
     2             STATION(ISCAT), ' horizon! (', NHORIZ(ISCAT),
     3             ')'
               CALL WLOG( 1, MSGTXT )
            END IF
C
C           there is a horizon, how many lines needed for 8 cols?
C
            NLINES = ((NHORIZ(ISCAT)-1) / 8) + 1
            IF( MOD( (NHORIZ(ISCAT)-1), 8) .EQ. 0 ) NLINES = NLINES -1
C
C           make robust for length of line
C
            LINE =  ' '
C
C           first line is always the same, write the first point with deg
C
            WRITE( LINE(1:31), '( 5X, A16, F5.1, 1X, A3, A1 )' )
     1           'horizon_map_az =', HORAZ(1,ISCAT), 'deg', COL
            DO J = 1, NLINES
               IF( J .NE. 1 ) WRITE( LINE(1:31), '( 31X )')
               DO I= 1, MIN(8, NHORIZ(ISCAT)-((J-1)*8+1))
C
C                 set delimiter 
C
                  DEL = COL
                  IF( (J-1)*8+1+I .EQ. NHORIZ(ISCAT) ) DEL = SEP
C
C                 write rest of line
C
                  WRITE( LINE(32+(I-1)*6:31+I*6), '( F5.1, A1 )' )
     1                 HORAZ((J-1)*8+I+1,ISCAT), DEL
               END DO
C
C              flush line and reset
C
               WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
               LINE =  ' '
            END DO
C
C           elevation array in exactly the same way:
C
            LINE =  ' '
            WRITE( LINE(1:31), '( 5X, A16, F5.1, 1X, A, A1 )' )
     1           'horizon_map_el =', HOREL(1,ISCAT), 'deg', COL
            DO J = 1, NLINES
               IF( J .NE. 1 ) WRITE( LINE(1:31), '( 31X )')
               DO I= 1, MIN(8, NHORIZ(ISCAT)-((J-1)*8+1))
                  DEL = COL
                  IF( (J-1)*8+I+1 .EQ. NHORIZ(ISCAT) ) DEL = SEP
C
                  WRITE( LINE(32+(I-1)*6:31+I*6), '( F5.1, A1 )' )
     1                 HOREL((J-1)*8+I+1,ISCAT), DEL
               END DO
C
               WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
               LINE =  ' '
            END DO
C
         END IF
C
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP  
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
C
      IF( VIOLFS )
     1    CALL WLOG( 1,'VXWRSI: More than 60 points on horizon '//
     2    'mask, not supported in PCFS; this VEX will NOT run!!!!')
C
      RETURN
      END
