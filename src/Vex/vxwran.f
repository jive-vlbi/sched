      SUBROUTINE VXWRAN
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the AN = $ANTENNA section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
C     Huib's local variables 
C      
      INTEGER   IXX, ISTA, ISCAT, I
      INTEGER   LEN1
C ----------------------------------------------------------------------
C
C     Antenna section
C
      WRITE( IVEX, '( A, A1 )' ) '$ANTENNA', SEP
      DO IXX = 1, NANVEX
C
C        find a station to which this refers
C
         ISTA = -1
         DO I = 1, NSTA
            IF( ISTAAN(I) .EQ. IXX ) ISTA = I
         END DO
C
         IF( ISTA .LT. 0 ) 
     1        CALL ERRLOG(' VXWRAN: no station for $ANTENNA def ')
C
         ISCAT = STANUM(ISTA)
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1       ANLINK(IXX)(1:LEN1(ANLINK(IXX))),SEP
C
         IF( MOUNT(ISCAT) .EQ. 'ALTAZ' ) THEN
            WRITE( IVEX, '( 5X, A, A, 1X, A1, 1X, A, A1 )' )
     1          'axis_type = ', 'az', COL, 'el', SEP
C
C           assuming axrate is elev first. It's in deg/min
C
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F6.1, 1X, A, 1X, A1,
     1           I3, 1X, A, A1, 2X, A1, 1X, F8.3, 1X, A )' )
     2           'antenna_motion = ', 'el', COL, AX2RATE(ISCAT), 
     3           'deg/min', COL, NINT(TSETTLE(ISCAT)), 'sec', SEP, COM,
     4           AX2ACC(ISCAT), 'deg/sec/sec'
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F6.1, 1X, A, 1X, A1, 
     1           I3, 1X, A, A1, 2X, A1, 1X, F8.3, 1X, A )' )
     2           'antenna_motion = ', 'az', COL, AX1RATE(ISCAT), 
     3           'deg/min', COL, NINT(TSETTLE(ISCAT)), 'sec', SEP, COM,
     4           AX1ACC(ISCAT), 'deg/sec/sec'
         ELSE IF( MOUNT(ISCAT) .EQ. 'EQUAT' ) THEN
            WRITE( IVEX, '( 5X, A, A, 1X, A1, 1X, A, A1 )' )
     1           'axis_type = ', 'ha', COL, 'dec', SEP
C
C           assuming axrate is dec first. In deg/min...
C
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F6.1, 1X, A, 1X, A1, 
     1           I3, 1X, A, A1, 2X, A1, 1X, F8.3, 1X, A )' )
     2           'antenna_motion = ', 'dec', COL, AX2RATE(ISCAT), 
     3           'deg/min', COL, NINT(TSETTLE(ISCAT)), 'sec', SEP, COM,
     4           AX2ACC(ISCAT), 'deg/sec/sec'
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F6.1, 1X, A, 1X, A1, 
     1           I3, 1X, A, A1, 2X, A1, 1X, F8.3, 1X, A )' )
     2           'antenna_motion = ', 'ha', COL, AX1RATE(ISCAT), 
     3           'deg/min', COL, NINT(TSETTLE(ISCAT)), 'sec', SEP, COM,
     4           AX1ACC(ISCAT), 'deg/sec/sec'
         ELSE IF( MOUNT(ISCAT) .EQ. 'XYNS' ) THEN
            WRITE( IVEX, '( 5X, A, A, 1X, A1, 1X, A, A1 )' )
     1           'axis_type = ', 'x', COL, 'yns', SEP
C
C           assuming axrate is x first. In deg/min...
C
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F6.1, 1X, A, 1X, A1, 
     1           I3, 1X, A, A1, 2X, A1, 1X, F8.3, 1X, A )' )
     2           'antenna_motion = ', 'x', COL, AX1RATE(ISCAT), 
     3           'deg/min', COL, NINT(TSETTLE(ISCAT)), 'sec', SEP, COM,
     4           AX1ACC(ISCAT), 'deg/sec/sec'
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F6.1, 1X, A, 1X, A1, 
     1           I3, 1X, A, A1, 2X, A1, 1X, F8.3, 1X, A )' )
     2           'antenna_motion = ', 'y', COL, AX2RATE(ISCAT), 
     3           'deg/min', COL, NINT(TSETTLE(ISCAT)), 'sec', SEP, COM,
     4           AX2ACC(ISCAT), 'deg/sec/sec'
         ELSE IF( MOUNT(ISCAT) .EQ. 'XYEW' ) THEN
            WRITE( IVEX, '( 5X, A, A, 1X, A1, 1X, A, A1 )' )
     1           'axis_type = ', 'x', COL, 'yew', SEP
C
C           assuming axrate is x first. In deg/min...
C
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F6.1, 1X, A, 1X, A1, 
     1           I3, 1X, A, A1, 2X, A1, 1X, F8.3, 1X, A )' )
     2           'antenna_motion = ', 'x', COL, AX1RATE(ISCAT), 
     3           'deg/min', COL, NINT(TSETTLE(ISCAT)), 'sec', SEP, COM,
     4           AX1ACC(ISCAT), 'deg/sec/sec'
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F6.1, 1X, A, 1X, A1, 
     1           I3, 1X, A, A1, 2X, A1, 1X, F8.3, 1X, A )' )
     2           'antenna_motion = ', 'y', COL, AX2RATE(ISCAT), 
     3           'deg/min', COL, NINT(TSETTLE(ISCAT)), 'sec', SEP, COM,
     4           AX2ACC(ISCAT), 'deg/sec/sec'
         ELSE
            WRITE( IVEX, '( 5X, A, A, 1X, A1, 1X, A, A1 )' )
     1           'axis_type = ', 'un-', COL, 'known', SEP
            CALL WLOG( 1,' VXWRAN: unknown antenna mount: '//
     1           MOUNT(ISCAT) )
         ENDIF
C
C           axis offset now available
C
         WRITE( IVEX, '( 5X, A, F10.5, 1X, A, A1 )' ) 
     1       'axis_offset = ',AXOFF(ISCAT),'m', SEP         
C
C        no cable wraps etc
C            
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP  
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END
