      SUBROUTINE VXSUDT( ISRC, INAME )
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes the detailed source paramters,  
C     for ISRC, using INAME alias number
C     By H.J. van Langevelde, JIVE, 051001
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
      INTEGER   ISRC, INAME

      INTEGER   LEN1
      INTEGER   I
      REAL      RARATE, DECRAT
      CHARACTER TFORM*16, STRRA*16, STRDEC*16, CH1*1

      WRITE( IVEX, '( 5X, A, A, A1 )' )
     1    'source_name = ', 
     2    SOURCE(INAME,ISRC)(1:LEN1(SOURCE(INAME,ISRC))), SEP
C
C                 we have some info on cal code, write in comment
C
            WRITE( IVEX, '( A1, 4X, A, A1 )' )
     1          COM, 'this source had calibrator code: ', 
     2          CALCODE(ISRC)
C
      DO I = 1, 5
         IF( I .NE. INAME ) THEN
C
C                 source name is not sufficient, alternate source 
C                 names in comment, more than * source could appear
C
            IF( CSUSED(I,ISRC) .EQ. ' ' .OR.
     1          CSUSED(I,ISRC) .EQ. '+' .OR.
     2          CSUSED(I,ISRC) .EQ. '*' ) THEN 
               IF( SOURCE(I,ISRC) .NE. ' ' ) THEN
                  WRITE( IVEX, '( A1, 4X, A, A )' ) COM,
     1                'alternate source name: ',
     2                SOURCE(I,ISRC)(1:LEN1(SOURCE(I,ISRC)))
               END IF
            ELSE
               WRITE( MSGTXT, '( A, A1 )' ) 
     1             'VXWRSU: unclear CSUSED character: ',
     1             CSUSED(INAME,ISRC)
               CALL ERRLOG( MSGTXT )
            END IF
         END IF
      END DO
      IF( REMARK(ISRC) .NE. ' ' ) 
     1    WRITE( IVEX, '( A1, 4X, A )' ) COM, 
     2    REMARK(ISRC)(1:LEN1(REMARK(ISRC)))
C
C        Print the used coordinates, put others in comments,
C        first J2000
C
      STRRA = TFORM( RA2000(ISRC), 'T', 0, 2, 9, 'hms' ) 
      STRDEC = TFORM( D2000(ISRC),  ' ', 1, 2, 8,  'd''"' )
      CH1 = '*'
      IF( C2000(ISRC) .NE. ' ' ) CH1 = ' '
      WRITE( IVEX, '( A1, 4X, A, A, A1, 1X, A, A, A1, 1X, 
     1    A, A, A1 )' )
     2    CH1, 'ra = ', STRRA(1:LEN1(STRRA)), 
     3    SEP, 'dec = ', STRDEC(1:LEN1(STRDEC)),
     4    SEP, 'ref_coord_frame = ', 'J2000', SEP 
         
C
C        next 1950....
C
      STRRA = TFORM( RA1950(ISRC), 'T', 0, 2, 9, 'hms' ) 
      STRDEC = TFORM( D1950(ISRC),  ' ', 1, 2, 8, 'd''"' )
      CH1 = '*'
      IF( C1950(ISRC) .NE. ' ' ) CH1 = ' '
      WRITE( IVEX, '( A1, 4X, A, A, A1, 1X, A, A, A1, 1X, 
     1    A, A, A1 )' )
     2    CH1, 'ra = ', STRRA(1:LEN1(STRRA)), 
     3    SEP, 'dec = ', STRDEC(1:LEN1(STRDEC)),
     4    SEP, 'ref_coord_frame = ', 'B1950', SEP
C
C           and last coordinates of DATE
C
      STRRA = TFORM( RAP(ISRC), 'T', 0, 2, 9, 'hms' ) 
      STRDEC = TFORM( DECP(ISRC),  ' ', 1, 2, 8, 'd''"' )
      CH1 = '*'
      IF( CDATE(ISRC) .NE. ' ' ) 
     1    CALL ERRLOG('VXWRSU: coordinates of date not supported')
      WRITE( IVEX, '( A1, 4X, A, A, A1, 1X, A, A, A1, 1X, 
     1    A, A, A1 )' )
     2    CH1, 'ra = ', STRRA(1:LEN1(STRRA)), 
     3    SEP, 'dec = ', STRDEC(1:LEN1(STRDEC)),
     4    SEP, 'ref_coord_frame = ', 'Date', SEP
C
C     proper motions, not clear what units and if epoch is needed?
C     Corrected 12/12/2008 DRA-DDEC is in (arc)sec per day      
C     and should be mlutiplied by 365.25 to give 'per year'; 
C     not divided 
      IF( DRA(ISRC) .NE. 0.0 .OR. 
     1    DDEC(ISRC) .NE. 0.0  ) THEN
         RARATE = 15 * DRA(ISRC) * 365.25
         DECRAT = DDEC(ISRC) * 365.25
         WRITE( IVEX, '( 5X, A, G8.2, 1X, A, A1, 
     1       1X, A, G8.2, 1X, A, A1 )' ) 'ra_rate = ', RARATE,
     2       'asec/yr', SEP, 'dec_rate = ', DECRAT,
     3       'asec/yr', SEP
      ENDIF
C
C     and vlsr when needed, not actually used, but others may need it
C
      IF( VLSR(1,ISRC) .GT. -1.E+8 ) THEN
         IF( VELREF(ISRC) .EQ. 'G' ) THEN 
            WRITE( IVEX, '( A1, 4X, A, F10.2, 1X, A,
     1          A1, 1X, A1, A )' )
     2          COM, 'velocity_wrt_Geo = ', VLSR(1,ISRC), 
     3          'km/sec', SEP, COM,
     4          ' warning, only given here for 1st channel'
         END IF
         IF( VELREF(ISRC) .EQ. 'H' ) THEN 
            WRITE( IVEX, '( A1, 4X, A, F10.2, 1X, A, 
     1          A1, 1X, A1, A )' )
     2          COM, 'velocity_wrt_Helio = ', VLSR(1,ISRC), 
     3          'km/sec', SEP, COM,
     2          ' warning, only given here for 1st channel'
         END IF
         IF( VELREF(ISRC) .EQ. 'L' ) THEN 
            WRITE( IVEX, '( A1, 4X, A, F10.2, 1X, A, 
     1          A1, 1X, A1, A )' )
     2          COM, 'velocity_wrt_LSR = ', VLSR(1,ISRC), 
     3          'km/sec', SEP, COM,
     2          ' warning, only given here for 1st channel'
         END IF
         IF( VELDEF(ISRC) .EQ. 'R' ) THEN 
            WRITE( IVEX, '( A1, 4X, A )' )
     2          COM, 
     2          'calculations used radio definition '
         END IF
         IF( VELDEF(ISRC) .EQ. 'O' ) THEN 
            WRITE( IVEX, '( A1, 4X, A )' )
     2          COM, 
     2          'calculations used optical definition '
         END IF
         IF( VELDEF(ISRC) .EQ. 'Z' ) THEN 
            WRITE( IVEX, '( A1, 4X, A )' )
     2          COM, 
     2          'calculations used redshift '
         END IF
      END IF

      RETURN
      END
