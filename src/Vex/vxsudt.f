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
C      REAL      RARATE, DECRAT
      CHARACTER TFORM*17, STRRA*17, STRDEC*17, CH1*1

      WRITE( IVEX, '( 5X, A, A, A1 )' )
     1    'source_name = ', 
     2    SOURCE(INAME,ISRC)(1:LEN1(SOURCE(INAME,ISRC))), SEP
C
C                 we have some info on cal code, write in comment
C
      WRITE( IVEX, '( A1, 4X, A, A1 )' )
     1     COM, 'this source had calibrator code: ', 
     2     CALCODE(ISRC)
C
      IF( PLANET(ISRC) ) THEN
         WRITE( IVEX, '( A1, 4X, A, A )' )
     1     COM, 'This source is a planet.  ',
     2     'The motion is not handled in VEX.'
         WRITE( IVEX, '( A1, 4X, A )' )
     1     COM, 'Do not use the position for pointing or correlation.'
      END IF

      IF( SATEL(ISRC) ) THEN

         WRITE( IVEX, '( A1, 4X, A / A1, 4X, A)' )
     1     COM, 'This source is a satellite; using NRAO extension:  ',
     2     COM, 'Do not use the position for pointing or correlation.'

C        Grab the satellite number and output the name of the TLE
C        file and the satellite number.  The satini routine checks
C        that exactly one of satfile/tlefile contains 'NONE'.

         I = SATN (ISRC)
         IF( SATFILE(I)(1:4) .NE. 'NONE' ) THEN
            WRITE( IVEX, '( 5x, "source_type = ", A, " : ", I8, ";" )')
     1         SATFILE (I)(1:LEN1(SATFILE (I))),
     2         SATNUM (I)
         ELSE
            WRITE( IVEX, '( 5x, "source_type = ", A, " : ", I8, ";" )')
     1         TLEFILE (I)(1:LEN1(TLEFILE (I))),
     2         SATNUM (I)
         END IF
      END IF

C
      DO I = 1, MALIAS
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
     1             'VXWRDT: unclear CSUSED character: ',
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
C        Dec. 12, 2008  RCW - changed to leave J2000 coordinate
C        uncommented.
C
      STRRA = TFORM( RA2000(ISRC), 'T', 0, 2, 10, 'hms' ) 
      STRDEC = TFORM( D2000(ISRC),  ' ', 1, 2, 9,  'd''"' )
      CH1 = ' '
C      IF( C2000(ISRC) .NE. ' ' ) CH1 = ' '
      WRITE( IVEX, '( A1, 4X, A, A, A1, 1X, A, A, A1, 1X, 
     1    A, A, A1 )' )
     2    CH1, 'ra = ', STRRA(1:LEN1(STRRA)), 
     3    SEP, 'dec = ', STRDEC(1:LEN1(STRDEC)),
     4    SEP, 'ref_coord_frame = ', 'J2000', SEP 
         
C
C        next 1950....
C
      STRRA = TFORM( RA1950(ISRC), 'T', 0, 2, 10, 'hms' ) 
      STRDEC = TFORM( D1950(ISRC),  ' ', 1, 2, 9, 'd''"' )
      CH1 = '*'
C       IF( C1950(ISRC) .NE. ' ' ) CH1 = ' '
      WRITE( IVEX, '( A1, 4X, A, A, A1, 1X, A, A, A1, 1X, 
     1    A, A, A1 )' )
     2    CH1, 'ra = ', STRRA(1:LEN1(STRRA)), 
     3    SEP, 'dec = ', STRDEC(1:LEN1(STRDEC)),
     4    SEP, 'ref_coord_frame = ', 'B1950', SEP
C
C           and last coordinates of DATE
C
C           June 19, 2009. RCW  Allow coordinates of date from user.
C           J2000 version will be used internally and in the Vex file.
C
      STRRA = TFORM( RAP(ISRC), 'T', 0, 2, 10, 'hms' ) 
      STRDEC = TFORM( DECP(ISRC),  ' ', 1, 2, 9, 'd''"' )
      CH1 = '*'
C      IF( CDATE(ISRC) .NE. ' ' ) 
C     1    CALL ERRLOG('VXWRDT: coordinates of date not supported')
      WRITE( IVEX, '( A1, 4X, A, A, A1, 1X, A, A, A1, 1X, 
     1    A, A, A1 )' )
     2    CH1, 'ra = ', STRRA(1:LEN1(STRRA)), 
     3    SEP, 'dec = ', STRDEC(1:LEN1(STRDEC)),
     4    SEP, 'ref_coord_frame = ', 'Date', SEP
C
C     Worked on this Dec. 13, 2008.  RCW
C     SCHED shifts the positions of sources subject to proper motion
C     to close to the time of the observation (default is stop time 
C     of the first scan).  This is because the precision available
C     at some antennas (eg VLA) for rates is not adequate to 
C     describe proper motions over years.  But SCHED can also 
C     handle planetary motions (seconds per day) which are typically
C     much faster than proper motions (arcsec/yr).  When shifting
C     a source position to a current epoch, SCHED also puts the
C     rates into the planetary motion terms even though they 
C     will typically be small.
C
C     DRA and DDEC are the planetary motions.  The units are s/day 
C     and arcsec/dsy (coordinate change per day).  I would prefer
C     to use these units, but the Vex definition seem to want
C     rates more apropriate for proper motion - arcsec/yr.  I
C     fear users may also only update the coordinate once rather
C     than tracking the position.
C
C     Thinking about this, I don't want do activate it until we
C     have a better understanding all around about what should
C     be done.  Don't panic for values too low to matter (like
C     actual stellar proper motions), but, for now, refuse to
C     deal with things moving fast enough to go more than 10
C     arcsec in a day, and warn about correlation if it is 
C     going more than 0.1 mas/day.
C     
      IF( DRA(ISRC) .GE. 0.0001/15.0 .OR. 
     1    DDEC(ISRC) .GE. 0.0001  ) THEN
         CALL WLOG( 1, '++++ VXSUDT: WARNING Planetary motion'//
     1      ' over 0.01 mas/day used.' )
         CALL WLOG( 1, '             VEX file does not describe it.' )
         CALL WLOG( 1, '             Do not use for correlation.' )
         IF( DRA(ISRC) .GE. 10.0/15.0 .OR. 
     1       DDEC(ISRC) .GE. 10.0  ) THEN
            CALL WLOG( 1, 
     1          '++++ VXSUDT: Planetary motion over 10 arcsec/day'//
     2          ' specified for source ' )
            CALL WLOG( 1, 
     1          '             ' // 
     2          SOURCE(INAME,ISRC)(1:LEN1(SOURCE(INAME,ISRC))) )
            CALL WLOG( 1, 
     1          '             Planetary motion is not implemented' //
     2          ' in VEX output, but this is too large for pointing')
            CALL WLOG( 1,
     1          '             for more than a short time.' )
            CALL WLOG( 1,
     1          '             Be sure to update position adequately' //
     2          ' often.' )
         END IF
C
C         The following is what was there after the last update.
C         It has a few problems.  DRA is a change of coordinate
C         value per day.  To get to arcsec, it would need to be
C         divided by cos(dec) in addition to the multiply by 15
C         to get from time to arc units.  Also the epoch is 
C         required to do anything useful with the rates.
C
C         RARATE = 15 * DRA(ISRC) * 365.25
C         DECRAT = DDEC(ISRC) * 365.25
C         WRITE( IVEX, '( 5X, A, G7.2, 1X, A, A1, 
C     1       1X, A, G7.2, 1X, A, A1 )' ) 'ra_rate = ', RARATE,
C     2       'asec/yr', SEP, 'dec_rate = ', DECRAT,
C     3       'asec/yr', SEP
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
