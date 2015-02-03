C$Procedure      CONVRT ( Convert Units )
 
      SUBROUTINE CONVRT ( X, IN, OUT, Y )
 
C$ Abstract
C
C      Take a measurement X, the units associated with
C      X, and units to which X should be converted; return Y ---
C      the value of the measurement in the output units.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     CONVERSION, UNITS
C
C$ Declarations
 
      DOUBLE PRECISION X
      CHARACTER*(*)    IN
      CHARACTER*(*)    OUT
      DOUBLE PRECISION Y
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     X          I   Number representing a measurement in some units.
C     IN         I   The units in which X is measured.
C     OUT        I   Desired units for the measurement.
C     Y          O   The measurment in the desired units.
C
C$ Detailed_Input
C
C     X          is a number representing a measurement in the units
C                specified by IN.
C
C     IN         represents the units associated with a measurement X.
C                Acceptable units are:
C
C                Angles:                 'RADIANS'
C                                        'DEGREES'
C                                        'ARCMINUTES'
C                                        'ARCSECONDS'
C                                        'HOURANGLE'
C                                        'MINUTEANGLE'
C                                        'SECONDANGLE'
C
C                Metric Distances:       'METERS'
C                                        'KM'
C                                        'CM'
C                                        'MM'
C
C                English Distances:      'FEET'
C                                        'INCHES'
C                                        'YARDS'
C                                        'STATUTE_MILES'
C                                        'NAUTICAL_MILES'
C
C                Astrometric Distances:  'AU'
C                                        'PARSECS'
C                                        'LIGHTSECS'
C                                        'LIGHTYEARS' julian lightyears
C
C                Time:                   'SECONDS'
C                                        'MINUTES'
C                                        'HOURS'
C                                        'DAYS'
C                                        'JULIAN_YEARS'
C                                        'TROPICAL_YEARS'
C                                        'YEARS' (same as julian years)
C
C     OUT        represents the units desired for the measurement X.
C                See the description of IN.
C
C$ Detailed_Output
C
C     Y          is the input measurement converted to the desired
C                units.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input units, output units, or both input and
C        output units are not recognized, the error
C        SPICE(UNITSNOTREC) is signaled.
C
C     2) If the units being converted between are incompatible, the
C        error SPICE(INCOMPATIBLEUNITS) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine converts a measurement X given in units specified by
C     IN to the equivalent value Y in units specified by OUT.
C
C     If a unit is not recognized, an error message is produced that
C     indicates which one was not recognized.
C
C     If input and output units are incompatible (for example ANGLE
C     and DISTANCE units) and error message will be produced stating
C     the requested units and associated types.
C
C$ Examples
C
C     To convert 1 meter to statute miles and feet you could
C
C        CALL CONVRT ( 1.0D0, 'METERS',        'STATUTE_MILES', MILES )
C        CALL CONVRT ( MILES, 'STATUTE_MILES', 'FEET',          FEET  )
C
C     or
C
C        CALL CONVRT ( 1.0D0, 'METERS', 'STATUTE_MILES', MILES )
C        CALL CONVRT ( 1.0D0, 'METERS', 'FEET',          FEET  )
C
C
C$ Restrictions
C
C     You should make sure that your units are appropriate for the
C     measurement. This routine does not do any checking for over-
C     flow. Something like
C
C        CALL ( 10.0D22, 'LIGHTYEARS', 'MM', Y )
C
C     will cause a floating point overflow.
C
C     Some of the units are not "defined" quantities.  In such a case
C     a best estimate is provided as of the date of the current version
C     of this routine. Those estimated quantities are:
C
C         1 AU    --- the astronomical unit. The value was taken from 
C                     the JPL ephemeris DE125. This value is an
C                     approximation and should not be used for
C                     high-accuracy work. It agrees with the value used
C                     in the JPL planetary ephemeris DE430
C                     (149597870.700 km) at the 100m level.
C
C         The tropical year is the time from equinox to equinox.  This
C         varies slightly with time.
C
C         1 PARSEC --- is dependent upon the value of the astronomical
C                      unit.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     C.A. Curzon     (JPL)
C     H.A. Neilan     (JPL)
C     W.M. Owen       (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 01-JUL-2014 (NJB)
C
C        Updated the description of the AU in the Restrictions
C        section.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WMO) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     convert units
C
C-&
 
 
 
C$ Revisions
C
C-    Beta Version 1.2.0, 05-JAN-1990 (WLT)
C
C        Data statements for double precision values were changed
C        to include a 'D' so that this routine would function properly
C        on the Univac.
C
C-   Beta Version 1.1.0, 02-MAR-1989 (HAN)
C
C        The variable LIGHTYEAR was changed to LTYEAR in order to
C        comply with the ANSI Fortran Standard six character
C        variable name length restriction.
C
C-&
 
 
 
C
C     SPICELIB functions
C
 
      LOGICAL          RETURN
      INTEGER          ISRCHC
      DOUBLE PRECISION DPR
 
C
C     Local variables
C
      CHARACTER*16     INU
      CHARACTER*16     OUTU
 
 
 
      INTEGER          I
      INTEGER          J
      DOUBLE PRECISION TEMP
 
      DOUBLE PRECISION SECPJY
      PARAMETER      ( SECPJY =  86400.0D0 * 365.25D0      )
 
      DOUBLE PRECISION SECPTY
      PARAMETER      ( SECPTY =  86400.0D0 * 365.2421988D0 )
 
      DOUBLE PRECISION LTYEAR
      PARAMETER      ( LTYEAR = 86400.0D0 * 365.25D0 * 299792458.0D0)
 
      DOUBLE PRECISION DPM
      PARAMETER      ( DPM  = 1.0D0  /   60.0D0      )
 
      DOUBLE PRECISION DPS
      PARAMETER      ( DPS  = 1.0D0  / 3600.0D0      )
 
      DOUBLE PRECISION DPSA
      PARAMETER      ( DPSA = 15.0D0 / 3600.0D0      )
 
      DOUBLE PRECISION AU
      PARAMETER      ( AU   = 1.4959787061368887D11  )
C
C     1.0d0 divided by the sin of 1 arc second
C
      DOUBLE PRECISION SCALE
      PARAMETER      ( SCALE = 2.0626480624790438D+05 )
 
 
      DOUBLE PRECISION PARSEC
      PARAMETER      ( PARSEC = AU * SCALE )
 
      INTEGER          ANG
      PARAMETER      ( ANG  = 0         )
 
      INTEGER          DIST
      PARAMETER      ( DIST = ANG  + 7  )
 
      INTEGER          TME
      PARAMETER      ( TME  = DIST + 13 )
 
      INTEGER          TOTAL
      PARAMETER      ( TOTAL = TME + 7  )
 
      CHARACTER*16     UNITS  (TOTAL)
      SAVE             UNITS
 
      DOUBLE PRECISION CNVRTN (TOTAL)
      SAVE             CNVRTN
 
      CHARACTER*8      TYPE   (TOTAL)
      SAVE             TYPE
 
      LOGICAL          FIRST
      SAVE             FIRST
 
 
 
      DATA             FIRST / .TRUE. /
 
C
C     Angular Conversions:
C
C                 (1)  Degrees/Radians
C                 (2)  Degrees/Degrees
C                 (3)  Degrees/ARCMINUTES
C                 (4)  Degrees/ARCSECONDS
C
C                 ()   Degrees/HOURANGLE
C                 ()   Degrees/MINUTEANGLE
C                 ()   Degrees/SECONDANGLE
C
      DATA UNITS  (ANG + 1)  / 'RADIANS'     /
      DATA UNITS  (ANG + 2)  / 'DEGREES'     /
      DATA UNITS  (ANG + 3)  / 'ARCMINUTES'  /
      DATA UNITS  (ANG + 4)  / 'ARCSECONDS'  /
      DATA UNITS  (ANG + 5)  / 'HOURANGLE'   /
      DATA UNITS  (ANG + 6)  / 'MINUTEANGLE' /
      DATA UNITS  (ANG + 7)  / 'SECONDANGLE' /
C
C     DATA CNVRTN (ANG + 1)  /      DPR()   /
C
C     This value will be loaded using the SPICELIB function DPR()
C     on the first execution of this routine.
C
      DATA CNVRTN (ANG + 2)  /    1.0D0   /
      DATA CNVRTN (ANG + 3)  /      DPM   /
      DATA CNVRTN (ANG + 4)  /      DPS   /
      DATA CNVRTN (ANG + 5)  / 15.000D0   /
      DATA CNVRTN (ANG + 6)  /  0.250D0   /
      DATA CNVRTN (ANG + 7)  /  DPSA      /
 
      DATA TYPE   (ANG + 1)  / 'ANGLE'    /
      DATA TYPE   (ANG + 2)  / 'ANGLE'    /
      DATA TYPE   (ANG + 3)  / 'ANGLE'    /
      DATA TYPE   (ANG + 4)  / 'ANGLE'    /
      DATA TYPE   (ANG + 5)  / 'ANGLE'    /
      DATA TYPE   (ANG + 6)  / 'ANGLE'    /
      DATA TYPE   (ANG + 7)  / 'ANGLE'    /
C
C     Distance Conversions ( 5 through 17 )
C
C                 ( 5) Meters/Meter
C                 ( 6) Meters/Km
C                 ( 7) Meters/Cm
C                 ( 8) Meters/mm
C                 ( 9) Meters/Lightsecs
C                 (10) Meters/AU
C
      DATA UNITS (DIST + 1)  / 'METERS'               /
      DATA UNITS (DIST + 2)  / 'KM'                   /
      DATA UNITS (DIST + 3)  / 'CM'                   /
      DATA UNITS (DIST + 4)  / 'MM'                   /
      DATA UNITS (DIST + 5)  / 'LIGHTSECS'            /
      DATA UNITS (DIST + 6)  / 'AU'                   /
 
      DATA CNVRTN(DIST + 1)  / 1.00D0                 /
      DATA CNVRTN(DIST + 2)  / 1000.0D0               /
      DATA CNVRTN(DIST + 3)  / 0.01D0                 /
      DATA CNVRTN(DIST + 4)  / 0.001D0                /
      DATA CNVRTN(DIST + 5)  / 299 792 458.0D0        /
      DATA CNVRTN(DIST + 6)  / AU                     /
 
      DATA TYPE  (DIST + 1)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 2)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 3)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 4)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 5)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 6)  / 'DISTANCE'             /
 
C
C     Distance Conversions
C
C                 (+ 7 ) Meters/Foot
C                 (+ 8 ) Meters/inch
C                 (+ 9 ) Meters/Statute Mile
C                 (+ 10) Meters/Nautical Mile
C                 (+ 11) Meters/Yard
C
      DATA UNITS (DIST + 7 ) / 'FEET'            /
      DATA UNITS (DIST + 8 ) / 'INCHES'          /
      DATA UNITS (DIST + 9 ) / 'STATUTE_MILES'   /
      DATA UNITS (DIST + 10) / 'NAUTICAL_MILES'  /
      DATA UNITS (DIST + 11) / 'YARDS'           /
 
      DATA CNVRTN(DIST + 7 ) / 0.3048D0          /
      DATA CNVRTN(DIST + 8 ) / 0.0254D0          /
      DATA CNVRTN(DIST + 9 ) / 1 609.344D0       /
      DATA CNVRTN(DIST + 10) / 1 852.0D0         /
      DATA CNVRTN(DIST + 11) / 0.9144D0          /
 
      DATA TYPE  (DIST + 7 ) / 'DISTANCE'        /
      DATA TYPE  (DIST + 8 ) / 'DISTANCE'        /
      DATA TYPE  (DIST + 9 ) / 'DISTANCE'        /
      DATA TYPE  (DIST + 10) / 'DISTANCE'        /
      DATA TYPE  (DIST + 11) / 'DISTANCE'        /
 
C
C     Distance Conversions
C
C                 (+ 12) Meters/LightYear
C                 (+ 13) Meters/Parsec
C
      DATA UNITS (DIST + 12) / 'LIGHTYEARS'            /
      DATA UNITS (DIST + 13) / 'PARSECS'               /
 
      DATA CNVRTN(DIST + 12) / LTYEAR                  /
      DATA CNVRTN(DIST + 13) / PARSEC                  /
 
      DATA TYPE  (DIST + 12) / 'DISTANCE'              /
      DATA TYPE  (DIST + 13) / 'DISTANCE'              /
 
C
C     Time Conversions
C
C                 (+ 1 ) seconds / second
C                 (+ 2 ) seconds / minute
C                 (+ 3 ) seconds / hour
C                 (+ 4 ) seconds / day
C                 (+ 5 ) Seconds / Julian year
C                 (+ 6 ) Seconds / Tropical year
C                 (+ 7 ) Seconds / year          --- same as Julian year
C
      DATA UNITS ( TME + 1 ) / 'SECONDS'        /
      DATA UNITS ( TME + 2 ) / 'MINUTES'        /
      DATA UNITS ( TME + 3 ) / 'HOURS'          /
      DATA UNITS ( TME + 4 ) / 'DAYS'           /
      DATA UNITS ( TME + 5 ) / 'JULIAN_YEARS'   /
      DATA UNITS ( TME + 6 ) / 'TROPICAL_YEARS' /
      DATA UNITS ( TME + 7 ) / 'YEARS'          /
 
      DATA CNVRTN( TME + 1 ) /     1.0D0        /
      DATA CNVRTN( TME + 2 ) /    60.0D0        /
      DATA CNVRTN( TME + 3 ) /  3600.0D0        /
      DATA CNVRTN( TME + 4 ) / 86400.0D0        /
      DATA CNVRTN( TME + 5 ) / SECPJY           /
      DATA CNVRTN( TME + 6 ) / SECPTY           /
      DATA CNVRTN( TME + 7 ) / SECPJY           /
 
      DATA TYPE  ( TME + 1 ) / 'TIME'           /
      DATA TYPE  ( TME + 2 ) / 'TIME'           /
      DATA TYPE  ( TME + 3 ) / 'TIME'           /
      DATA TYPE  ( TME + 4 ) / 'TIME'           /
      DATA TYPE  ( TME + 5 ) / 'TIME'           /
      DATA TYPE  ( TME + 6 ) / 'TIME'           /
      DATA TYPE  ( TME + 7 ) / 'TIME'           /
 
 
C
C     Set up the error processing.
C
      IF ( RETURN() ) RETURN
      CALL CHKIN ( 'CONVRT' )
 
 
 
      IF ( FIRST ) THEN
         CNVRTN( ANG + 1 ) =  DPR()
         FIRST             = .FALSE.
      END IF
 
 
      CALL UCASE ( IN,  INU  )
      CALL UCASE ( OUT, OUTU )
 
      I    = ISRCHC( INU,  TOTAL, UNITS )
      J    = ISRCHC( OUTU, TOTAL, UNITS )
 
      IF (      ( I .EQ. 0 )
     .     .OR. ( J. EQ. 0 ) ) THEN
 
         IF (       ( I .EQ. 0 )
     .        .AND. ( J .EQ. 0 ) ) THEN
 
 
            CALL SETMSG ('CONVRT: Neither the input units ' // INU  //
     .                   'nor the output units '            // OUTU //
     .                   'were recognized.'
     .                  )
 
            CALL SIGERR ('SPICE(UNITSNOTREC)')
 
            CALL CHKOUT ( 'CONVRT' )
            RETURN
 
         ELSE IF ( I .EQ. 0 ) THEN
 
            CALL SETMSG ('CONVRT: Input units '// INU //
     .                   ' were not recognized'
     .                  )
 
            CALL SIGERR ('SPICE(UNITSNOTREC)')
 
            CALL CHKOUT ( 'CONVRT' )
            RETURN
 
         ELSE IF ( J .EQ. 0 ) THEN
 
            CALL SETMSG ('CONVRT: Output units '// OUTU //
     .                   ' were not recognized'
     .                  )
 
            CALL SIGERR ('SPICE(UNITSNOTREC)')
 
            CALL CHKOUT ( 'CONVRT' )
            RETURN
 
         END IF
 
      END IF
 
      IF ( TYPE(I) .NE. TYPE(J) ) THEN
 
         CALL SETMSG ('CONVRT: Incompatible units. '   //
     .                'You are attempting to convert ' //
     .                INU  // 'type: ' // TYPE(I)      //
     .                ' to '                           //
     .                OUTU // 'type: ' // TYPE(J)      //
     .                '.'
     .               )
 
         CALL SIGERR ('SPICE(INCOMPATIBLEUNITS)')
 
         CALL CHKOUT ( 'CONVRT' )
         RETURN
 
      END IF
 
 
      TEMP = X    * CNVRTN ( I )
      Y    = TEMP / CNVRTN ( J )
 
 
      CALL CHKOUT ( 'CONVRT' )
      RETURN
 
      END
