C$Procedure ET2LST ( ET to Local Solar Time )
 
      SUBROUTINE ET2LST ( ET, BODY, LONG, TYPE, HR, MN, SC, TIME, AMPM )
 
C$ Abstract
C
C     Given an ephemeris epoch ET, compute the local solar time for
C     an object on the surface of a body at a specified longitude.
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
C     TIME
C
C$ Keywords
C
C     TIME
C
C$ Declarations
 
      IMPLICIT NONE

      DOUBLE PRECISION      ET
      INTEGER               BODY
      DOUBLE PRECISION      LONG
      CHARACTER*(*)         TYPE
      INTEGER               HR
      INTEGER               MN
      INTEGER               SC
      CHARACTER*(*)         TIME
      CHARACTER*(*)         AMPM
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch in seconds past J2000 epoch
C     BODY       I   ID-code of the body of interest
C     LONG       I   Longitude of surface point (RADIANS)
C     TYPE       I   Type of longitude 'PLANETOCENTRIC', etc.
C     HR         O   Local hour on a "24 hour" clock
C     MN         O   Minutes past the hour
C     SC         O   Seconds past the minute
C     TIME       O   String giving local time on 24 hour clock
C     AMPM       O   String giving time on A.M./ P.M. scale
C
C$ Detailed_Input
C
C     ET         is the epoch expressed in TDB seconds past
C                the J2000 epoch at which a local time is desired.
C
C     BODY       is the NAIF ID-code of a body on which local
C                time is to be measured.
C
C     LONG       is the longitude (either planetocentric or
C                planetographic) in radians of the site on the
C                surface of body for which local time should be
C                computed.
C
C     TYPE       is the form of longitude supplied by the variable
C                LONG.  Allowed values are 'PLANETOCENTRIC' and
C                'PLANETOGRAPHIC'.  Note the case of the letters
C                in TYPE is insignificant.  Both 'PLANETOCENTRIC'
C                and 'planetocentric' are recognized.
C
C$ Detailed_Output
C
C     HR         is the local "hour" of the site specified at the
C                epoch ET. Note that an "hour" of local time does not
C                have the same duration as an hour measured by
C                conventional clocks.  It is simply a representation
C                of an angle. See the "Particulars" section for a more
C                complete discussion of the meaning of local time.
C
C     MN         is the number of "minutes" past the hour of the
C                local time of the site at the epoch ET. Again note
C                that a "local minute" is not the same as a minute
C                you would measure with conventional clocks.
C
C     SC         is the number of "seconds" past the minute of the
C                local time of the site at the epoch ET.  Again note
C                that a "local second" is not the same as a second
C                you would measure with conventional clocks.
C
C     TIME       is a string expressing the local time
C                on a "24 hour" local clock.
C
C     AMPM       is a string expressing the local time on a "12 hour"
C                local clock together with the traditional AM/PM
C                label to indicate whether the sun has crossed
C                the local zenith meridian.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) This routine defines local solar time for any point on the
C        surface of the Sun to be 12:00:00 noon.
C
C     2) If the TYPE of the coordinates is not recognized, the
C        error 'SPICE(UNKNOWNSYSTEM)' will be signaled.
C
C     3) If the body-fixed frame to associate with BODY cannot be
C        determined, the error 'SPICE(CANTFINDFRAME)' is signaled.
C
C     4) If insufficient data is available to compute the
C        location of the sun in body-fixed coordinates, the
C        error will be diagnosed by a routine called by this one.
C
C     5) If the BODY#_PM keyword required to determine the body
C        rotation sense is not found in the POOL or if it is found but
C        is not a numeric keyword with at least two elements, the error
C        'SPICE(CANTGETROTATIONTYPE)' is signaled.
C
C$ Files
C
C     Suitable SPK and PCK files must be loaded prior to calling this
C     routine so that the body-fixed position of the sun relative to
C     BODY can be computed. The PCK files must contain the standard
C     BODY#_PM keyword need by this routine to determine the body
C     rotation sense.
C
C     When the input longitude is planetographic, the default 
C     interpretation of this value can be overridden using the optional
C     kernel variable
C
C        BODY<body ID>_PGR_POSITIVE_LON
C
C     which is normally defined via loading a text kernel. 
C
C$ Particulars
C
C     This routine returns the local solar time at a user
C     specified location on a user specified body.
C
C     Let SUNLNG be the planetocentric longitude (in degrees) of
C     the sun as viewed from the center of the body of interest.
C
C     Let SITLNG be the planetocentric longitude (in degrees) of
C     the site for which local time is desired.
C
C     We define local time to be 12 + (SITLNG - SUNLNG)/15
C
C     (where appropriate care is taken to map ( SITLNG - SUNLNG )
C     into the range from -180 to 180).
C
C     Using this definition, we see that from the point of view
C     of this routine, local solar time is simply a measure of angles
C     between meridians on the surface of a body.  Consequently,
C     this routine is not appropriate for computing "local times"
C     in the sense of Pacific Standard Time.   For computing times
C     relative to standard time zones on earth, see the routines
C     TIMOUT and STR2ET.
C
C
C     Regarding planetographic longitude
C     ----------------------------------
C
C     In the planetographic coordinate system, longitude is defined
C     using the spin sense of the body.  Longitude is positive to the
C     west if the spin is prograde and positive to the east if the spin
C     is retrograde.  The spin sense is given by the sign of the first
C     degree term of the time-dependent polynomial for the body's prime
C     meridian Euler angle "W":  the spin is retrograde if this term is
C     negative and prograde otherwise.  For the sun, planets, most
C     natural satellites, and selected asteroids, the polynomial
C     expression for W may be found in a SPICE PCK kernel.
C
C     The earth, moon, and sun are exceptions: planetographic longitude
C     is measured positive east for these bodies.
C
C     If you wish to override the default sense of positive
C     planetographic longitude for a particular body, you can do so by
C     defining the kernel variable
C
C        BODY<body ID>_PGR_POSITIVE_LON
C
C     where <body ID> represents the NAIF ID code of the body. This
C     variable may be assigned either of the values
C
C        'WEST'
C        'EAST'
C
C     For example, you can have this routine treat the longitude
C     of the earth as increasing to the west using the kernel
C     variable assignment
C
C        BODY399_PGR_POSITIVE_LON = 'WEST'
C        
C     Normally such assignments are made by placing them in a text
C     kernel and loading that kernel via FURNSH.
C
C
C$ Examples
C
C     The following code fragment illustrates how you
C     could print the local time at a site on Mars with
C     planetographic longitude 326.17 deg E at epoch ET.
C
C     (This example assumes all required SPK and PCK files have
C     been loaded).
C
C     Convert the longitude to radians, set the type of the longitude
C     and make up a mnemonic for Mars' ID-code.
C
C     LONG = 326.17 * RPD()
C     TYPE = 'PLANETOGRAPHIC'
C     MARS = 499
C
C     CALL ET2LST ( ET, MARS, LONG, TYPE, HR, MN, SC, TIME, AMPM )
C
C     WRITE (*,*) 'The local time at Mars 326.17 degrees E '
C     WRITE (*,*) 'planetographic longitude is: ', AMPM
C
C$ Restrictions
C
C     This routine relies on being able to determine the name
C     of the body-fixed frame associated with BODY through the
C     frames subsystem.  If the BODY specified is NOT one of the
C     nine planets or their satellites, you will need to load
C     an appropriate frame definition kernel that contains
C     the relationship between the body id and the body-fixed frame
C     name.  See the FRAMES required reading for more details
C     on specifying this relationship.
C
C     The routine determines the body rotation sense using the PCK
C     keyword BODY#_PM. Therefore, you will need to a text PCK file
C     defining the complete set of the standard PCK body rotation
C     keywords for the body of interest. The text PCK file must be
C     loaded independently of whether a binary PCK file providing
C     rotation data for the same body is loaded or not.
C
C     Although it is not currently the case for any of the Solar System
C     bodies, it is possible that the retrograde rotation rate of a
C     body would be slower than the orbital rate of the body rotation
C     around the Sun. The routine does not account for such cases; for
C     them it will compute incorrect the local time progressing
C     backwards.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.2, 18-APR-2014 (BVS)
C
C        Minor edits to long error messages.
C
C-    SPICELIB Version 3.0.1, 09-SEP-2009 (EDW)
C
C        Header edits: deleted a spurious C$ marker from the 
C        "Detailed_Output" section. The existence of the marker
C        caused a failure in the HTML documentation creation script.
C        
C        Deleted the "Revisions" section as it contained several
C        identical entries from the "Version" section.
C
C        Corrected order of header sections.
C
C-    SPICELIB Version 3.0.0, 28-OCT-2006 (BVS)
C
C        Bug fix: incorrect computation of the local time for the
C        bodies with the retrograde rotation causing the local time to
C        flow backwards has been fixed. The local time for all types of
C        bodies now progresses as expected -- midnight, increasing AM
C        hours, noon, increasing PM hours, next midnight, and so on.
C
C-    SPICELIB Version 2.0.0, 03-NOV-2005 (NJB)
C
C        Bug fix:  treatment of planetographic longitude has been
C        updated to be consistent with the SPICE planetographic/
C        rectangular coordinate conversion routines.  The effect of
C        this change is that the default sense of positive longitude
C        for the moon is now east; also, the default sense of positive
C        planetographic longitude now may be overridden for any body
C        (see Particulars above).
C
C        Updated to remove non-standard use of duplicate arguments
C        in RMAIND calls.
C
C-    SPICELIB Version 1.1.0, 24-MAR-1998 (WLT)
C
C        The integer variable SUN was never initialized in the
C        previous version of the routine.  Now it is set to
C        the proper value of 10.
C
C-    SPICELIB Version 1.0.0, 9-JUL-1997 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Compute the local time for a point on a body.
C
C-&


C
C     SPICELIB Functions
C
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
  
      LOGICAL               RETURN
 
C
C     Local parameters
C
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               SUN
      PARAMETER           ( SUN    = 10 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
 
C
C     Local Variables
C 
      CHARACTER*(4)         AMORPM
      CHARACTER*(BDNMLN)    BODNAM
      CHARACTER*(WDSIZE)    FRAME
      CHARACTER*(2)         H
      CHARACTER*(WDSIZE)    BPMKWD
      CHARACTER*(2)         M
      CHARACTER*(WDSIZE)    MYTYPE
      CHARACTER*(2)         S
      CHARACTER*(1)         KWTYPE
 
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      HOURS
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MINS
      DOUBLE PRECISION      MYLONG
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      SECNDS
      DOUBLE PRECISION      SLAT
      DOUBLE PRECISION      SLONG
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TMPANG
      DOUBLE PRECISION      TMPSEC
 
      INTEGER               FRCODE
      INTEGER               HRAMPM
      INTEGER               N
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ET2LST')

 
      CALL LJUST ( TYPE,   MYTYPE )
      CALL UCASE ( MYTYPE, MYTYPE )
 
      IF ( MYTYPE .EQ. 'PLANETOGRAPHIC' ) THEN
C
C        Find planetocentric longitude corresponding to the input
C        longitude.  We first represent in rectangular coordinates
C        a surface point having zero latitude, zero altitude, and
C        the input planetographic longitude. We then find the
C        planetocentric longitude of this point.
C
C        Since PGRREC accepts a body name, map the input code to 
C        a name, if possible.  Otherwise, just convert the input code
C        to a string.
C
         CALL BODC2N ( BODY, BODNAM, FOUND )

         IF ( .NOT. FOUND ) THEN
            CALL INTSTR ( BODY, BODNAM )
         END IF

C
C        Convert planetographic coordinates to rectangular coordinates.
C        All we care about here is longitude.  Set the other inputs
C        as follows:
C
C            Latitude          = 0 
C            Altitude          = 0
C            Equatorial radius = 1
C            Flattening factor = 0
C
         CALL PGRREC ( BODNAM, LONG, 0.D0, 0.D0, 1.D0, 0.D0, SPOINT )

C
C        The output MYLONG is planetocentric longitude.  The other
C        outputs are not used.  Note that the variable RANGE appears
C        later in another RECLAT call; it's not used after that.
C
         CALL RECLAT ( SPOINT, RANGE, MYLONG, LAT )

 
      ELSE IF ( MYTYPE .EQ. 'PLANETOCENTRIC' ) THEN
 
         MYLONG = LONG
 
      ELSE
 
         CALL SETMSG ( 'The coordinate system ''#'' is not a '
     .   //            'recognized system of longitude.  The '
     .   //            'recognized systems are '
     .   //            '''PLANETOCENTRIC'' and '
     .   //            '''PLANETOGRAPHIC''. ' )
 
         CALL ERRCH  ( '#', TYPE )
         CALL SIGERR ( 'SPICE(UNKNOWNSYSTEM)'  )
         CALL CHKOUT ( 'ET2LST' )
         RETURN
 
      END IF
C
C     It's always noon on the surface of the sun.
C
      IF ( BODY .EQ. 10 ) THEN
 
         HR = 12
         MN = 0
         SC = 0
 
         TIME = '12:00:00'
         AMPM = '12:00:00 P.M.'
 
         CALL CHKOUT ( 'ET2LST' )
         RETURN
 
      END IF
 
C
C     Get the body-fixed position of the sun.
C
      CALL CIDFRM ( BODY, FRCODE, FRAME, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The body-fixed frame associated with '
     .   //            'body # could not be determined.  This '
     .   //            'information needs to be "loaded" via a '
     .   //            'frames definition kernel.  See '
     .   //            'frames.req for more details. '         )
         CALL ERRINT ( '#', BODY                               )
         CALL SIGERR ( 'SPICE(CANTFINDFRAME)'                  )
         CALL CHKOUT ( 'ET2LST'                                )
         RETURN
 
      END IF
 
 
 
      CALL SPKEZ  (  SUN,   ET,    FRAME, 'LT+S', BODY, STATE, LT )
      CALL RECLAT (  STATE, RANGE, SLONG,  SLAT )
 
      ANGLE = MYLONG - SLONG
 
C
C     Force the angle into the region from -PI to PI
C
      CALL RMAIND ( ANGLE, TWOPI(), Q, TMPANG )
      ANGLE = TMPANG

      IF ( ANGLE .GT. PI() ) THEN
         ANGLE = ANGLE - TWOPI()
      END IF

C
C     Get the rotation sense of the body and invert the angle if the 
C     rotation sense is retrograde. Use the BODY#_PM PCK keyword to 
C     determine the sense of the body rotation.
C
      BPMKWD = 'BODY#_PM'
      CALL REPMI ( BPMKWD, '#', BODY, BPMKWD )

      CALL DTPOOL ( BPMKWD, FOUND, N, KWTYPE )

      IF (      .NOT. FOUND
     .      .OR. KWTYPE .NE. 'N'
     .      .OR. N      .LT.  2  ) THEN

         CALL SETMSG ( 'The rotation type for the body # could '
     .   //            'not be determined because the # keyword '
     .   //            'was either not found in the POOL or '
     .   //            'or it was not of the expected type and/or ' 
     .   //            'dimension. This keyword is usually '
     .   //            'provided via a planetary constants '
     .   //            'kernel. See pck.req for more details. ')
         CALL ERRINT ( '#', BODY                               )
         CALL ERRCH  ( '#', BPMKWD                             )
         CALL SIGERR ( 'SPICE(CANTGETROTATIONTYPE)'            )
         CALL CHKOUT ( 'ET2LST'                                )
         RETURN

      ELSE

C
C        If the rotation rate is negative, invert the angle.
C
         CALL GDPOOL ( BPMKWD, 2, 1, N, RATE, FOUND )

         IF ( RATE .LT. 0.D0 ) THEN
            ANGLE = - ANGLE
         END IF
         
      END IF

C
C     Convert the angle to "angle seconds" before or after local noon.
C
      SECNDS = 86400.0D0 * ANGLE / TWOPI()
      SECNDS = BRCKTD ( SECNDS, -43200.0D0, 43200.0D0 )
 
C
C     Get the hour, and minutes components of the local time.
C
      CALL RMAIND ( SECNDS, 3600.0D0, HOURS, TMPSEC )
      CALL RMAIND ( TMPSEC, 60.0D0,   MINS,  SECNDS )
C
C     Construct the integer components of the local time.
C
      HR = 12 + INT(HOURS)
      MN =      INT(MINS)
      SC =      INT(SECNDS)
C
C     Set the A.M./P.M. components of local time.
C
      IF      ( HR .EQ. 24 ) THEN
 
         HR     = 0
         HRAMPM = 12
         AMORPM = 'A.M.'
 
      ELSE IF ( HR .GT. 12 ) THEN
 
         HRAMPM = HR - 12
         AMORPM = 'P.M.'
 
      ELSE IF ( HR .EQ. 12 ) THEN
 
         HRAMPM = 12
         AMORPM = 'P.M.'
 
      ELSE IF ( HR .EQ. 0 ) THEN
 
         HRAMPM = 12
         AMORPM = 'A.M.'
 
      ELSE
 
         HRAMPM = HR
         AMORPM = 'A.M.'
 
      END IF
 
C
C     Now construct the two strings we need.
C
      HOURS  = DBLE(HR)
      MINS   = DBLE(MN)
      SECNDS = DBLE(SC)
 
      CALL DPFMT ( HOURS,  '0x', H )
      CALL DPFMT ( MINS,   '0x', M )
      CALL DPFMT ( SECNDS, '0x', S )
 
      TIME = H // ':' // M // ':' // S
 
 
      HOURS = DBLE(HRAMPM)
 
      CALL DPFMT ( HOURS,  '0x', H )
 
      AMPM = H // ':' // M // ':' // S //  ' ' // AMORPM
 
      CALL CHKOUT ( 'ET2LST' )
      RETURN
      END

