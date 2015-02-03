C$Procedure    LS  ( Return L_s, planetocentric longitude of the sun )

      DOUBLE PRECISION FUNCTION LS ( BODY, ET, CORR )
      IMPLICIT NONE

C$ Abstract
C
C     Compute L_s, the planetocentric longitude of the sun, as seen
C     from a specified body.
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
C     GEOMETRY
C
C$ Declarations

      INTEGER               BODY
      DOUBLE PRECISION      ET
      CHARACTER*(*)         CORR

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BODY       I   NAIF integer code of central body.
C     ET         I   Epoch in ephemeris seconds past J2000.
C     CORR       I   Aberration correction.
C
C     The function returns the value of L_s for the specified body
C     at the specified time.
C
C$ Detailed_Input
C
C     BODY        is the NAIF integer code of the central body,
C                 typically a planet.
C
C     ET          is the epoch in ephemeris seconds past J2000 at which
C                 the longitude of the sun (L_s) is to be computed.
C
C     CORR        indicates the aberration corrections to be applied
C                 when computing the longitude of the sun.  CORR
C                 may be any of the following.
C
C                    'NONE'     Apply no correction.
C
C                    'LT'       Correct the position of the sun,
C                               relative to the central body, for
C                               planetary (light time) aberration.
C
C                    'LT+S'     Correct the position of the sun,
C                               relative to the central body, for
C                               planetary and stellar aberrations.
C
C$ Detailed_Output
C
C     The function returns the value of L_s for the specified body
C     at the specified time.  This is the longitude of the Sun,
C     relative to the central body, in a right-handed frame whose
C     basis vectors are defined as follows:
C
C        - The positive Z direction is given by the instantaneous
C          angular velocity vector of the orbit of the body about
C          the sun.
C
C        - The positive X direction is that of the cross product of the
C          instantaneous north spin axis of the body with the positive
C          Z direction.
C
C        - The positive Y direction is Z x X.
C
C     Units are radians; the range is -pi to pi.  Longitudes are
C     positive east.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If no SPK (ephemeris) file has been loaded prior to calling
C        this routine, or if the SPK data has insufficient coverage, an
C        error will be diagnosed and signaled by a routine in the call
C        tree of this routine.
C
C     2) If a PCK file containing rotational elements for the central
C        body has not been loaded prior to calling this routine, an
C        error will be diagnosed and signaled by a routine called by a
C        routine in the call tree of this routine.
C
C     3) If the instantaneous angular velocity and spin axis of BODY
C        are parallel, the return value is unspecified.  
C
C$ Files
C
C     1) An SPK file (or file) containing ephemeris data sufficient to
C        compute the geometric state of the central body relative to
C        the sun at ET must be loaded before this routine is called. If
C        light time correction is used, data must be available that
C        enable computation of the state the sun relative to the solar
C        system barycenter at the light-time corrected epoch.  If
C        stellar aberration correction is used, data must be available
C        that enable computation of the state the central body relative
C        to the solar system barycenter at ET.
C
C     2) A PCK file containing rotational elements for the central body
C        must be loaded before this routine is called.
C
C$ Particulars
C
C     The direction of the vernal equinox for the central body is
C     determined from the instantaneous equatorial and orbital planes
C     of the central body.  This equinox definition is specified in
C     reference [1].  The "instantaneous orbital plane" is interpreted
C     in this routine as the plane normal to the cross product of the
C     position and velocity of the central body relative to the sun.
C     A geometric state is used for this normal vector computation.
C     The "instantaneous equatorial plane" is that normal to the
C     central body's north pole at the requested epoch.  The pole
C     direction is determined from rotational elements loaded via
C     a PCK file.
C
C     The result returned by this routine will depend on the
C     ephemeris data and rotational elements used.  The result may
C     differ from that given in any particular version of the
C     Astronomical Almanac, due to differences in these input data,
C     and due to differences in precision of the computations.
C
C$ Examples
C
C     1) A simple program that computes L_s for Mars.  The geometric
C        state of the sun is used.
C
C
C            PROGRAM MARS_LS
C            IMPLICIT NONE
C
C            DOUBLE PRECISION      DPR
C
C            INTEGER               FILSIZ
C            PARAMETER           ( FILSIZ = 255 )
C
C            CHARACTER*(FILSIZ)    PCK
C            CHARACTER*(FILSIZ)    SPK
C            CHARACTER*(FILSIZ)    LEAP
C            CHARACTER*(30)        UTC
C            CHARACTER*(15)        CORR
C
C            DOUBLE PRECISION      ET
C            DOUBLE PRECISION      LONG
C            DOUBLE PRECISION      LS
C
C            INTEGER               BODY
C            INTEGER               HANDLE
C
C            DATA  BODY   /  499      /
C            DATA  CORR   /  'NONE'   /
C
C
C            CALL PROMPT ( 'Enter name of leapseconds kernel > ', LEAP )
C            CALL PROMPT ( 'Enter name of PCK file           > ', PCK  )
C            CALL PROMPT ( 'Enter name of SPK file           > ', SPK  )
C
C            CALL FURNSH ( LEAP )
C            CALL FURNSH ( PCK  )
C            CALL FURNSH ( SPK  )
C
C            WRITE (*,*) ' '
C            WRITE (*,*) 'Kernels have been loaded.'
C            WRITE (*,*) ' '
C
C            DO WHILE ( .TRUE. )
C
C               CALL PROMPT ( 'Enter UTC time > ', UTC )
C
C               CALL UTC2ET ( UTC, ET )
C
C      C
C      C        Convert longitude to degrees and move it into the range
C      C        [0, 360).
C      C
C               LONG = DPR() * LS ( BODY, ET, CORR )
C
C               IF ( LONG .LT. 0.D0 ) THEN
C                  LONG = LONG + 360.D0
C               END IF
C
C               WRITE (*,*) ' '
C               WRITE (*,*) 'Mars L_s (deg.) = ',  LONG
C               WRITE (*,*) ' '
C
C            END DO
C
C            END
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1] "The Astronomical Almanac for the Year 2005." U.S. Government
C         Printing Office, Washington, D.C., 1984, page L9.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Chronos Version 1.1.2, 02-OCT-2006 (BVS)
C
C        Replaced LDPOOL and SPKELF with FURNSH in the Examples
C        section.
C
C-    Chronos Version 1.1.1, 07-JAN-2005 (NJB)
C
C        Description of reference frame in Detailed_Output header
C        section was corrected.  Miscellaneous other header updates
C        were made.
C
C-    Beta Version 1.1.0, 14-DEC-1996 (NJB)
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         REF
      PARAMETER           ( REF     = 'J2000' )

      INTEGER               SUN
      PARAMETER           ( SUN     = 10 )

C
C     Local variables
C
      DOUBLE PRECISION      UAVEL  ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NPOLE  ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TRANS  ( 3, 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         LS = 0.D0
         RETURN
      ELSE
         CALL CHKIN ( 'LS' )
      END IF

C
C     Look up the direction of the North pole of the central body.
C
      CALL TIPBOD ( REF, BODY, ET, TIPM )

      DO I = 1, 3
         NPOLE(I) = TIPM(3,I)
      END DO

C
C     Get the geometric state of the body relative to the sun.
C
      CALL SPKEZ ( BODY, ET, REF, 'NONE', SUN, STATE, LT )

C
C     Get the unit direction vector parallel to the angular velocity
C     vector of the orbit.  This is just the unitized cross product of
C     position and velocity.
C
      CALL UCRSS ( STATE, STATE(4), UAVEL )

C
C     We want to form a transformation matrix that maps vectors from
C     basis REF to the following frame:
C
C        Z  =  UAVEL
C
C        X  =  NPOLE x UAVEL
C
C        Y  =  Z x X
C
C     We'll find the position of the Sun relative to this frame.  In
C     our computations, we want our basis vectors to have unit length.
C
      CALL VEQU  ( UAVEL, Z     )
      CALL UCRSS ( NPOLE, Z,  X )
      CALL UCRSS ( Z,     X,  Y )

      DO I = 1, 3
         TRANS(1,I) = X(I)
         TRANS(2,I) = Y(I)
         TRANS(3,I) = Z(I)
      END DO

C
C     Get the state of the sun in frame REF.  Since we may be using
C     aberration corrections, this is not necessarily the negative of
C     the state we've just found.
C
      CALL SPKEZ ( SUN, ET, REF, CORR, BODY, STATE, LT )

C
C     Now transform the position of the Sun into the "equator and
C     equinox" frame.
C
      CALL MXV ( TRANS, STATE, POS )

C
C     Let RECLAT find the longitude LS for us.
C
      CALL RECLAT ( POS, RADIUS, LS, LAT )

      CALL CHKOUT ( 'LS' )
      RETURN
      END
