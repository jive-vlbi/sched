C$Procedure    LSPCN  ( Longitude of the sun, planetocentric )

      DOUBLE PRECISION FUNCTION LSPCN ( BODY, ET, ABCORR )

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
C     NAIF_IDS
C     PCK
C     TIME
C     SPK
C
C$ Keywords
C
C     GEOMETRY
C     TIME
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         BODY
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BODY       I   Name of central body.
C     ET         I   Epoch in seconds past J2000 TDB.
C     ABCORR     I   Aberration correction.
C
C     The function returns the value of L_s for the specified body
C     at the specified time.
C
C$ Detailed_Input
C
C     BODY        is the name of the central body, typically a planet.
C
C     ET          is the epoch at which the longitude of the sun (L_s)
C                 is to be computed. ET is expressed as seconds past
C                 J2000 TDB (Barycentric Dynamical Time).
C
C     ABCORR      indicates the aberration corrections to be applied
C                 when computing the longitude of the sun.  ABCORR may
C                 be any of the following.
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
C     The function returns the planetocentric longitude of the sun,
C     often called "L_s," for the specified body at the specified time.
C     This is the longitude of the body-sun vector in a right-handed
C     frame whose basis vectors are defined as follows:
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
C     Units are radians; the range is 0 to 2*pi.  Longitudes are
C     positive to the east.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input body name cannot be translated to an ID code,
C        and if the name is not a string representation of an integer
C        (for example, '399'), the error SPICE(NOTRANSLATION) is
C        signaled.
C
C     2) If no SPK (ephemeris) file has been loaded prior to calling
C        this routine, or if the SPK data has insufficient coverage, an
C        error will be diagnosed and signaled by a routine in the call
C        tree of this routine.
C
C     3) If a PCK file containing rotational elements for the central
C        body has not been loaded prior to calling this routine, an
C        error will be diagnosed and signaled by a routine called by a
C        routine in the call tree of this routine.
C
C     4) If the instantaneous angular velocity and spin axis of BODY
C        are parallel, the error will be diagnosed and signaled by a
C        routine in the call tree of this routine.
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
C     The geometric state of the central body relative to the sun is
C     used for this normal vector computation. The "instantaneous
C     equatorial plane" is normal to the central body's north pole
C     at the requested epoch.  The pole direction is determined from
C     rotational elements loaded via a PCK file.
C
C     The result returned by this routine will depend on the
C     ephemeris data and rotational elements used.  The result may
C     differ from that given in any particular version of the
C     Astronomical Almanac, due to differences in these input data,
C     and due to differences in precision of the computations.
C
C$ Examples
C
C     1) A simple program that computes L_s for a body and time
C        supplied interactively.  The geometric state of the sun is
C        used.
C
C
C            PROGRAM EX1
C            IMPLICIT NONE
C
C            DOUBLE PRECISION      DPR
C            DOUBLE PRECISION      LSPCN
C
C            CHARACTER*(*)         ABCORR
C            PARAMETER           ( ABCORR = 'NONE' )
C
C            INTEGER               FILSIZ
C            PARAMETER           ( FILSIZ = 255 )
C            
C            INTEGER               NAMLEN
C            PARAMETER           ( NAMLEN = 36 )
C
C            INTEGER               TIMLEN
C            PARAMETER           ( TIMLEN = 40 )
C
C            CHARACTER*(NAMLEN)    BODY
C            CHARACTER*(FILSIZ)    LSK
C            CHARACTER*(FILSIZ)    PCK
C            CHARACTER*(FILSIZ)    SPK
C            CHARACTER*(TIMLEN)    TIMSTR
C
C            DOUBLE PRECISION      ET
C            DOUBLE PRECISION      LON
C
C
C            CALL PROMPT ( 'Enter name of leapseconds kernel > ', LSK )
C            CALL PROMPT ( 'Enter name of PCK file           > ', PCK )
C            CALL PROMPT ( 'Enter name of SPK file           > ', SPK )
C
C            CALL FURNSH ( LSK )
C            CALL FURNSH ( PCK )
C            CALL FURNSH ( SPK )
C
C            WRITE (*,*) ' '
C            WRITE (*,*) 'Kernels have been loaded.'
C            WRITE (*,*) ' '
C
C            DO WHILE ( .TRUE. )
C
C               CALL PROMPT ( 'Enter name of central body       > ', 
C           .                  BODY                                  )
C               CALL PROMPT ( 'Enter calendar, JD, or DOY time  > ', 
C           .                  TIMSTR                                )
C
C               CALL STR2ET ( TIMSTR, ET )
C
C      C
C      C        Convert longitude to degrees.
C      C
C               LON = DPR() * LSPCN ( BODY, ET, ABCORR )
C
C               WRITE (*,*) ' '
C               WRITE (*,*) 'Central body              = ',  BODY
C               WRITE (*,*) 'Time                      = ',  TIMSTR
C               WRITE (*,*) 'Planetocentric L_s (deg.) = ',  LON
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
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 19-SEP-2013 (BVS)
C
C        Updated to save the input body name and ZZBODTRN state
C        counter and to do name-ID conversion only if the counter
C        has changed.
C
C-    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB)
C
C-&

C$ Index_Entries
C
C     planetocentric longitude of sun
C     compute L_s
C     compute Ls
C     compute L_sub_s
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         REF
      PARAMETER           ( REF     = 'J2000' )

      INTEGER               SUN
      PARAMETER           ( SUN     = 10 )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )


C
C     Local variables
C
      DOUBLE PRECISION      UAVEL  ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NPOLE  ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      BSTATE ( 6 )
      DOUBLE PRECISION      SSTATE ( 6 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TRANS  ( 3, 3 )

      INTEGER               IDCODE
      INTEGER               I

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVBODY
      INTEGER               SVIDCD
      LOGICAL               SVFND1

      LOGICAL               FIRST

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVBODY
      SAVE                  SVIDCD
      SAVE                  SVFND1

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Give the function an initial value.
C     
      LSPCN = 0.D0

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'LSPCN' )

C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counters
C
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF

C
C     Map the body name to an ID code.
C
      CALL ZZBODS2C ( SVCTR1, SVBODY, SVIDCD, SVFND1,
     .                BODY, IDCODE, FOUND    )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The body name # could not be translated ' //
     .                 'to a NAIF ID code.  The cause of this '   //
     .                 'problem may be that you need an updated ' //
     .                 'version of the SPICE Toolkit.'            )
         CALL ERRCH  ( '#', BODY                                  )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                     )
         CALL CHKOUT ( 'LSPCN'                                    )
         RETURN

      END IF

C
C     Look up the direction of the North pole of the central body.
C     Note that TIPBOD does make use of binary PCK data if available.
C
      CALL TIPBOD ( REF, IDCODE, ET, TIPM )

      DO I = 1, 3
         NPOLE(I) = TIPM(3,I)
      END DO

C
C     Get the geometric state of the body relative to the sun.
C
      CALL SPKGEO ( IDCODE, ET, REF, SUN, BSTATE, LT )

C
C     Get the unit direction vector parallel to the angular velocity
C     vector of the orbit.  This is just the unitized cross product of
C     position and velocity.
C
      CALL UCRSS ( BSTATE, BSTATE(4), UAVEL )

C
C     We want to create a transformation matrix that maps vectors from
C     basis REF to the following frame:

C        Z  =  UAVEL
C
C        X  =  NPOLE x UAVEL
C
C        Y  =  Z x X
C
C     This is a "two-vector" frame with the unit orbital
C     angular velocity vector UAVEL as the primary vector and the
C     spin axis NPOLE as the secondary vector.  The primary 
C     vector is associated with the +Z axis; the secondary vector
C     is associated with the +Y axis.
C
      CALL TWOVEC ( UAVEL, 3, NPOLE, 2, TRANS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'LSPCN' )
         RETURN
      END IF

C
C     We'll find the position of the Sun relative to this frame. 
C
C     Get the state of the sun in frame REF.  Since we may be using
C     aberration corrections, this is not necessarily the negative of
C     the state we've just found.
C
      CALL SPKEZR ( 'SUN', ET, REF, ABCORR, BODY, SSTATE, LT )

C
C     Now transform the position of the Sun into the "orbit plane
C     and equinox" frame.
C
      CALL MXV ( TRANS, SSTATE, POS )

C
C     Let RECRAD find the longitude LS for us.  RECRAD performs
C     the same coordinate transformation as the more commonly used
C     RECLAT, but the range of right ascension is 0:2*pi, which is
C     what we want for Ls.
C
      CALL RECRAD ( POS, RADIUS, LSPCN, LAT )

      CALL CHKOUT ( 'LSPCN' )
      RETURN
      END
