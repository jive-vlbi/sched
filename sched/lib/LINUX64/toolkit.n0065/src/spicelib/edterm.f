C$Procedure EDTERM ( Ellipsoid terminator )
 
      SUBROUTINE EDTERM ( TRMTYP, SOURCE, TARGET, ET,      
     .                    FIXREF, ABCORR, OBSRVR, NPTS, 
     .                    TRGEPC, OBSPOS, TRMPTS        )
  
C$ Abstract
C
C     Compute a set of points on the umbral or penumbral terminator of
C     a specified target body, where the target shape is modeled as an
C     ellipsoid.
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
C     FRAMES
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     BODY
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         TRMTYP
      CHARACTER*(*)         SOURCE
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      INTEGER               NPTS
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      TRMPTS ( 3, NPTS )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TRMTYP     I   Terminator type.
C     SOURCE     I   Light source.
C     TARGET     I   Target body.
C     ET         I   Observation epoch.
C     FIXREF     I   Body-fixed frame associated with target.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Observer.
C     NPTS       I   Number of points in terminator set.
C     TRGEPC     O   Epoch associated with target center.
C     OBSPOS     O   Position of observer in body-fixed frame.
C     TRMPTS     O   Terminator point set.
C   
C$ Detailed_Input
C
C     TRMTYP      is a string indicating the type of terminator to
C                 compute: umbral or penumbral. The umbral terminator
C                 is the boundary of the portion of the ellipsoid
C                 surface in total shadow. The penumbral terminator is
C                 the boundary of the portion of the surface that is
C                 completely illuminated. Note that in astronomy
C                 references, the unqualified word "terminator" refers
C                 to the umbral terminator. Here, the unqualified 
C                 word refers to either type of terminator.
C
C                 Possible values of TRMTYP are
C
C                    'UMBRAL' 
C                    'PENUMBRAL'
C
C                 Case and leading or trailing blanks in TRMTYP are
C                 not significant.
C
C
C     SOURCE      is the name of the body acting as a light source.
C                 SOURCE is case-insensitive, and leading and trailing
C                 blanks in TARGET are not significant. Optionally, you
C                 may supply a string containing the integer ID code
C                 for the object. For example both 'SUN' and '10' are
C                 legitimate strings that indicate the Sun is the light
C                 source.
C
C                 This routine assumes that a kernel variable
C                 representing the light source's radii is present in
C                 the kernel pool. Normally the kernel variable would
C                 be defined by loading a PCK file.
C
C                 The shape of the light source is always modeled as a
C                 sphere, regardless of whether radii defining a
C                 triaxial ellipsoidal shape model are available in the
C                 kernel pool. The maximum radius of the body is used
C                 as the radius of the sphere.
C
C
C     TARGET      is the name of the target body. TARGET is
C                 case-insensitive, and leading and trailing blanks in
C                 TARGET are not significant. Optionally, you may
C                 supply a string containing the integer ID code for
C                 the object. For example both 'MOON' and '301' are
C                 legitimate strings that indicate the moon is the
C                 target body.
C
C                 This routine assumes that a kernel variable
C                 representing the target's radii is present in the
C                 kernel pool. Normally the kernel variable would be
C                 defined by loading a PCK file.
C
C
C     ET          is the epoch of participation of the observer,
C                 expressed as ephemeris seconds past J2000 TDB: ET is
C                 the epoch at which the observer's position is
C                 computed.
C
C                 When aberration corrections are not used, ET is also
C                 the epoch at which the position and orientation of the
C                 target body and position of the light source are
C                 computed.
C
C                 When aberration corrections are used, ET is the epoch
C                 at which the observer's position relative to the
C                 solar system barycenter is computed; in this case the
C                 position and orientation of the target body are
C                 computed at ET-LT, where LT is the one-way light time
C                 between the target body's center and the observer.
C                 See the description of ABCORR below for details.
C
C
C     FIXREF      is the name of the reference frame relative to which
C                 the output terminator points are expressed. This must
C                 be a body-centered, body-fixed frame associated with
C                 the target. The frame's axes must be compatible with
C                 the triaxial ellipsoidal shape model associated with
C                 the target body (normally provide via a PCK): this
C                 routine assumes that the first, second, and third
C                 axis lengths correspond, respectively, to the x, y,
C                 and z-axes of the frame designated by FIXREF.
C
C                 FIXREF may refer to a built-in frame (documented in
C                 the Frames Required Reading) or a frame defined by a
C                 loaded frame kernel (FK).
C
C                 The orientation of the frame designated by FIXREF is
C                 evaluated at epoch of participation of the target
C                 body. See the descriptions of ET and ABCORR for
C                 details.
C
C
C     ABCORR      indicates the aberration correction to be applied
C                 when computing the observer-target position, the
C                 orientation of the target body, and the target-
C                 source position vector. ABCORR may be any of
C                 the following.
C
C                    'NONE'     Apply no correction. Compute the 
C                               terminator points using the position
C                               of the light source and target, and
C                               the orientation of the target, at ET.
C
C                 Let LT represent the one-way light time between the
C                 observer and the target body's center. The following
C                 values of ABCORR apply to the "reception" case in
C                 which photons depart from the target body's center at
C                 the light-time corrected epoch ET-LT and *arrive* at
C                 the observer's location at ET:
C
C
C                    'LT'       Correct for one-way light time (also
C                               called "planetary aberration") using a
C                               Newtonian formulation. This correction
C                               yields the location of the terminator
C                               points at the approximate time they
C                               emitted photons arriving at the
C                               observer at ET (the difference between
C                               light time to the target center and
C                               light time to the terminator points
C                               is ignored).
C 
C                               The light time correction uses an
C                               iterative solution of the light time
C                               equation. The solution invoked by the
C                               'LT' option uses one iteration.
C
C                               The target position as seen by the
C                               observer, the position of the light
C                               source as seen from the target at
C                               ET-LT, and the rotation of the target
C                               body, are corrected for light time.
C
C                    'LT+S'     Correct for one-way light time and
C                               stellar aberration using a Newtonian
C                               formulation. This option modifies the
C                               positions obtained with the 'LT' option
C                               to account for the observer's velocity
C                               relative to the solar system
C                               barycenter. This correction also
C                               applies to the position of the light
C                               source relative to the target. The
C                               result is the apparent terminator as
C                               seen by the observer.
C
C                    'CN'       Converged Newtonian light time
C                               correction. In solving the light time
C                               equation, the 'CN' correction iterates
C                               until the solution converges. The
C                               position and rotation of the target
C                               body and the position of the light
C                               source relative to the target are
C                               corrected for light time.
C
C                    'CN+S'     Converged Newtonian light time
C                               and stellar aberration corrections.
C
C
C     OBSRVR      is the name of the observing body. This is typically
C                 a spacecraft, the Earth, or a surface point on the
C                 Earth. OBSRVR is case-insensitive, and leading and
C                 trailing blanks in OBSRVR are not significant.
C                 Optionally, you may supply a string containing the
C                 integer ID code for the object. For example both
C                 'EARTH' and '399' are legitimate strings that indicate
C                 the Earth is the observer.
C
C                    
C     NPTS        is the number of terminator points to compute.
C
C     
C$ Detailed_Output
C
C     TRGEPC      is the "target epoch."  TRGEPC is defined as follows:
C                 letting LT be the one-way light time between the
C                 target center and observer, TRGEPC is either the
C                 epoch ET-LT or ET depending on whether the requested
C                 aberration correction is, respectively, for received
C                 radiation or omitted. LT is computed using the
C                 method indicated by ABCORR.
C
C                 TRGEPC is expressed as seconds past J2000 TDB.
C
C
C     OBSPOS      is the vector from the center of the target body at
C                 epoch TRGEPC to the observer at epoch ET. OBSPOS is
C                 expressed in the target body-fixed reference frame
C                 FIXREF, which is evaluated at TRGEPC.
C
C                 OBSPOS is returned to simplify various related
C                 computations that would otherwise be cumbersome. For
C                 example, the vector XVEC from the observer to the
C                 Ith terminator point can be calculated via the call
C
C                    CALL VSUB ( TRMPTS(1,I), OBSPOS, XVEC )
C
C                 To transform the vector OBSPOS from a reference frame
C                 FIXREF at time TRGEPC to a time-dependent reference
C                 frame REF at time ET, the routine PXFRM2 should be
C                 called. Let XFORM be the 3x3 matrix representing the
C                 rotation from the reference frame FIXREF at time
C                 TRGEPC to the reference frame REF at time ET. Then
C                 OBSPOS can be transformed to the result REFVEC as
C                 follows:
C
C                     CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM )
C                     CALL MXV    ( XFORM,  OBSPOS, REFVEC )
C
C
C     TRMPTS      is an array of points on the umbral or penumbral
C                 terminator of the ellipsoid, as specified by the
C                 input argument TRMTYP. The Ith point is contained in
C                 the array elements
C
C                     TRMPTS(J,I),  J = 1, 2, 3
C
C                 Each terminator point is the point of tangency of a
C                 plane that is also tangent to the light source. These
C                 associated points of tangency on the light source
C                 have uniform distribution in longitude when expressed
C                 in a cylindrical coordinate system whose Z-axis is
C                 the target center to source center vector. The
C                 magnitude of the separation in longitude between the
C                 tangency points on the light source is
C
C                    2*Pi / NPTS 
C
C                 If the target is spherical, the terminator points
C                 also are uniformly distributed in longitude in the
C                 cylindrical system described above. If the target is
C                 non-spherical, the longitude distribution of the
C                 points generally is not uniform.
C                                        
C                 The terminator points are expressed in the body-fixed
C                 reference frame designated by FIXREF. Units are km.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name FIXREF cannot be mapped
C         to a frame ID code, the error SPICE(NOTRANSLATION) is
C         signaled.
C
C     2)  If the target name TARGET cannot be mapped
C         to a body ID code, the error SPICE(NOTRANSLATION) is
C         signaled.
C
C     3)  If the frame designated by FIXREF is not centered
C         on the target, the error SPICE(INVALIDFIXREF) is
C         signaled.
C
C     4)  If the terminator type is not recognized, the error
C         will be diagnosed by a routine in the call tree of
C         this routine.
C
C     5)  If the terminator point count NPTS is not at least 1, the
C         error will be diagnosed by a routine in the call tree of this
C         routine.
C
C     6)  If any of the ellipsoid's semi-axis lengths are non-positive,
C         the error will be diagnosed by a routine in the call tree of
C         this routine.
C
C     7)  If the light source has non-positive radius, the error
C         will be diagnosed by a routine in the call tree of
C         this routine.
C
C     8)  If the light source intersects the smallest sphere
C         centered at the origin and containing the ellipsoid, the
C         error will be diagnosed by a routine in the call tree of
C         this routine.
C
C     9)  If radii for the target body or light source are not
C         available in the kernel pool, the error will be diagnosed by
C         a routine in the call tree of this routine. If radii are
C         available but either body does not have three radii, the
C         error SPICE(INVALIDCOUNT) will be signaled.
C
C     10) If any SPK look-up fails, the error will be diagnosed by
C         a routine in the call tree of this routine.
C
C$ Files
C
C     Appropriate SPK, PCK, and frame kernels must be loaded by the
C     calling program before this routine is called. 
C
C     The following data are required:
C
C        - SPK data: ephemeris data for the target, observer, and light
C          source must be loaded. If aberration corrections are used,
C          the states of all three objects relative to the solar system
C          barycenter must be calculable from the available ephemeris
C          data. Typically ephemeris data are made available by loading
C          one or more SPK files via FURNSH.
C
C        - PCK data: triaxial radii for the target body and
C          the light source must be loaded into the kernel pool.
C          Typically this is done by loading a text PCK file via
C          FURNSH.
C
C        - Further PCK data: rotation data for the target body must
C          be loaded. These may be provided in a text or binary PCK
C          file. 
C
C        - Frame data: if a frame definition is required to convert
C          the observer and target states to the target body-fixed
C          frame designated by FIXREF, that definition must be
C          available in the kernel pool. Typically the definitions of
C          frames not already built-in to SPICE are supplied by loading
C          a frame kernel.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine models the boundaries of shadow regions on an
C     ellipsoidal target body "illuminated" by a spherical light
C     source. Light rays are assumed to travel along straight lines;
C     refraction is not modeled.
C
C     Points on the target body's surface are classified according to
C     their illumination as follows:
C
C        -  A target surface point X for which no vector from X to any
C           point in the light source intersects the target, except at
C           X, is considered to be "completely illuminated."
C
C        -  A target surface point X for which each vector from X to a
C           point in the light source intersects the target at points 
C           other than X is considered to be "in total shadow."
C
C        -  All other target points are considered to be in partial 
C           shadow.
C
C     In this routine, we use the term "umbral terminator" to denote
C     the curve usually called the "terminator": this curve is the
C     boundary of the portion of the target body's surface that lies in
C     total shadow. We use the term "penumbral terminator" to denote
C     the boundary of the completely illuminated portion of the
C     surface.
C
C     In general, the terminator on an ellipsoid is a more complicated
C     curve than the limb (which is always an ellipse). Aside from
C     various special cases, the terminator does not lie in a plane.
C
C     However, the condition for a point X on the ellipsoid to lie on
C     the terminator is simple: a plane tangent to the ellipsoid at X
C     must also be tangent to the light source. If this tangent plane
C     does not intersect the vector from the center of the ellipsoid to
C     the center of the light source, then X lies on the umbral
C     terminator; otherwise X lies on the penumbral terminator.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C
C     1)  Compute sets of umbral and penumbral terminator points on the
C         Moon. Perform a consistency check using the solar incidence
C         angle at each point. We expect to see a solar incidence angle
C         of approximately 90 degrees. Since the solar incidence angle
C         is measured between the local outward normal and the
C         direction to the center of the Sun, the solar incidence angle
C         at an umbral terminator point should exceed 90 degrees by
C         approximately the angular radius of the Sun, while the angle
C         at a penumbral terminator point should be less than 90
C         degrees by that amount.
C
C         This program loads SPICE kernels via a meta-kernel. The 
C         meta-kernel used to produce the results shown below is
C
C            KPL/MK
C
C            This meta-kernel is intended to support operation of SPICE
C            example programs. The kernels shown here should not be
C            assumed to contain adequate or correct versions of data
C            required by SPICE-based user applications.
C
C            In order for an application to use this meta-kernel, the
C            kernels referenced here must be present in the user's
C            current working directory.
C
C
C            \begindata
C
C               KERNELS_TO_LOAD = ( 'de421.bsp',
C                                   'pck00010.tpc',
C                                   'naif0010.tls'  )
C
C            \begintext
C
C
C         Program source code: 
C
C
C               PROGRAM EX1
C               IMPLICIT NONE
C         C
C         C     SPICELIB functions
C         C
C               DOUBLE PRECISION      DPR
C               DOUBLE PRECISION      VDIST
C         C
C         C     Local parameters
C         C
C               CHARACTER*(*)         FMT0
C               PARAMETER           ( FMT0   = '(1X,A,I2,A)' )
C
C               CHARACTER*(*)         FMT1
C               PARAMETER           ( FMT1   = '(1X,A,F18.9)' )
C
C               CHARACTER*(*)         META
C               PARAMETER           ( META   = 'edterm.tm' )
C
C               INTEGER               NPTS
C               PARAMETER           ( NPTS   = 3 )
C
C               INTEGER               CORLEN
C               PARAMETER           ( CORLEN = 5 )
C
C               INTEGER               BDNMLN
C               PARAMETER           ( BDNMLN = 36 )
C
C               INTEGER               FRNMLN
C               PARAMETER           ( FRNMLN = 32 )
C
C               INTEGER               TIMLEN
C               PARAMETER           ( TIMLEN = 50 )
C
C               INTEGER               TYPLEN
C               PARAMETER           ( TYPLEN = 10 )
C
C               INTEGER               NTYPES
C               PARAMETER           ( NTYPES = 2 )
C         C
C         C     Local variables
C         C
C               CHARACTER*(CORLEN)    ABCORR
C               CHARACTER*(FRNMLN)    FIXREF
C               CHARACTER*(BDNMLN)    OBSRVR
C               CHARACTER*(BDNMLN)    SOURCE
C               CHARACTER*(BDNMLN)    TARGET
C               CHARACTER*(TYPLEN)    TRMTPS ( NTYPES )
C               CHARACTER*(TIMLEN)    UTC
C
C               DOUBLE PRECISION      ANGRAD
C               DOUBLE PRECISION      EMISSN
C               DOUBLE PRECISION      ET
C               DOUBLE PRECISION      LAT
C               DOUBLE PRECISION      LON
C               DOUBLE PRECISION      LT
C               DOUBLE PRECISION      OBSPOS ( 3 )
C               DOUBLE PRECISION      PHASE
C               DOUBLE PRECISION      RADIUS
C               DOUBLE PRECISION      S      ( NTYPES )
C               DOUBLE PRECISION      SOLAR
C               DOUBLE PRECISION      SRCPOS ( 3 )
C               DOUBLE PRECISION      SRCRAD ( 3 )
C               DOUBLE PRECISION      SRFVEC ( 3 )
C               DOUBLE PRECISION      TRGEPC
C               DOUBLE PRECISION      TRMPTS ( 3, NPTS )
C
C               INTEGER               I
C               INTEGER               N
C               INTEGER               TRMIDX
C
C               LOGICAL               FIRST
C
C         C
C         C     Initial values
C         C
C               DATA                  FIRST  / .TRUE.               /
C               DATA                  TRMTPS / 'UMBRAL', 'PENUMBRAL'/
C               DATA                  S      / -1.D0,    1.D0       /
C
C         C
C         C     Load the meta-kernel.
C         C
C               CALL FURNSH ( META )
C
C         C
C         C     Set the observation time.
C         C
C               UTC    = '2007 FEB 3 00:00:00.000'
C
C               CALL STR2ET ( UTC, ET )
C
C         C
C         C     Set the participating objects, the reference
C         C     frame, and the aberration correction.
C         C
C               OBSRVR = 'EARTH'
C               TARGET = 'MOON'
C               SOURCE = 'SUN'
C               FIXREF = 'IAU_MOON'
C               ABCORR = 'LT+S'
C         C
C         C     Look up the radii of the Sun.
C         C
C               CALL BODVRD ( SOURCE, 'RADII', 3, N, SRCRAD )
C
C         C
C         C     Compute terminator points.
C         C
C               DO TRMIDX = 1, 2
C
C                  CALL EDTERM ( TRMTPS(TRMIDX), SOURCE, TARGET,
C              .                 ET,             FIXREF, ABCORR,
C              .                 OBSRVR,         NPTS,   TRGEPC,
C              .                 OBSPOS,         TRMPTS          )
C         C
C         C        Validate terminator points.
C         C
C         C        Look up the target-sun vector at the light-time
C         C        corrected target epoch.
C         C
C                  IF ( FIRST ) THEN
C
C                     CALL SPKPOS ( SOURCE, TRGEPC, FIXREF,
C              .                    ABCORR, TARGET, SRCPOS, LT )
C                     FIRST = .FALSE.
C
C                  END IF
C
C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Terminator type: '//TRMTPS(TRMIDX)
C
C                  DO I = 1, NPTS
C
C                     WRITE (*,*) ' '
C
C                     CALL RECLAT ( TRMPTS(1,I), RADIUS, LON, LAT )
C
C                     WRITE (*,FMT0) '  Terminator point ', I, ':'
C                     WRITE (*,FMT1)
C              .            '    Radius                     (km):  ',
C              .            RADIUS
C                     WRITE (*,FMT1)
C              .            '    Planetocentric longitude   (deg): ',
C              .            LON*DPR()
C                     WRITE (*,FMT1)
C              .            '    Planetocentric latitude    (deg): ',
C              .            LAT*DPR()
C
C         C
C         C           Find the illumination angles at the
C         C           Ith terminator point.
C         C
C                     CALL ILUMIN ( 'Ellipsoid',  TARGET, ET,
C              .                     FIXREF,      ABCORR, OBSRVR,
C              .                     TRMPTS(1,I), TRGEPC, SRFVEC,
C              .                     PHASE,       SOLAR,  EMISSN )
C
C                     WRITE (*,FMT1)
C              .            '    Solar incidence angle      (deg): ',
C              .            SOLAR*DPR()
C         C
C         C           Display the solar incidence angle after
C         C           adjusting the angle for the angular radius
C         C           of the Sun as seen from the Ith terminator
C         C           point. The result should be approximately
C         C           90 degrees.
C         C
C                     ANGRAD = ASIN(   SRCRAD(1)
C              .                     / VDIST( SRCPOS, TRMPTS(1,I) ) )
C
C                     WRITE (*, '(1X,A)' )
C              .            '    Solar incidence angle adjusted for'
C                     WRITE (*,FMT1)
C              .            '    sun''s angular radius (deg):       ',
C              .            (SOLAR + S(TRMIDX)*ANGRAD) * DPR()
C                  END DO
C
C               END DO
C
C               END
C
C
C        When this program was executed on a PC/Linux/gfortan platform,
C        the output was:
C
C
C           Terminator type: UMBRAL
C
C             Terminator point  1:
C               Radius                     (km):      1737.400000000
C               Planetocentric longitude   (deg):      -95.084552819
C               Planetocentric latitude    (deg):        0.004052763
C               Solar incidence angle      (deg):       90.269765819
C               Solar incidence angle adjusted for
C               sun's angular radius (deg):             90.000000129
C
C             Terminator point  2:
C               Radius                     (km):      1737.400000000
C               Planetocentric longitude   (deg):       84.228091534
C               Planetocentric latitude    (deg):       59.995755519
C               Solar incidence angle      (deg):       90.269765706
C               Solar incidence angle adjusted for
C               sun's angular radius (deg):             90.000000016
C
C             Terminator point  3:
C               Radius                     (km):      1737.400000000
C               Planetocentric longitude   (deg):       87.216417974
C               Planetocentric latitude    (deg):      -59.979550515
C               Solar incidence angle      (deg):       90.269765730
C               Solar incidence angle adjusted for
C               sun's angular radius (deg):             90.000000040
C
C           Terminator type: PENUMBRAL
C
C             Terminator point  1:
C               Radius                     (km):      1737.400000000
C               Planetocentric longitude   (deg):       84.914100511
C               Planetocentric latitude    (deg):       -0.004073047
C               Solar incidence angle      (deg):       89.730234406
C               Solar incidence angle adjusted for
C               sun's angular radius (deg):             90.000000126
C
C             Terminator point  2:
C               Radius                     (km):      1737.400000000
C               Planetocentric longitude   (deg):      -95.769215814
C               Planetocentric latitude    (deg):      -59.995785101
C               Solar incidence angle      (deg):       89.730234298
C               Solar incidence angle adjusted for
C               sun's angular radius (deg):             90.000000018
C
C             Terminator point  3:
C               Radius                     (km):      1737.400000000
C               Planetocentric longitude   (deg):      -92.780892017
C               Planetocentric latitude    (deg):       59.979498997
C               Solar incidence angle      (deg):       89.730234322
C               Solar incidence angle adjusted for
C               sun's angular radius (deg):             90.000000042
C
C
C$ Restrictions
C
C     1) This routine models light paths as straight lines.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 31-MAR-2014 (NJB) (BVS)
C
C        A correction was made to the Detailed_Output section of 
C        the header: the subroutine name VMINUS was changed to VSUB.
C
C        The header example program was re-written. The metakernel for
C        the example program has been updated, as was the program's
C        output.
C
C        Various portions of the header were re-written.
C
C        Updated to save the input body names and ZZBODTRN state
C        counters and to do name-ID conversions only if the counters
C        have changed.
C
C        Updated to save the input frame name and POOL state counter
C        and to do frame name-ID conversion only if the counter has
C        changed.
C
C-    SPICELIB Version 1.0.0, 03-FEB-2007 (NJB)
C
C-&
 
C$ Index_Entries
C
C     find terminator on ellipsoid
C     find umbral terminator on ellipsoid
C     find penumbral terminator on ellipsoid
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL   = 36 )

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )


C
C     Local variables
C
      DOUBLE PRECISION      LTSRC
      DOUBLE PRECISION      LTTARG
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SRCPOS ( 3 )
      DOUBLE PRECISION      SRCRAD ( 3 )
      DOUBLE PRECISION      TRGPOS ( 3 )
      DOUBLE PRECISION      TRGRAD ( 3 )

      INTEGER               N
      INTEGER               CENTER
      INTEGER               CLSSID
      INTEGER               FRCLAS
      INTEGER               FRCODE
      INTEGER               OBSID
      INTEGER               SRCID
      INTEGER               TRGID

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTGID
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVSCRE
      INTEGER               SVSRCI
      LOGICAL               SVFND2

      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSR
      INTEGER               SVOBSI
      LOGICAL               SVFND3

      LOGICAL               FIRST

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR4 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVFREF
      INTEGER               SVFRCD


C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTGID
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVSCRE
      SAVE                  SVSRCI
      SAVE                  SVFND2

      SAVE                  SVCTR3
      SAVE                  SVOBSR
      SAVE                  SVOBSI
      SAVE                  SVFND3

      SAVE                  FIRST

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR4
      SAVE                  SVFREF
      SAVE                  SVFRCD


C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'EDTERM' )

C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )
         CALL ZZCTRUIN( SVCTR3 )
         CALL ZZCTRUIN( SVCTR4 )

         FIRST = .FALSE.

      END IF

C
C     Get the input frame code and frame info.
C
      CALL ZZNAMFRM ( SVCTR4, SVFREF, SVFRCD, FIXREF, FRCODE )

      IF ( FRCODE .EQ. 0 ) THEN
         
         CALL SETMSG ( 'Input frame # has no associated frame '
     .   //            'ID code.'                               )
         CALL ERRCH  ( '#',  FIXREF                             )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                   )
         CALL CHKOUT ( 'EDTERM'                                 )
         RETURN

      END IF

      CALL FRINFO ( FRCODE, CENTER, FRCLAS, CLSSID, FOUND )

      IF ( .NOT. FOUND ) THEN
         
         CALL SETMSG ( 'Input frame # has associated frame '
     .   //            'ID code #, but no info was found '
     .   //            'by FRINFO for this frame.'              )
         CALL ERRCH  ( '#',  FIXREF                             )
         CALL ERRINT ( '#',  FRCODE                             )
         CALL SIGERR ( 'SPICE(BUG)'                             )
         CALL CHKOUT ( 'EDTERM'                                 )
         RETURN

      END IF

C
C     Get the ID codes of the target, source, and observer.
C
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTGID, SVFND1,
     .                TARGET, TRGID, FOUND    )

      IF ( .NOT. FOUND ) THEN
         
         CALL SETMSG ( 'Input target # has no associated body '
     .   //            'ID code.'                               )
         CALL ERRCH  ( '#',  TARGET                             )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                   )
         CALL CHKOUT ( 'EDTERM'                                 )
         RETURN

      END IF

      CALL ZZBODS2C ( SVCTR2, SVSCRE, SVSRCI, SVFND2,
     .                SOURCE, SRCID, FOUND    )

      IF ( .NOT. FOUND ) THEN
         
         CALL SETMSG ( 'Input source # has no associated body '
     .   //            'ID code.'                               )
         CALL ERRCH  ( '#',  SOURCE                             )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                   )
         CALL CHKOUT ( 'EDTERM'                                 )
         RETURN

      END IF

      CALL ZZBODS2C ( SVCTR3, SVOBSR, SVOBSI, SVFND3,
     .                OBSRVR, OBSID, FOUND    )

      IF ( .NOT. FOUND ) THEN
         
         CALL SETMSG ( 'Input observer # has no associated body '
     .   //            'ID code.'                               )
         CALL ERRCH  ( '#',  OBSRVR                             )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                   )
         CALL CHKOUT ( 'EDTERM'                                 )
         RETURN

      END IF

C
C     If the frame is not centered on the target, reject it.
C
      IF ( CENTER .NE. TRGID ) THEN

         CALL SETMSG ( 'Input frame # is not centered on target '
     .   //            'body #. This frame must be a body-fixed '
     .   //            'frame associated with the target.'       )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL ERRCH  ( '#',  TARGET                              )
         CALL SIGERR ( 'SPICE(INVALIDFIXREF)'                    )
         CALL CHKOUT ( 'EDTERM'                                  )
         RETURN

      END IF
 
C
C     Look up the radii associated with the target body.
C
      CALL BODVCD ( TRGID, 'RADII', 3, N, TRGRAD )

      IF ( N .NE. 3 ) THEN

         CALL SETMSG ( 'Three radii are required for the target '
     .   //            'body''s (#) shape model, but # were found.' )
         CALL ERRCH  ( '#',  TARGET                                 )
         CALL ERRINT ( '#',  N                                      )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
         CALL CHKOUT ( 'EDTERM'                                     )
         RETURN

      END IF

C
C     Look up the radii associated with the light source.
C
      CALL BODVCD ( SRCID, 'RADII', 3, N, SRCRAD )

      IF ( N .NE. 3 ) THEN

         CALL SETMSG ( 'Three radii are required for the light '
     .   //            'source''s (#) shape model, but # were found.' )
         CALL ERRCH  ( '#',  SOURCE                                   )
         CALL ERRINT ( '#',  N                                        )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                          )
         CALL CHKOUT ( 'EDTERM'                                       )
         RETURN

      END IF

      R = MAX ( SRCRAD(1), SRCRAD(2), SRCRAD(3) )

C
C     Look up the observer-target vector and the target-source vector.
C     Also set the output OBSPOS.
C
      CALL SPKEZP   ( TRGID, ET,     FIXREF, ABCORR,  
     .                OBSID, TRGPOS, LTTARG         )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'EDTERM' )
         RETURN
      END IF

      CALL ZZCOREPC ( ABCORR, ET,    LTTARG, TRGEPC )

      CALL VMINUS   ( TRGPOS, OBSPOS )

      CALL SPKEZP   ( SRCID, TRGEPC, FIXREF, ABCORR, 
     .                TRGID, SRCPOS, LTSRC          )

C
C     We're ready to compute the terminator.
C
      CALL ZZEDTERM ( TRMTYP, TRGRAD(1), TRGRAD(2), TRGRAD(3),  
     .                R,      SRCPOS,    NPTS,      TRMPTS     )    


      CALL CHKOUT ( 'EDTERM' )
      RETURN
      END
