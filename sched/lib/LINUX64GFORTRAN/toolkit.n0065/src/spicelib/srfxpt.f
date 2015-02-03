C$Procedure      SRFXPT ( Surface intercept point )
 
      SUBROUTINE SRFXPT ( METHOD,  TARGET,  ET,      ABCORR, 
     .                    OBSRVR,  DREF,    DVEC,    SPOINT, 
     .                    DIST,    TRGEPC,  OBSPOS,  FOUND )
      
C$ Abstract
C
C     Deprecated: This routine has been superseded by the SPICELIB
C     routine SINCPT. This routine is supported for purposes of
C     backward compatibility only.
C
C     Given an observer and a direction vector defining a ray, compute
C     the surface intercept point of the ray on a target body at a
C     specified epoch, optionally corrected for light time and stellar
C     aberration.
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
C     NAIF_IDS
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE              'frmtyp.inc'
      INCLUDE               'zzctr.inc'
      
      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      OBSPOS ( 3 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body.
C     ET         I   Epoch in ephemeris seconds past J2000 TDB.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Name of observing body.
C     DREF       I   Reference frame of input direction vector.
C     DVEC       I   Ray's direction vector.
C     SPOINT     O   Surface intercept point on the target body.
C     DIST       O   Distance from the observer to the intercept point.
C     TRGEPC     O   Intercept epoch.
C     OBSPOS     O   Observer position relative to target center.
C     FOUND      O   Flag indicating whether intercept was found.
C
C$ Detailed_Input
C
C     METHOD      is a short string providing parameters defining
C                 the computation method to be used.  Parameters
C                 include, but are not limited to, the shape model
C                 used to represent the surface of the target body.
C
C                 The only choice currently supported is
C
C                    'Ellipsoid'        The intercept computation uses
C                                       a triaxial ellipsoid to model
C                                       the surface of the target body.
C                                       The ellipsoid's radii must be
C                                       available in the kernel pool.
C
C                 Neither case nor white space are significant in 
C                 METHOD.  For example, the string ' eLLipsoid ' is 
C                 valid.                 
C
C                 In a later Toolkit release, this argument will be
C                 used to invoke a wider range of surface
C                 representations. For example, it will be possible to
C                 represent the target body's surface using a digital
C                 model.
C
C                 
C     TARGET      is the name of the target body.  TARGET is
C                 case-insensitive, and leading and trailing blanks in
C                 TARGET are not significant. Optionally, you may
C                 supply a string containing the integer ID code 
C                 for the object.  For example both 'MOON' and '301'
C                 are legitimate strings that indicate the moon is the
C                 target body.
C
C                 When the target body's surface is represented by a
C                 tri-axial ellipsoid, this routine assumes that a
C                 kernel variable representing the ellipsoid's radii is
C                 present in the kernel pool.  Normally the kernel
C                 variable would be defined by loading a PCK file.
C
C
C     ET          is the epoch of participation of the observer,
C                 expressed as ephemeris seconds past J2000 TDB: ET is
C                 the epoch at which the observer's state is computed.
C
C                 When aberration corrections are not used, ET is also
C                 the epoch at which the state and orientation of the
C                 target body are computed.
C
C                 When aberration corrections are used, ET is the epoch
C                 at which the observer's state relative to the solar
C                 system barycenter is computed; in this case the
C                 position and orientation of the target body are
C                 computed at ET-LT or ET+LT, where LT is the one-way
C                 light time between the intercept point and the
C                 observer, and the sign applied to LT depends on the
C                 selected correction. See the description of ABCORR
C                 below for details.
C
C
C     ABCORR      indicates the aberration correction to be applied
C                 when computing the observer-target state and the
C                 orientation of the target body.  ABCORR may be any of
C                 the following.
C
C                    'NONE'     Apply no correction. Return the 
C                               geometric surface intercept point on the
C                               target body.
C
C                 Let LT represent the one-way light time between the
C                 observer and the surface intercept point (note: NOT
C                 between the observer and the target body's center).
C                 The following values of ABCORR apply to the
C                 "reception" case in which photons depart from the
C                 intercept point's location at the light-time
C                 corrected epoch ET-LT and *arrive* at the observer's
C                 location at ET:
C
C
C                    'LT'       Correct for one-way light time (also
C                               called "planetary aberration") using a
C                               Newtonian formulation. This correction
C                               yields the location of the surface
C                               intercept point at the moment it
C                               emitted photons arriving at the
C                               observer at ET.
C 
C                               The light time correction uses an
C                               iterative solution of the light time
C                               equation. The solution invoked by the
C                               'LT' option uses one iteration.
C
C                               Both the target state as seen by the
C                               observer, and rotation of the target
C                               body, are corrected for light time.
C
C                    'LT+S'     Correct for one-way light time and
C                               stellar aberration using a Newtonian
C                               formulation. This option modifies the
C                               state obtained with the 'LT' option to
C                               account for the observer's velocity
C                               relative to the solar system
C                               barycenter. The result is the apparent
C                               surface intercept point as seen by the
C                               observer.
C
C                    'CN'       Converged Newtonian light time
C                               correction.  In solving the light time
C                               equation, the 'CN' correction iterates
C                               until the solution converges. Both the
C                               state and rotation of the target body
C                               are corrected for light time.
C
C                    'CN+S'     Converged Newtonian light time
C                               and stellar aberration corrections.
C
C                 The following values of ABCORR apply to the
C                 "transmission" case in which photons *depart* from
C                 the observer's location at ET and arrive at the
C                 intercept point at the light-time corrected epoch
C                 ET+LT:
C
C
C                    'XLT'      "Transmission" case:  correct for
C                               one-way light time using a Newtonian
C                               formulation. This correction yields the
C                               intercept location at the moment it
C                               receives photons emitted from the
C                               observer's location at ET. 
C
C                               The light time correction uses an
C                               iterative solution of the light time
C                               equation. The solution invoked by the
C                               'LT' option uses one iteration.
C
C                               Both the target state as seen by the
C                               observer, and rotation of the target
C                               body, are corrected for light time.
C
C                    'XLT+S'    "Transmission" case:  correct for
C                               one-way light time and stellar
C                               aberration using a Newtonian
C                               formulation  This option modifies the
C                               intercept obtained with the 'XLT'
C                               option to account for the observer's
C                               velocity relative to the solar system
C                               barycenter.
C
C                    'XCN'      Converged Newtonian light time
C                               correction.  This is the same as XLT
C                               correction but with further iterations
C                               to a converged Newtonian light time
C                               solution. 
C
C                    'XCN+S'    "Transmission" case:  converged 
C                               Newtonian light time and stellar 
C                               aberration corrections.
C
C
C     OBSRVR      is the name of the observing body.  This is typically
C                 a spacecraft, the earth, or a surface point on the
C                 earth.  OBSRVR is case-insensitive, and leading and
C                 trailing blanks in OBSRVR are not significant.
C                 Optionally, you may supply a string containing the
C                 integer ID code for the object.  For example both
C                 'MOON' and '301' are legitimate strings that indicate
C                 the moon is the observer.
C
C
C     DREF        is the name of the reference frame relative to which
C                 the input direction vector is expressed. This may be
C                 any frame supported by the SPICE system, including
C                 built-in frames (documented in the Frames Required
C                 Reading) and frames defined by a loaded frame kernel
C                 (FK).
C
C                 When DREF designates a non-inertial frame, the
C                 orientation of the frame is evaluated at an epoch
C                 dependent on the frame's center and, if the center is
C                 not the observer, on the selected aberration
C                 correction. See the description of the direction
C                 vector DVEC for details.
C
C
C     DVEC        Pointing vector emanating from the observer.  The
C                 intercept with the target body's surface of the ray
C                 defined by the observer and DVEC is sought.
C
C                 DVEC is specified relative to the reference frame
C                 designated by DREF.
C
C                 Non-inertial reference frames are treated as follows:
C                 if the center of the frame is at the observer's
C                 location, the frame is evaluated at ET.  If the
C                 frame's center is located elsewhere, then letting
C                 LTCENT be the one-way light time between the observer
C                 and the central body associated with the frame, the
C                 orientation of the frame is evaluated at ET-LTCENT,
C                 ET+LTCENT, or ET depending on whether the requested
C                 aberration correction is, respectively, for received
C                 radiation, transmitted radiation, or is omitted.
C                 LTCENT is computed using the method indicated by
C                 ABCORR.
C
C$ Detailed_Output
C
C
C     SPOINT      is the surface intercept point on the target body of
C                 the ray defined by the observer and the direction
C                 vector. If the ray intersects the target body in
C                 multiple points, the selected intersection point is
C                 the one closest to the observer.  The output
C                 argument FOUND (see below) indicates whether an
C                 intercept was found.
C
C                 SPOINT is expressed in Cartesian coordinates,
C                 relative to the body-fixed frame associated with the
C                 target body.  The body-fixed target frame is 
C                 evaluated at the intercept epoch TRGEPC (see
C                 description below).
C
C                 When light time correction is used, the duration of
C                 light travel between SPOINT to the observer is
C                 considered to be the one way light time.  When both
C                 light time and stellar aberration corrections are
C                 used, SPOINT is selected such that, when SPOINT is
C                 corrected for light time and the vector from the
C                 observer to the light-time corrected location of
C                 SPOINT is corrected for stellar aberration, the
C                 resulting vector is parallel to the ray defined by
C                 the observer's location and DVEC.
C
C                 The components of SPOINT are given in units of km.
C
C
C     DIST        is the distance between the observer and the surface
C                 intercept on the target body.  DIST is given in 
C                 units of km.
C
C
C     TRGEPC      is the "intercept epoch."  This is the epoch at which
C                 the ray defined by OBSRVR and DVEC intercepts the
C                 target surface at SPOINT.  TRGEPC is defined as
C                 follows: letting LT be the one-way light time between
C                 the observer and the intercept point, TRGEPC is the
C                 epoch ET-LT, ET+LT, or ET depending on whether the
C                 requested aberration correction is, respectively, for
C                 received radiation, transmitted radiation, or
C                 omitted. LT is computed using the method indicated by
C                 ABCORR.
C
C                 TRGEPC is expressed as seconds past J2000 TDB.
C
C
C     OBSPOS      is the vector from the center of the target body at
C                 epoch TRGEPC to the observer at epoch ET.  OBSPOS is
C                 expressed in the target body-fixed reference frame
C                 evaluated at TRGEPC.  (This is the frame relative to
C                 which SPOINT is given.)
C
C                 OBSPOS is returned to simplify various related
C                 computations that would otherwise be cumbersome.  For
C                 example, the vector XVEC from the observer to SPOINT
C                 can be calculated via the call
C
C                    CALL VSUB ( SPOINT, OBSPOS, XVEC )
C
C                 The components of OBSPOS are given in units of km.
C
C
C     FOUND       A logical flag indicating whether or not the ray
C                 intersects the target.  If an intersection exists
C                 FOUND will be returned as .TRUE.  If the ray misses
C                 the target, FOUND will be returned as .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If any of the listed errors occur, the output arguments are 
C     left unchanged.
C 
C
C     1)  If the input argument METHOD is not recognized, the error
C         will be signaled by a routine in the call tree of this
C         routine.
C
C     2)  If TARGET cannot be mapped to an ID code, the error
C         SPICE(IDCODENOTFOUND) will be signaled. If OBSRVR 
C         cannot be mapped to an ID code, the error will be 
C         signaled by a routine in the call tree of this
C         routine.
C
C     3)  If the input argument ABCORR is invalid, the error
C         will be signaled by a routine in the call tree of this
C         routine.
C
C     4)  If a body-fixed reference frame associated with the
C         target cannot be found, the error SPICE(NOFRAME) will
C         be signaled.
C
C     5)  If OBSRVR and TARGET map to the same NAIF integer ID codes,
C         the error will be signaled by a routine in the call tree of
C         this routine.
C
C     6)  If frame definition data enabling the evaluation of the state 
C         of the target relative to the observer in target body-fixed
C         coordinates have not been loaded prior to calling SRFXPT, the
C         error will be  signaled by a routine in the call tree of this
C         routine.
C
C     7)  If the specified aberration correction is not recognized, the
C         error will be signaled by a routine in the call tree of this
C         routine.
C
C     8)  If insufficient ephemeris data have been loaded prior to
C         calling SRFXPT, the error will be diagnosed and signaled by a
C         routine in the call tree of this routine.  Note that when
C         light time correction is used, sufficient ephemeris data
C         must be available to propagate the states of both observer
C         and target to the solar system barycenter.
C
C     9)  If the computation method has been specified as "Ellipsoid"
C         and triaxial radii of the target body have not been loaded
C         into the kernel pool prior to calling SRFXPT, the error will
C         be signaled by a routine in the call tree of this routine.
C
C     10) The target must be an extended body:  if any of the radii of 
C         the target body are non-positive, the error will be signaled
C         by routines in the call tree of this routine.
C 
C     11) If PCK data needed to define the target body-fixed frame
C         have not been loaded prior to calling SRFXPT, the error will
C         be signaled by a routine in the call tree of this routine.
C
C     12) If the reference frame designated by DREF is not recognized
C         by the SPICE frame subsystem, the error will be signaled
C         by a routine in the call tree of this routine.
C
C     13) If the direction vector DVEC is the zero vector, the error
C         is signaled  by a routine in the call tree of this routine.
C         
C
C$ Files
C
C     Appropriate SPK, PCK, and frame kernels must be loaded by the
C     calling program before this routine is called.  CK, SCLK, and
C     IK kernels may be required as well. 
C
C     The following data are required:
C
C        - SPK data:  ephemeris data for target and observer must be
C          loaded.  If aberration corrections are used, the states of
C          target and observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C          Typically ephemeris data are made available by loading one
C          or more SPK files via FURNSH.
C
C        - PCK data:  if the computation method is specified as
C          "Ellipsoid," triaxial radii for the target body must be 
C          loaded into the kernel pool.  Typically this is done by
C          loading a text PCK file via FURNSH.
C
C        - Further PCK data:  rotation data for the target body must
C          be loaded.  These may be provided in a text or binary PCK
C          file. 
C
C        - Frame data:  if a frame definition is required to convert
C          the observer and target states to the body-fixed frame of
C          the target, that definition must be available in the kernel
C          pool. Similarly, the frame definition required to map
C          between the frame designated by DREF and the target
C          body-fixed frame must be available. Typically the
C          definitions of frames not already built-in to SPICE are
C          supplied by loading a frame kernel.
C
C     The following data may be required:
C
C        - CK data:  if the frame to which DREF refers is fixed to a
C          spacecraft instrument or structure, at least one CK file
C          will be needed to permit transformation of vectors between
C          that frame and both J2000 and the target body-fixed frame.
C
C        - SCLK data:  if a CK file is needed, an associated SCLK
C          kernel is required to enable conversion between encoded SCLK
C          (used to time-tag CK data) and barycentric dynamical time
C          (TDB).
C
C        - IK data:  one or more I-kernels may be required to enable
C          transformation of vectors from an instrument-fixed frame to
C          a spacecraft-fixed frame whose attitude is given by a
C          C-kernel.
C
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     Given a ray defined by a direction vector and the location of an
C     observer, SRFXPT computes the surface intercept point of the ray
C     on a specified target body. SRFXPT also determines the distance
C     between the observer and the surface intercept point.
C
C     When aberration corrections are used, this routine finds the
C     value of SPOINT such that, if SPOINT is regarded as an ephemeris
C     object, after the selected aberration corrections are applied to
C     the vector from the observer to SPOINT, the resulting vector is
C     parallel to the direction vector DVEC.
C
C     This routine computes light time corrections using light time
C     between the observer and the surface intercept point, as opposed
C     to the center of the target.  Similarly, stellar aberration
C     corrections done by this routine are based on the direction of
C     the vector from the observer to the light-time corrected
C     intercept point, not to the target center.  This technique avoids
C     errors due to the differential between aberration corrections
C     across the target body. Therefore it's valid to use aberration
C     corrections with this routine even when the observer is very
C     close to the intercept point, in particular when the
C     observer-intercept point distance is much less than the
C     observer-target center distance.  It's also valid to use stellar
C     aberration corrections even when the intercept point is near or
C     on the limb (as may occur in occultation computations using a
C     point target).
C
C     When comparing surface intercept point computations with results
C     from sources other than SPICE, it's essential to make sure the
C     same geometric definitions are used.
C     
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms.  The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C
C     Example 1
C     ---------
C
C     The following program computes surface intercept points on Mars
C     for the boresight and FOV boundary vectors of the MGS MOC narrow
C     angle camera.  The intercepts are computed for a single
C     observation epoch.  Light time and stellar aberration corrections
C     are used.  For simplicity, camera distortion is ignored.
C
C
C           PROGRAM MOCXPT
C           IMPLICIT NONE
C
C     C
C     C     Local parameters
C     C
C           INTEGER               ABCLEN
C           PARAMETER           ( ABCLEN = 20 )
C
C           INTEGER               LNSIZE
C           PARAMETER           ( LNSIZE = 78 )
C
C           INTEGER               METLEN
C           PARAMETER           ( METLEN = 40 )
C
C           INTEGER               NAMLEN
C           PARAMETER           ( NAMLEN = 32 )
C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 50 )
C
C           INTEGER               SHPLEN
C           PARAMETER           ( SHPLEN = 80 )
C
C           INTEGER               NCORNR
C           PARAMETER           ( NCORNR = 4 )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(ABCLEN)    ABCORR
C           CHARACTER*(NAMLEN)    CAMERA
C           CHARACTER*(NAMLEN)    DREF
C           CHARACTER*(METLEN)    METHOD
C           CHARACTER*(NAMLEN)    OBSRVR
C           CHARACTER*(NAMLEN)    SHAPE
C           CHARACTER*(NAMLEN)    TARGET
C           CHARACTER*(LNSIZE)    TITLE
C           CHARACTER*(TIMLEN)    UTC
C
C           DOUBLE PRECISION      BOUNDS ( 3, NCORNR )
C           DOUBLE PRECISION      BSIGHT ( 3 )
C           DOUBLE PRECISION      DIST
C           DOUBLE PRECISION      DPR
C           DOUBLE PRECISION      DVEC   ( 3 )
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      LAT
C           DOUBLE PRECISION      LON
C           DOUBLE PRECISION      OBSPOS ( 3 )
C           DOUBLE PRECISION      RADIUS
C           DOUBLE PRECISION      SPOINT ( 3 )
C           DOUBLE PRECISION      TRGEPC
C
C           INTEGER               CAMID
C           INTEGER               I
C           INTEGER               J
C           INTEGER               N
C
C           LOGICAL               FOUND
C
C           DATA                  ABCORR / 'LT+S'      /
C           DATA                  CAMERA / 'MGS_MOC_NA'/
C           DATA                  METHOD / 'Ellipsoid' /
C           DATA                  OBSRVR / 'MGS'       /
C           DATA                  TARGET / 'Mars'      /
C           DATA                  UTC    / '2003 OCT 13 06:00:00 UTC' /
C
C     C
C     C     Load kernel files:
C     C
C     C        - Leapseconds kernel
C     C        - MGS SCLK kernel
C     C        - Text PCK file
C     C        - Planetary SPK file
C     C        - MGS I-kernel
C     C        - MGS spacecraft bus C-kernel
C     C        - MGS SPK file
C     C
C           CALL FURNSH ( 'naif0007.tls'           )
C           CALL FURNSH ( 'mgs_sclkscet_00052.tsc' )
C           CALL FURNSH ( 'mars_iau2000_v0.tpc'    )
C           CALL FURNSH ( 'de405s.bsp'             )
C           CALL FURNSH ( 'mgs_moc_v20.ti'         )
C           CALL FURNSH ( 'mgs_ext12.bsp'          )
C           CALL FURNSH ( 'mgs_sc_ext12.bc'        )
C
C     C
C     C     Convert the UTC request time to ET (seconds past
C     C     J2000, TDB).
C     C
C           CALL STR2ET ( UTC, ET )
C
C     C
C     C     Get the MGS MOC Narrow angle camera (MGS_MOC_NA)
C     C     ID code.  Then look up the field of view (FOV)
C     C     parameters by calling GETFOV.
C     C
C           CALL BODN2C ( CAMERA, CAMID, FOUND )
C
C           IF ( .NOT. FOUND ) THEN
C              CALL SETMSG ( 'Could not find ID code for ' //
C          .                 'instrument #.'               )
C              CALL ERRCH  ( '#', CAMERA                   )
C              CALL SIGERR ( 'SPICE(NOTRANSLATION)'        )
C           END IF
C
C     C
C     C     GETFOV will return the name of the camera-fixed frame
C     C     in the string DREF, the camera boresight vector in
C     C     the array BSIGHT, and the FOV corner vectors in the
C     C     array BOUNDS.
C     C
C           CALL GETFOV ( CAMID,  NCORNR, SHAPE,  DREF,
C          .              BSIGHT, N,      BOUNDS       )
C
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Surface Intercept Locations for Camera'
C           WRITE (*,*) 'FOV Boundary and Boresight Vectors'
C           WRITE (*,*) ' '
C           WRITE (*,*) '   Instrument:            ', CAMERA
C           WRITE (*,*) '   Epoch:                 ', UTC
C           WRITE (*,*) '   Aberration correction: ', ABCORR
C           WRITE (*,*) ' '
C
C     C
C     C     Now compute and display the surface intercepts for the
C     C     boresight and all of the FOV boundary vectors.
C     C
C           DO I = 1, NCORNR+1
C
C              IF ( I .LE. NCORNR ) THEN
C
C                 TITLE = 'Corner vector #'
C                 CALL REPMI ( TITLE, '#', I, TITLE )
C
C                 CALL VEQU ( BOUNDS(1,I), DVEC )
C
C              ELSE
C
C                 TITLE = 'Boresight vector'
C                 CALL VEQU ( BSIGHT, DVEC )
C
C              END IF
C
C     C
C     C        Compute the surface intercept point using
C     C        the specified aberration corrections.  
C     C 
C     C        SRFXPT will signal an error if required kernel 
C     C        data are unavailable.  See example (2) below for 
C     C        a suggestion on detecting absence of C-kernel
C     C        data prior to calling SRFXPT.
C     C        
C              CALL SRFXPT ( METHOD, TARGET, ET,     ABCORR, 
C          .                 OBSRVR, DREF,   DVEC,   SPOINT, 
C          .                 DIST,   TRGEPC, OBSPOS, FOUND  )
C
C              IF ( FOUND ) THEN
C     C
C     C           Convert rectangular coordinates to planetocentric
C     C           latitude and longitude.  Convert radians to degrees.
C     C
C                 CALL RECLAT ( SPOINT, RADIUS, LON, LAT )
C
C                 LON = LON * DPR ()
C                 LAT = LAT * DPR ()
C     C
C     C           Display the results.
C     C
C                 WRITE (*,*) ' '
C                 WRITE (*,*) TITLE
C
C                 TITLE = '  Vector in # frame = '
C                 CALL REPMC ( TITLE, '#', DREF, TITLE )
C
C                 WRITE (*,*) ' '
C                 WRITE (*,*) TITLE
C
C                 IF ( I .LE. NCORNR ) THEN
C                    WRITE (*,*) '  ', ( BOUNDS(J,I), J = 1, 3 )
C                 ELSE
C                    WRITE (*,*) '  ', BSIGHT
C                 END IF
C
C                 WRITE (*,*) ' '
C                 WRITE (*,*) '  Intercept:'
C                 WRITE (*,*)
C          .      '     Radius                   (km)  = ', RADIUS
C                 WRITE (*,*)
C          .      '     Planetocentric Latitude  (deg) = ', LAT
C                 WRITE (*,*)
C          .      '     Planetocentric Longitude (deg) = ', LON
C                 WRITE (*,*)
C          .      '     Range                    (km)  = ', DIST
C                 WRITE (*,*) ' '
C
C              ELSE
C
C                 WRITE (*,*) ' '
C                 WRITE (*,*) 'Intercept not found.'
C                 WRITE (*,*) ' '
C
C              END IF
C
C           END DO
C
C           END
C
C
C     When this program is executed, the output will be: 
C
C
C        Surface Intercept Locations for Camera
C        FOV Boundary and Boresight Vectors
C
C           Instrument:            MGS_MOC_NA
C           Epoch:                 2003 OCT 13 06:00:00 UTC
C           Aberration correction: LT+S
C
C
C        Corner vector 1
C
C          Vector in MGS_MOC_NA frame =
C            1.85713838E-06 -0.00380156227  0.999992774
C
C          Intercept:
C             Radius                   (km)  =   3384.94126
C             Planetocentric Latitude  (deg) =  -48.4771189
C             Planetocentric Longitude (deg) =  -123.473655
C             Range                    (km)  =   388.983627
C
C
C        Corner vector 2
C
C          Vector in MGS_MOC_NA frame =
C            1.85713838E-06  0.00380156227  0.999992774
C
C          Intercept:
C             Radius                   (km)  =   3384.93982
C             Planetocentric Latitude  (deg) =  -48.4812729
C             Planetocentric Longitude (deg) =  -123.398399
C             Range                    (km)  =   388.975659
C
C
C        Corner vector 3
C
C          Vector in MGS_MOC_NA frame =
C           -1.85713838E-06  0.00380156227  0.999992774
C
C          Intercept:
C             Radius                   (km)  =   3384.93982
C             Planetocentric Latitude  (deg) =  -48.4812985
C             Planetocentric Longitude (deg) =  -123.398403
C             Range                    (km)  =   388.9752
C
C
C        Corner vector 4
C
C          Vector in MGS_MOC_NA frame =
C           -1.85713838E-06 -0.00380156227  0.999992774
C
C          Intercept:
C             Radius                   (km)  =   3384.94125
C             Planetocentric Latitude  (deg) =  -48.4771444
C             Planetocentric Longitude (deg) =  -123.473658
C             Range                    (km)  =   388.983168
C
C
C        Boresight vector
C
C          Vector in MGS_MOC_NA frame =
C            0.  0.  1.
C
C          Intercept:
C             Radius                   (km)  =   3384.94054
C             Planetocentric Latitude  (deg) =  -48.4792166
C             Planetocentric Longitude (deg) =  -123.43603
C             Range                    (km)  =   388.976266
C
C
C
C     Example 2
C     ---------
C
C     SRFXPT will signal an error if required kernel data are
C     unavailable:  for example, in the program of Example 1, if the
C     C-kernel containing data for the MGS bus had a gap at epoch ET,
C     SRFXPT would be unable to transform the direction vector DVEC
C     from the reference frame fixed to the camera to the reference
C     frame fixed to the target body.
C
C     We could modify the code of Example 1 as shown below to test for
C     the availability of C-kernel data.  We would add the declarations
C     shown, and we'd call the C-kernel reader CKGP to find whether the
C     desired pointing was available.  Depending on the value of the
C     FOUND flag returned by CKGP, we'd go on to compute the surface
C     intercept point or respond to the error condition.
C
C
C                        .
C                        .
C                        .
C
C     C
C     C     Local parameters
C     C
C           INTEGER               BUSID
C           PARAMETER           ( BUSID = -94000 )
C
C           INTEGER               MGS
C           PARAMETER           ( MGS    = -94 )
C                        .
C                        .
C                        .
C
C     C
C     C     Local variables
C     C
C
C           DOUBLE PRECISION      CLKOUT
C           DOUBLE PRECISION      CMAT   ( 3, 3 )
C           DOUBLE PRECISION      SCLKDP
C
C                        .
C                        .
C                        .
C
C     C
C     C     Look up the transformation from the J2000 frame to the
C     C     MGS spacecraft frame.  To do this, we'll need to represent
C     C     our observation epoch in terms of MGS encoded SCLK.
C     C
C           CALL SCE2C ( MGS, ET, SCLKDP )
C
C     C
C     C     Look up the spacecraft attitude from the C-kernel.
C     C
C           CALL CKGP ( BUSID, SCLKDP, 0.D0, 'J2000',
C          .            CMAT,  CLKOUT, FOUND         )
C
C           IF ( FOUND ) THEN
C
C              [Proceed to compute intercept point]
C
C           ELSE
C
C              [Handle case where pointing is unavailable
C               for the epoch of interest]
C
C           END IF
C                        .
C                        .
C                        .
C
C$ Restrictions
C
C     A cautionary note:  if aberration corrections are used, and 
C     if DREF is the target body-fixed frame, the epoch at which that
C     frame is evaluated is offset from ET by the light time between
C     the observer and the *center* of the target body. This light time
C     normally will differ from the light time between the observer and
C     intercept point.  Consequently the orientation of the target
C     body-fixed frame at TRGEPC will not match that of the target
C     body-fixed frame at the epoch associated with DREF.  As a result,
C     various derived quantities may not be as expected:  for example,
C     OBSPOS would not be the inverse of the aberration-corrected
C     position of the target as seen by the observer.
C
C     In many applications the errors arising from this frame
C     discrepancy may be insignificant; however a safe approach is to
C     always use as DREF a frame other than the target body-fixed
C     frame.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.5.0, 31-MAR-2014 (BVS)
C
C        Updated to save the input body names and ZZBODTRN state
C        counters and to do name-ID conversions only if the counters
C        have changed.
C
C        Updated to save the input frame name and POOL state counter
C        and to do frame name-ID conversion only if the counter has
C        changed.
C
C-    SPICELIB Version 1.4.1, 18-MAY-2010 (BVS) 
C
C        Index line now states that this routine is deprecated.
C
C-    SPICELIB Version 1.4.0, 23-MAR-2009 (NJB)
C
C        Bug fix: quick test for non-intersection is
C        no longer performed when observer-target distance
C        is less than target's maximum radius.
C
C        Typo correction in Required_Reading: changed FRAME 
C        to FRAMES.
C
C-    SPICELIB Version 1.3.0, 15-FEB-2008 (NJB) 
C
C        Bug fix: near-miss case light time improvement
C        logic is no longer applied when a geometric 
C        solution is requested via ABCORR.
C
C        References to unneeded variables FJ2000 and FIRST
c        were deleted.
C
C        Header typo was corrected; reference to VMINUS was replaced
C        with reference to VSUB.
C
C        Abstract now states that this routine is deprecated.
C
C-    SPICELIB Version 1.2.1, 25-APR-2007 (NJB) 
C
C        Header typo was corrected; reference to VMINUS was replaced
C        with reference to VSUB.
C
C-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB) 
C
C        Call to BODVAR was replaced with call to BODVCD.
C
C-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB) 
C
C        Updated to use BODS2C.
C
C-    SPICELIB Version 1.0.0, 27-FEB-2004 (NJB) 
C
C-&
 
C$ Index_Entries
C
C     DEPRECATED surface intercept point
C
C-&
 

C$ Revisions
C
C-    SPICELIB Version 1.3.0, 30-JAN-2008 (NJB) 
C
C        Bug fix: near-miss case light time improvement
C        logic is no longer applied when a geometric 
C        solution is requested via ABCORR.
C
C-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB) 
C
C        Updated to use BODS2C.  This simplifies the name-to-ID
C        mapping code.
C
C-&



 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP
      
      LOGICAL               EQCHR
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
C
C     This value will become system-dependent when systems
C     using 128-bit d.p. numbers are supported by SPICELIB.
C     CNVLIM, when added to 1.0D0, should yield 1.0D0. 
C
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-17 )
     
C
C     Round-off error limit for arc sine input:
C
      DOUBLE PRECISION      RNDTOL
      PARAMETER           ( RNDTOL = 1.D-14 )

C
C     Fraction of planetary angular radius used to define
C     region outside of which rays are immediately rejected
C     as non-intersecting.
C
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.001D0 )


      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN =  15 )

      INTEGER               MAXITR
      PARAMETER           ( MAXITR =  10 )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )


C
C     Local variables
C
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(ABCLEN)    LOCCOR

      DOUBLE PRECISION      ETDIFF
      DOUBLE PRECISION      J2DIR  ( 3 )
      DOUBLE PRECISION      J2EST  ( 3 )
      DOUBLE PRECISION      J2POS  ( 3 )
      DOUBLE PRECISION      J2TMAT ( 3, 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTCENT
      DOUBLE PRECISION      LTDIFF
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      NEGPOS ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      PREVET
      DOUBLE PRECISION      PREVLT
      DOUBLE PRECISION      R2JMAT ( 3, 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RAYALT
      DOUBLE PRECISION      REFEPC
      DOUBLE PRECISION      REJECT
      DOUBLE PRECISION      RPOS   ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SSBOST ( 6 )
      DOUBLE PRECISION      SSBTST ( 6 )
      DOUBLE PRECISION      STLDIR ( 3 )
      DOUBLE PRECISION      STLERR ( 3 )
      DOUBLE PRECISION      STLTMP ( 3 )
      DOUBLE PRECISION      TPOS   ( 3 )
      DOUBLE PRECISION      TRGDIR ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               CENTER
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               NITR
      INTEGER               NRADII
      INTEGER               OBSCDE
      INTEGER               REFCDE
      INTEGER               TRGCDE
      INTEGER               TYPE
      INTEGER               TYPEID
      
      LOGICAL               FND
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTCDE
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSR
      INTEGER               SVOBSC
      LOGICAL               SVFND2

      LOGICAL               FIRST

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVDREF
      INTEGER               SVREFC


C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTCDE
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVOBSR
      SAVE                  SVOBSC
      SAVE                  SVFND2

      SAVE                  FIRST

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR3
      SAVE                  SVDREF
      SAVE                  SVREFC


C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SRFXPT' )

C
C     Nothing has been found yet.
C
      FOUND = .FALSE.

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

         FIRST = .FALSE.

      END IF
 
C
C     Obtain integer codes for the target and observer.
C 
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTCDE, SVFND1,
     .                TARGET, TRGCDE, FND    )
            
      IF ( .NOT. FND ) THEN
      
         CALL SETMSG ( 'The target, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SRFXPT'                                   )
         RETURN
      
      END IF
      
      
      CALL ZZBODS2C ( SVCTR2, SVOBSR, SVOBSC, SVFND2,
     .                OBSRVR, OBSCDE, FND    )
      
      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SRFXPT'                                   )
         RETURN
      
      END IF
      
 
C
C     Check the input body codes.  If they are equal, signal
C     an error.
C
      IF ( OBSCDE .EQ. TRGCDE ) THEN
 
         CALL SETMSG ( 'In computing the surface intercept point, ' //
     .                 'the observing body and target body are the '//
     .                 'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( 'SRFXPT'                                     )
         RETURN
 
      END IF
 
C
C     Get a left-justified, upper case copy of the aberration
C     correction flag.
C
      CALL LJUST ( ABCORR, LOCCOR )
      CALL UCASE ( LOCCOR, LOCCOR )

C
C     Check for stellar aberration in the aberration correction flag.
C     
      USESTL  =  INDEX( LOCCOR, '+S') .GT. 0 

C
C     Now remove the stellar aberration component from the aberration
C     correction flag; we'll do our state lookups without stellar
C     aberration correction.
C
      CALL REPMC ( LOCCOR, '+S', ' ', LOCCOR )

C
C     Decide whether the aberration correction is for received or
C     transmitted radiation.
C
      XMIT = EQCHR ( LOCCOR(1:1),  'X' ) 

C
C     Decide what sort of light time correction has been requested.
C
      USECN = ( LOCCOR(1:2) .EQ. 'CN' ) .OR. ( LOCCOR(1:3) .EQ. 'XCN' ) 
       
      USELT =        USECN
     .        .OR. ( LOCCOR(1:2) .EQ. 'LT'  )
     .        .OR. ( LOCCOR(1:3) .EQ. 'XLT' )
         
C
C     Get the sign S prefixing LT in the expression for TRGEPC.
C     When light time correction is not used, setting S = 0
C     allows us to seamlessly set TRGEPC equal to ET.
C
      IF ( USELT ) THEN

         IF ( XMIT ) THEN
            S   =  1.D0
         ELSE
            S   = -1.D0
         END IF
         
      ELSE
         S = 0.D0
      END IF

C
C     Find the name of the body-fixed frame associated with the
C     target body.  We'll want the state of the target relative to
C     the observer in this body-fixed frame.
C
      CALL CIDFRM ( TRGCDE, FRCODE, FRNAME, FND )

      IF ( .NOT. FND ) THEN
      
         CALL SETMSG ( 'No body-fixed frame is associated with '   //
     .                 'target body #; a frame kernel must be '    //
     .                 'loaded to make this association.  Consult '//
     .                 'the FRAMES Required Reading for details.'   )
         CALL ERRCH  ( '#', TARGET                                  )
         CALL SIGERR ( 'SPICE(NOFRAME)'                             )
         CALL CHKOUT ( 'SRFXPT'                                     )
         RETURN
 
      END IF
 
C
C     Determine the position of the observer in target
C     body-fixed coordinates.
C
C         -  Call SPKEZP to compute the position of the target body as
C            seen from the observing body and the light time (LT)
C            between them.  We request that the coordinates of POS be
C            returned relative to the body fixed reference frame
C            associated with the target body, using aberration
C            corrections specified by the input argument ABCORR.
C
C         -  Call VMINUS to negate the direction of the vector (OBSPOS)
C            so it will be the position of the observer as seen from
C            the target body in target body fixed coordinates.
C
C            Note that this result is not the same as the result of
C            calling SPKEZP with the target and observer switched.  We
C            computed the vector FROM the observer TO the target in
C            order to get the proper light time and stellar aberration
C            corrections (if requested).  Now we need the inverse of
C            that corrected vector in order to compute the intercept
C            point.
C

      CALL SPKEZP ( TRGCDE, ET, FRNAME, LOCCOR, OBSCDE, TPOS, LT )
 
C
C     Negate the target's position to obtain the position of the
C     observer relative to the target.
C
      CALL VMINUS ( TPOS, OBSPOS )

C
C     We now need to convert the direction vector into the
C     body fixed frame associated with the target.  The target
C     epoch is dependent on the aberration correction.  The
C     coefficient S has been set to give us the correct answer
C     for each case.
C
      TRGEPC = ET  +  S*LT

C
C     Determine the attributes of the frame designated by DREF.
C
      CALL ZZNAMFRM ( SVCTR3, SVDREF, SVREFC, DREF, REFCDE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SRFXPT' )
         RETURN
      END IF

      CALL FRINFO ( REFCDE, CENTER, TYPE, TYPEID, FND )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' //
     .                 'the SPICE frame subsystem.  Possibly '   //
     .                 'a required frame definition kernel has ' //
     .                 'not been loaded.'                        )
         CALL ERRCH  ( '#',  DREF                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
         CALL CHKOUT ( 'SRFXPT'                                  )
         RETURN

      END IF
 
C
C     Transform the direction vector from frame DREF to the body-fixed
C     frame associated with the target.  The epoch TRGEPC associated
C     with the body-fixed frame has been set already.
C     
C     We'll compute the transformation in two parts:  first
C     from frame DREF to J2000, then from J2000 to the target
C     frame.
C
      IF ( TYPE .EQ. INERTL ) THEN
C
C        Inertial frames can be evaluated at any epoch.
C
         REFEPC = ET


      ELSE IF ( .NOT. USELT ) THEN
C
C        We're not using light time corrections (converged or
C        otherwise), so there's no time offset.
C
         REFEPC = ET


      ELSE IF ( CENTER .EQ. OBSCDE ) THEN
C
C        If the center of frame DREF is the observer (which is
C        usually the case if the observer is a spacecraft), then
C        the epoch of frame DREF is simply ET.
C
C        There's no offset between the center for frame DREF
C        and the observer.
C
         REFEPC = ET

      ELSE
C
C        Find the light time from the observer to the center of 
C        frame DREF.
C           
         CALL SPKEZP ( CENTER, ET, 'J2000', LOCCOR, OBSCDE,
     .                 RPOS,   LTCENT                       )
  
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SRFXPT' )
            RETURN
         END IF

         REFEPC  =  ET  +  S * LTCENT

      END IF

C
C     The epoch REFEPC associated with frame DREF has been set.
C
C     Compute the transformation from frame DREF to J2000 and the
C     transformation from J2000 to the target body-fixed frame.
C
C     Map DVEC to both the J2000 and target body-fixed frames. We'll
C     store DVEC, expressed relative to the J2000 frame, in the
C     variable J2DIR.  DVEC in the target body-fixed frame will be
C     stored in TRGDIR.
C
C     We may need both versions of DVEC:  if we use light time
C     correction, we'll update "intercept epoch", and hence the
C     transformation between J2000 and the target body-fixed frame.
C     The transformation between DREF and J2000 doesn't change, on the
C     other hand, so we don't have to recompute J2DIR.  We need TRGDIR
C     in all cases.
C
      CALL PXFORM ( DREF, 'J2000', REFEPC,  R2JMAT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SRFXPT' )
         RETURN
      END IF

      CALL MXV ( R2JMAT, DVEC, J2DIR )

C
C     Map J2DIR (in the J2000 frame) to the target body-fixed
C     frame.
C
      CALL PXFORM ( 'J2000', FRNAME,  TRGEPC,  J2TMAT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SRFXPT' )
         RETURN
      END IF

      CALL MXV ( J2TMAT, J2DIR, TRGDIR )

C
C     At this point,
C
C        TRGEPC is set.
C        TRGDIR is set.
C        J2DIR is set.
C
C
C     Get the J2000-relative state of the observer relative to
C     the solar system barycenter at ET.  We'll use this in
C     several places later.
C
      CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOST )

C
C     If we're using stellar aberration correction, at this point we'll
C     account for it.  We're going to find a surface point such that
C     the radiation path from that point to the observer, after
C     correction for stellar aberration, is parallel to the ray. So
C     by applying the inverse of the correction to the ray, we obtain
C     the ray with which we must perform our intercept computation.
C 
      IF ( USESTL ) THEN
C
C        We approximate the inverse stellar aberration correction by
C        using the correction for the reverse transmission direction.
C        If we're in the reception case, we apply the transmission
C        stellar aberration correction to J2DIR and vice versa.
C
         IF ( XMIT ) THEN
C
C           Use reception stellar aberration correction
C           routine STELAB to generate a first estimate of
C           the direction vector after stellar aberration
C           has been "removed"---that is, apply the inverse
C           of the transmission stellar aberration correction
C           mapping to J2DIR.
C
            CALL STELAB ( J2DIR, SSBOST(4), STLDIR )

C
C           Estimate the error in our first approximation 
C           by applying the transmission stellar aberration
C           to STLDIR and finding the difference with J2DIR.
C
            CALL STLABX ( STLDIR, SSBOST(4), J2EST  )            
            CALL VSUB   ( J2DIR,  J2EST,     STLERR )

C
C           Adding the error in the transmission mapping to STLDIR
C           will give us a second-order estimate of the inverse.
C
            CALL VADD ( STLERR, STLDIR, STLTMP )
            CALL VEQU ( STLTMP,         STLDIR )
C
C           At this point we've found a good estimate of the
C           direction vector under the inverse of the transmission
C           stellar aberration correction mapping.
C
         ELSE
C
C           Use transmission stellar aberration correction
C           routine STLABX to generate a first estimate of
C           the direction vector after stellar aberration
C           has been "removed."  
C
            CALL STLABX ( J2DIR, SSBOST(4), STLDIR )

C
C           Estimate the error in our first approximation 
C           by applying the reception stellar aberration
C           to STLDIR and finding the difference with J2DIR.
C
            CALL STELAB ( STLDIR, SSBOST(4), J2EST  )            
            CALL VSUB   ( J2DIR,  J2EST,     STLERR )

C
C           Adding the error in the reception mapping to STLDIR
C           will give us a second-order estimate of the inverse.
C
            CALL VADD ( STLERR, STLDIR, STLTMP )
            CALL VEQU ( STLTMP,         STLDIR )

C
C           At this point we've found a good estimate of the
C           direction vector under the inverse of the reception
C           stellar aberration correction mapping.
C
         END IF

C
C        Replace the J2000-relative ray direction with the corrected
C        direction.
C
         CALL VEQU ( STLDIR,  J2DIR )
         CALL MXV  ( J2TMAT,  J2DIR,   TRGDIR )

      END IF

C
C     Find the surface intercept point and distance from observer to 
C     intercept point using the specified geometric definition.
C
      IF (  EQSTR( METHOD, 'Ellipsoid' )  ) THEN 
C
C        Find the surface intercept given the target epoch,
C        observer-target position, and target body orientation
C        we've already computed.  If we're not using light
C        time correction, this is all we must do.  Otherwise,
C        our result will give us an initial estimate of the
C        target epoch, which we'll then improve.
C
C        Get the radii of the target body from the kernel pool.
C
         CALL BODVCD ( TRGCDE, 'RADII', 3, NRADII, RADII )

C
C        Make an easy test to see whether we can quit now because
C        an intercept cannot exist.  If the ray is separated from
C        the observer-target center vector by more than (MARGIN *
C        the maximum triaxial radius), we're done.  Let REJECT be
C        the angular separation limit.
C        
         MAXRAD = MAX  ( RADII(1), RADII(2), RADII(3) )

         RANGE  = VNORM(OBSPOS)

         IF ( RANGE .EQ. 0.D0 ) THEN
C
C           We've already ensured that observer and target are
C           distinct, so this should be a very unusual occurrence.
C
            CALL SETMSG ( 'Observer-target distance is zero.' )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'               )
            CALL CHKOUT ( 'SRFXPT'                            )
            RETURN

         END IF


         IF ( RANGE .GT. MARGIN*MAXRAD ) THEN
C
C           Compute the arc sine with SPICE error checking.
C
            REJECT = DASINE ( MARGIN * MAXRAD / RANGE,  RNDTOL ) 

            CALL VMINUS ( OBSPOS, NEGPOS )
         
            IF (  VSEP( NEGPOS, TRGDIR )  .GT.  REJECT  ) THEN
C
C              The angular separation of ray and target is too great
C              for a solution to exist, even with a better light time
C              estimate.
C
               CALL CHKOUT ( 'SRFXPT' )
               RETURN

            END IF

         END IF

C
C        Locate the nearest point to the observer on the target.
C
         CALL SURFPT ( OBSPOS, TRGDIR, RADII(1), RADII(2), RADII(3), 
     .                 SPOINT, FOUND                                 )
   
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SRFXPT' )
            RETURN
         END IF

C
C        If we're not using light time corrections, we're almost
C        done now. TRGEPC, OBSPOS, and FOUND have been set.
C        If an intercept was found, SPOINT has been set as well.
C        We haven't yet computed DIST.
C        
         IF ( .NOT. USELT ) THEN

            IF ( FOUND ) THEN
               DIST  =  VDIST ( OBSPOS, SPOINT )
            END IF

            CALL CHKOUT ( 'SRFXPT' )
            RETURN

         END IF


         IF ( .NOT. FOUND ) THEN
C
C           If there's no intercept, we're probably done.  However,
C           we need to guard against the possibility that the ray does
C           intersect the ellipsoid but we haven't discovered it
C           because our first light time estimate was too poor. 
C
C           We'll make an improved light time estimate as follows:
C           Find the nearest point on the ellipsoid to the ray.  Find
C           the light time between the observer and this point.  
C
C           If we're using converged Newtonian corrections, we
C           iterate this procedure up to two times.
C
            IF ( USECN ) THEN
               NITR = 2
            ELSE
               NITR = 1
            END IF

            I = 1

            DO WHILE (  ( I .LE. NITR ) .AND. ( .NOT. FOUND )  )

               CALL NPEDLN ( RADII(1), RADII(2), RADII(3), OBSPOS,  
     .                       TRGDIR,   PNEAR,    RAYALT           )

               LT      =  VDIST ( OBSPOS, PNEAR ) / CLIGHT()
C
C              Use the new light time estimate to repeat the intercept 
C              computation.
C
               TRGEPC  =  ET  +  S * LT

C
C              Get the J2000-relative state of the target relative to
C              the solar system barycenter at the target epoch.
C
               CALL SPKSSB ( TRGCDE, TRGEPC, 'J2000', SSBTST )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SRFXPT' )
                  RETURN
               END IF

C
C              Find the position of the observer relative to the target.
C              Convert this vector from the J2000 frame to the target
C              frame at TRGEPC.
C
               CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
               CALL PXFORM ( 'J2000', FRNAME, TRGEPC, XFORM )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SRFXPT' )
                  RETURN
               END IF 
           
C
C              Convert the observer's position relative to the target
C              from the J2000 frame to the target frame at the target
C              epoch.
C
               CALL MXV ( XFORM, J2POS, OBSPOS )

C
C              Convert the ray's direction vector from the J2000 frame
C              to the target frame at the target epoch.
C   
               CALL MXV ( XFORM, J2DIR, TRGDIR )

C
C              Repeat the intercept computation.
C
               CALL SURFPT ( OBSPOS,   TRGDIR, RADII(1), RADII(2),  
     .                       RADII(3), SPOINT, FOUND               )

               I = I + 1

            END DO
C
C           If there's still no intercept, we're done.
C
            IF ( .NOT. FOUND ) THEN
               CALL CHKOUT ( 'SRFXPT' )
               RETURN
            END IF

         END IF

C
C        We've got an intersection.  SURFPT doesn't compute range, so do
C        it here.
C
         DIST  =  VDIST ( OBSPOS, SPOINT )

C
C        Since we're using light time corrections, we're going to make
C        an estimate of light time to the intercept point, then re-do
C        our computation of the target position and orientation using
C        the new light time value.
C
         IF ( USECN ) THEN
            NITR = MAXITR
         ELSE
            NITR = 1
         END IF


C
C        Get the J2000-relative state of the observer relative to
C        the solar system barycenter at ET.
C
         CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOST )

C
C        Compute new light time estimate and new target epoch.
C
         LT     =  DIST / CLIGHT()
         TRGEPC =  ET  +  S * LT

         PREVLT = 0.D0
         PREVET = TRGEPC

         I      = 0
         LTDIFF = 1.D0
         ETDIFF = 1.D0

         DO WHILE (       ( I      .LT.   NITR                ) 
     .              .AND. ( LTDIFF .GT. ( CNVLIM * ABS(LT) )  )
     .              .AND. ( ETDIFF .GT.   0.D0                )  )

C
C           Get the J2000-relative state of the target relative to
C           the solar system barycenter at the target epoch.
C
            CALL SPKSSB ( TRGCDE, TRGEPC, 'J2000', SSBTST )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SRFXPT' )
               RETURN
            END IF
C
C           Find the position of the observer relative to the target.
C           Convert this vector from the J2000 frame to the target
C           frame at TRGEPC.
C
            CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
            CALL PXFORM ( 'J2000', FRNAME, TRGEPC, XFORM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SRFXPT' )
               RETURN
            END IF
         
C
C           Convert the observer's position relative to the target from
C           the J2000 frame to the target frame at the target epoch.
C
            CALL MXV    ( XFORM,  J2POS, OBSPOS )
            CALL VMINUS ( OBSPOS, NEGPOS )

C
C           Convert the ray's direction vector from the J2000 frame
C           to the target frame at the target epoch.
C
            CALL MXV ( XFORM, J2DIR, TRGDIR )

C
C           Repeat the intercept computation.
C
            CALL SURFPT ( OBSPOS, TRGDIR, RADII(1), RADII(2), RADII(3), 
     .                    SPOINT, FOUND                                )
 
C
C           If there's no intercept, we're done.
C
            IF ( .NOT. FOUND ) THEN
                CALL CHKOUT ( 'SRFXPT' )
               RETURN
            END IF

C
C           Compute the distance between intercept and observer.
C
            DIST    =   VDIST ( OBSPOS, SPOINT )

C
C           Compute new light time estimate and new target epoch.
C
            LT      =  DIST / CLIGHT()

            TRGEPC  =  ET  +  S * LT

C
C           We use the d.p. identity function TOUCHD to force the
C           compiler to create double precision arguments from the
C           differences LT-PREVLT and TRGEPC-PREVET. Some compilers
C           will perform extended-precision register arithmetic, which
C           can prevent a difference from rounding to zero.  Simply
C           storing the result of the subtraction in a double precision
C           variable doesn't solve the problem, because that variable
C           can be optimized out of existence.
C
            LTDIFF  =   ABS( TOUCHD(LT     - PREVLT) )
            ETDIFF  =   ABS( TOUCHD(TRGEPC - PREVET) )
            PREVLT  =   LT
            PREVET  =   TRGEPC        
            I       =   I + 1

         END DO
                   
      ELSE
      
         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( 'SRFXPT'                                        )
         RETURN
         
      END IF

C
C     FOUND, SPOINT, TRGEPC, and DIST have been set at this point.
C     
      CALL CHKOUT ( 'SRFXPT' )
      RETURN
      END
