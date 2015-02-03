C$Procedure SINCPT ( Surface intercept )
 
      SUBROUTINE SINCPT ( METHOD,  TARGET,  ET,      FIXREF, 
     .                    ABCORR,  OBSRVR,  DREF,    DVEC,    
     .                    SPOINT,  TRGEPC,  SRFVEC,  FOUND  )
      
C$ Abstract
C
C     Given an observer and a direction vector defining a ray, compute
C     the surface intercept of the ray on a target body at a specified
C     epoch, optionally corrected for light time and stellar
C     aberration.
C
C     This routine supersedes SRFXPT.
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

      INCLUDE               'frmtyp.inc'
      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzctr.inc'
      
      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SRFVEC ( 3 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body.
C     ET         I   Epoch in ephemeris seconds past J2000 TDB.
C     FIXREF     I   Body-fixed, body-centered target body frame.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Name of observing body.
C     DREF       I   Reference frame of ray's direction vector.
C     DVEC       I   Ray's direction vector.
C     SPOINT     O   Surface intercept point on the target body.
C     TRGEPC     O   Intercept epoch.
C     SRFVEC     O   Vector from observer to intercept point.
C     FOUND      O   Flag indicating whether intercept was found.
C
C$ Detailed_Input
C
C     METHOD      is a short string providing parameters defining
C                 the computation method to be used. 
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
C                 METHOD. For example, the string ' eLLipsoid ' is 
C                 valid.                 

C                 
C     TARGET      is the name of the target body. TARGET is
C                 case-insensitive, and leading and trailing blanks in
C                 TARGET are not significant. Optionally, you may
C                 supply a string containing the integer ID code 
C                 for the object. For example both 'MOON' and '301'
C                 are legitimate strings that indicate the Moon is the
C                 target body.
C
C                 When the target body's surface is represented by a
C                 tri-axial ellipsoid, this routine assumes that a
C                 kernel variable representing the ellipsoid's radii is
C                 present in the kernel pool. Normally the kernel
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
C                 When aberration corrections are used, the position
C                 and orientation of the target body are computed at
C                 ET-LT or ET+LT, where LT is the one-way light time
C                 between the intercept point and the observer, and the
C                 sign applied to LT depends on the selected
C                 correction. See the description of ABCORR below for
C                 details.
C
C                                  
C     FIXREF      is the name of the body-fixed, body-centered
C                 reference frame associated with the target body. The
C                 output intercept point SPOINT and the observer to
C                 intercept vector SRFVEC will be expressed relative to
C                 this reference frame. The string FIXREF is
C                 case-insensitive, and leading and trailing blanks in
C                 FIXREF are not significant.
C
C
C     ABCORR      indicates the aberration corrections to be applied
C                 when computing the target's position and orientation.
C
C                 For remote sensing applications, where the apparent
C                 surface intercept point seen by the observer is
C                 desired, normally the correction
C              
C                    'CN+S'
C     
C                 should be used. This and the other supported options
C                 are described below. ABCORR may be any of the 
C                 following:
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
C                               Both the target position as seen by the
C                               observer, and rotation of the target
C                               body, are corrected for light time.
C
C                    'LT+S'     Correct for one-way light time and
C                               stellar aberration using a Newtonian
C                               formulation. This option modifies the
C                               surface intercept obtained with the
C                               'LT' option to account for the
C                               observer's velocity relative to the
C                               solar system barycenter. These
C                               computations yield the apparent surface
C                               intercept point.
C
C                    'CN'       Converged Newtonian light time
C                               correction. In solving the light time
C                               equation, the 'CN' correction iterates
C                               until the solution converges. Both the
C                               position and rotation of the target
C                               body are corrected for light time.
C
C                    'CN+S'     Converged Newtonian light time and
C                               stellar aberration corrections. This
C                               option produces a solution that is at
C                               least as accurate at that obtainable
C                               with the 'LT+S' option. Whether the
C                               'CN+S' solution is substantially more
C                               accurate depends on the geometry of the
C                               participating objects and on the
C                               accuracy of the input data. In all
C                               cases this routine will execute more
C                               slowly when a converged solution is
C                               computed.
C
C                               For reception-case applications
C                               involving intercepts near the target
C                               body limb, this option should be used.
C
C                 The following values of ABCORR apply to the
C                 "transmission" case in which photons *depart* from
C                 the observer's location at ET and arrive at the
C                 intercept point at the light-time corrected epoch
C                 ET+LT:
C
C                    'XLT'      "Transmission" case: correct for
C                               one-way light time using a Newtonian
C                               formulation. This correction yields the
C                               intercept location at the moment it
C                               receives photons emitted from the
C                               observer's location at ET. 
C
C                               The light time correction uses an
C                               iterative solution of the light time
C                               equation. The solution invoked by the
C                               'XLT' option uses one iteration.
C
C                               Both the target position as seen by the
C                               observer, and rotation of the target
C                               body, are corrected for light time.
C
C                    'XLT+S'    "Transmission" case: correct for
C                               one-way light time and stellar
C                               aberration using a Newtonian
C                               formulation  This option modifies the
C                               intercept obtained with the 'XLT'
C                               option to account for the observer's
C                               velocity relative to the solar system
C                               barycenter.
C
C                    'XCN'      Converged Newtonian light time
C                               correction. This is the same as XLT
C                               correction but with further iterations
C                               to a converged Newtonian light time
C                               solution. 
C
C                    'XCN+S'    "Transmission" case: converged
C                               Newtonian light time and stellar
C                               aberration corrections. This option
C                               produces a solution that is at least as
C                               accurate at that obtainable with the
C                               'XLT+S' option. Whether the 'XCN+S'
C                               solution is substantially more accurate
C                               depends on the geometry of the
C                               participating objects and on the
C                               accuracy of the input data. In all
C                               cases this routine will execute more
C                               slowly when a converged solution is
C                               computed.
C
C                               For transmission-case applications
C                               involving intercepts near the target
C                               body limb, this option should be used.
C
C                Case and embedded blanks are not significant in
C                ABCORR. For example, the string
C
C                   'Cn + s'
C
C                 is valid.
C
C
C     OBSRVR      is the name of the observing body. This is typically
C                 a spacecraft, the earth, or a surface point on the
C                 earth. OBSRVR is case-insensitive, and leading and
C                 trailing blanks in OBSRVR are not significant.
C                 Optionally, you may supply a string containing the
C                 integer ID code for the object. For example both
C                 'MOON' and '301' are legitimate strings that indicate
C                 the Moon is the observer.
C
C
C     DREF        is the name of the reference frame relative to which
C                 the ray's direction vector is expressed. This may be
C                 any frame supported by the SPICE system, including
C                 built-in frames (documented in the Frames Required
C                 Reading) and frames defined by a loaded frame kernel
C                 (FK). The string DREF is case-insensitive, and
C                 leading and trailing blanks in DREF are not
C                 significant.
C
C                 When DREF designates a non-inertial frame, the
C                 orientation of the frame is evaluated at an epoch
C                 dependent on the frame's center and, if the center is
C                 not the observer, on the selected aberration
C                 correction. See the description of the direction
C                 vector DVEC for details.
C
C
C     DVEC        Ray direction vector emanating from the observer. The
C                 intercept with the target body's surface of the ray
C                 defined by the observer and DVEC is sought.
C
C                 DVEC is specified relative to the reference frame
C                 designated by DREF.
C
C                 Non-inertial reference frames are treated as follows:
C                 if the center of the frame is at the observer's
C                 location, the frame is evaluated at ET. If the
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
C
C$ Detailed_Output
C
C
C     SPOINT      is the surface intercept point on the target body of
C                 the ray defined by the observer and the direction
C                 vector. If the ray intersects the target body in
C                 multiple points, the selected intersection point is
C                 the one closest to the observer. The output argument
C                 FOUND (see below) indicates whether an intercept was
C                 found.
C
C                 SPOINT is expressed in Cartesian coordinates,
C                 relative to the target body-fixed frame designated by
C                 FIXREF. The body-fixed target frame is evaluated at
C                 the intercept epoch TRGEPC (see description below).
C
C                 When light time correction is used, the duration of
C                 light travel between SPOINT to the observer is
C                 considered to be the one way light time. When both
C                 light time and stellar aberration corrections are
C                 used, SPOINT is selected such that, when SPOINT is
C                 corrected for light time and stellar aberration, the
C                 resulting vector is parallel to SPOINT lies on the
C                 ray defined by the observer's location and DVEC.
C
C                 The components of SPOINT are given in units of km.
C
C
C     TRGEPC      is the "intercept epoch." TRGEPC is defined as
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
C     SRFVEC      is the vector from the observer's position at ET to
C                 the aberration-corrected (or optionally, geometric)
C                 position of SPOINT, where the aberration corrections
C                 are specified by ABCORR. SRFVEC is expressed in the
C                 target body-fixed reference frame designated by
C                 FIXREF, evaluated at TRGEPC.
C  
C                 The components of SRFVEC are given in units of km.
C
C                 One can use the SPICELIB function VNORM to obtain the
C                 distance between the observer and SPOINT:
C
C                    DIST = VNORM ( SRFVEC )
C
C                 The observer's position OBSPOS, relative to the
C                 target body's center, where the center's position is
C                 corrected for aberration effects as indicated by
C                 ABCORR, can be computed via the call:
C
C                    CALL VSUB ( SPOINT, SRFVEC, OBSPOS )
C
C                 To transform the vector SRFVEC from a reference frame
C                 FIXREF at time TRGEPC to a time-dependent reference
C                 frame REF at time ET, the routine PXFRM2 should be
C                 called. Let XFORM be the 3x3 matrix representing the
C                 rotation from the reference frame FIXREF at time
C                 TRGEPC to the reference frame REF at time ET. Then
C                 SRFVEC can be transformed to the result REFVEC as
C                 follows:
C
C                     CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM )
C                     CALL MXV    ( XFORM,  SRFVEC, REFVEC )
C
C                 The second example in the Examples header section
C                 below presents a complete program that demonstrates
C                 this procedure.
C     
C
C     FOUND       A logical flag indicating whether or not the ray
C                 intersects the target. If an intersection exists
C                 FOUND will be returned as .TRUE. If the ray misses
C                 the target, FOUND will be returned as .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C 
C
C     1)  If the specified aberration correction is unrecognized, the
C         error will be diagnosed and signaled by a routine in the call
C         tree of this routine.
C
C     2)  If either the target or observer input strings cannot be
C         converted to an integer ID code, the error
C         SPICE(IDCODENOTFOUND) is signaled.
C
C     3)  If OBSRVR and TARGET map to the same NAIF integer ID code,
C         the error SPICE(BODIESNOTDISTINCT) is signaled.
C
C     4)  If the input target body-fixed frame FIXREF is not
C         recognized, the error SPICE(NOFRAME) is signaled. A frame
C         name may fail to be recognized because a required frame
C         specification kernel has not been loaded; another cause is a
C         misspelling of the frame name.
C
C     5)  If the input frame FIXREF is not centered at the target body,
C         the error SPICE(INVALIDFRAME) is signaled.
C
C     6)  If the input argument METHOD is not recognized, the error
C         SPICE(INVALIDMETHOD) is signaled.
C
C     7)  If the target and observer have distinct identities but are
C         at the same location (for example, the target is Mars and the
C         observer is the Mars barycenter), the error
C         SPICE(NOSEPARATION) is signaled.
C
C     8)  If insufficient ephemeris data have been loaded prior to
C         calling SINCPT, the error will be diagnosed and signaled by a
C         routine in the call tree of this routine. Note that when
C         light time correction is used, sufficient ephemeris data must
C         be available to propagate the states of both observer and
C         target to the solar system barycenter.
C
C     9)  If the computation method specifies an ellipsoidal target
C         shape and triaxial radii of the target body have not been
C         loaded into the kernel pool prior to calling SINCPT, the
C         error will be diagnosed and signaled by a routine in the call
C         tree of this routine.
C
C     10) The target must be an extended body: if any of the radii of
C         the target body are non-positive, the error will be
C         diagnosed and signaled by routines in the call tree of this
C         routine.
C
C     11) If PCK data specifying the target body-fixed frame
C         orientation have not been loaded prior to calling SINCPT,
C         the error will be diagnosed and signaled by a routine in the
C         call tree of this routine.
C
C     12) If the reference frame designated by DREF is not recognized
C         by the SPICE frame subsystem, the error SPICE(NOFRAME)
C         will be signaled.
C
C     13) If the direction vector DVEC is the zero vector, the error
C         SPICE(ZEROVECTOR) will be signaled.
C         
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target and observer must be
C          loaded. If aberration corrections are used, the states of
C          target and observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C          Typically ephemeris data are made available by loading one
C          or more SPK files via FURNSH.
C
C        - PCK data: if the computation method is specified as
C          "Ellipsoid," triaxial radii for the target body must be 
C          loaded into the kernel pool. Typically this is done by
C          loading a text PCK file via FURNSH.
C
C        - Further PCK data: rotation data for the target body must
C          be loaded. These may be provided in a text or binary PCK
C          file. 
C
C     The following data may be required:
C
C        - Frame data: if a frame definition is required to convert
C          the observer and target states to the body-fixed frame of
C          the target, that definition must be available in the kernel
C          pool. Similarly, the frame definition required to map
C          between the frame designated by DREF and the target
C          body-fixed frame must be available. Typically the
C          definitions of frames not already built-in to SPICE are
C          supplied by loading a frame kernel.
C
C        - CK data: if the frame to which DREF refers is fixed to a
C          spacecraft instrument or structure, at least one CK file
C          will be needed to permit transformation of vectors between
C          that frame and both the J2000 and the target body-fixed
C          frames.
C
C        - SCLK data: if a CK file is needed, an associated SCLK
C          kernel is required to enable conversion between encoded SCLK
C          (used to time-tag CK data) and barycentric dynamical time
C          (TDB).
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     Given a ray defined by a direction vector and the location of an
C     observer, SINCPT computes the surface intercept point of the ray
C     on a specified target body. SINCPT also determines the vector
C     from the observer to the surface intercept point.
C
C     When aberration corrections are used, this routine finds the
C     value of SPOINT such that, if SPOINT is regarded as an ephemeris
C     object, after the selected aberration corrections are applied to
C     the vector from the observer to SPOINT, the resulting vector is
C     parallel to the direction vector DVEC.
C
C     This routine computes light time corrections using light time
C     between the observer and the surface intercept point, as opposed
C     to the center of the target. Similarly, stellar aberration
C     corrections done by this routine are based on the direction of
C     the vector from the observer to the light-time corrected
C     intercept point, not to the target center. This technique avoids
C     errors due to the differential between aberration corrections
C     across the target body. Therefore it's valid to use aberration
C     corrections with this routine even when the observer is very
C     close to the intercept point, in particular when the
C     observer-intercept point distance is much less than the
C     observer-target center distance. It's also valid to use stellar
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
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C
C     1) The following program computes surface intercept points on Mars
C        for the boresight and FOV boundary vectors of the MGS MOC
C        narrow angle camera. The intercepts are computed for a single
C        observation epoch. Light time and stellar aberration
C        corrections are used. For simplicity, camera distortion is
C        ignored.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File: mgs_example2.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de418.bsp                     Planetary ephemeris
C              pck00008.tpc                  Planet orientation and
C                                            radii
C              naif0008.tls                  Leapseconds
C              mgs_moc_v20.ti                MGS MOC instrument
C                                            parameters
C              mgs_sclkscet_00061.tsc        MGS SCLK coefficients
C              mgs_sc_ext12.bc               MGS s/c bus attitude
C              mgs_ext12_ipng_mgs95j.bsp     MGS ephemeris
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de418.bsp',
C                                  'pck00008.tpc',
C                                  'naif0008.tls',
C                                  'mgs_moc_v20.ti',
C                                  'mgs_sclkscet_00061.tsc',
C                                  'mgs_sc_ext12.bc',
C                                  'mgs_ext12_ipng_mgs95j.bsp' )
C           \begintext
C
C
C        Example code begins here.
C
C          PROGRAM EX1
C          IMPLICIT NONE
C    C
C    C     SPICELIB functions
C    C
C          DOUBLE PRECISION      VNORM
C
C    C
C    C     Local parameters
C    C
C          CHARACTER*(*)         META
C          PARAMETER           ( META   = 'mgs_example2.tm' )
C
C          INTEGER               ABCLEN
C          PARAMETER           ( ABCLEN = 20 )
C
C          INTEGER               LNSIZE
C          PARAMETER           ( LNSIZE = 78 )
C
C          INTEGER               METLEN
C          PARAMETER           ( METLEN = 40 )
C
C          INTEGER               NAMLEN
C          PARAMETER           ( NAMLEN = 32 )
C
C          INTEGER               TIMLEN
C          PARAMETER           ( TIMLEN = 50 )
C
C          INTEGER               SHPLEN
C          PARAMETER           ( SHPLEN = 80 )
C
C          INTEGER               NCORNR
C          PARAMETER           ( NCORNR = 4 )
C
C    C
C    C     Local variables
C    C
C          CHARACTER*(ABCLEN)    ABCORR
C          CHARACTER*(NAMLEN)    CAMERA
C          CHARACTER*(NAMLEN)    DREF
C          CHARACTER*(METLEN)    METHOD
C          CHARACTER*(NAMLEN)    OBSRVR
C          CHARACTER*(SHPLEN)    SHAPE
C          CHARACTER*(NAMLEN)    TARGET
C          CHARACTER*(LNSIZE)    TITLE
C          CHARACTER*(TIMLEN)    UTC
C
C          DOUBLE PRECISION      BOUNDS ( 3, NCORNR )
C          DOUBLE PRECISION      BSIGHT ( 3 )
C          DOUBLE PRECISION      DIST
C          DOUBLE PRECISION      DPR
C          DOUBLE PRECISION      DVEC   ( 3 )
C          DOUBLE PRECISION      ET
C          DOUBLE PRECISION      LAT
C          DOUBLE PRECISION      LON
C          DOUBLE PRECISION      RADIUS
C          DOUBLE PRECISION      SPOINT ( 3 )
C          DOUBLE PRECISION      SRFVEC ( 3 )
C          DOUBLE PRECISION      TRGEPC
C
C          INTEGER               CAMID
C          INTEGER               I
C          INTEGER               J
C          INTEGER               N
C
C          LOGICAL               FOUND
C
C          DATA                  ABCORR / 'CN+S'      /
C          DATA                  CAMERA / 'MGS_MOC_NA'/
C          DATA                  METHOD / 'Ellipsoid' /
C          DATA                  OBSRVR / 'MGS'       /
C          DATA                  TARGET / 'Mars'      /
C          DATA                  UTC    / '2003 OCT 13 06:00:00 UTC' /
C
C    C
C    C     Load kernel files:
C    C
C          CALL FURNSH ( META )
C
C    C
C    C     Convert the UTC request time to ET (seconds past
C    C     J2000, TDB).
C    C
C          CALL STR2ET ( UTC, ET )
C
C    C
C    C     Get the MGS MOC Narrow angle camera (MGS_MOC_NA)
C    C     ID code. Then look up the field of view (FOV)
C    C     parameters by calling GETFOV.
C    C
C          CALL BODN2C ( CAMERA, CAMID, FOUND )
C
C          IF ( .NOT. FOUND ) THEN
C             CALL SETMSG ( 'Could not find ID code for ' //
C         .                 'instrument #.'               )
C             CALL ERRCH  ( '#', CAMERA                   )
C             CALL SIGERR ( 'SPICE(NOTRANSLATION)'        )
C          END IF
C
C    C
C    C     GETFOV will return the name of the camera-fixed frame
C    C     in the string DREF, the camera boresight vector in
C    C     the array BSIGHT, and the FOV corner vectors in the
C    C     array BOUNDS.
C    C
C          CALL GETFOV ( CAMID,  NCORNR, SHAPE,  DREF,
C         .              BSIGHT, N,      BOUNDS       )
C
C
C          WRITE (*,*) ' '
C          WRITE (*,*) 'Surface Intercept Locations for Camera'
C          WRITE (*,*) 'FOV Boundary and Boresight Vectors'
C          WRITE (*,*) ' '
C          WRITE (*,*) '   Instrument:            ', CAMERA
C          WRITE (*,*) '   Epoch:                 ', UTC
C          WRITE (*,*) '   Aberration correction: ', ABCORR
C          WRITE (*,*) ' '
C
C    C
C    C     Now compute and display the surface intercepts for the
C    C     boresight and all of the FOV boundary vectors.
C    C
C          DO I = 1, NCORNR+1
C
C             IF ( I .LE. NCORNR ) THEN
C
C                TITLE = 'Corner vector #'
C                CALL REPMI ( TITLE, '#', I, TITLE )
C
C                CALL VEQU ( BOUNDS(1,I), DVEC )
C
C             ELSE
C
C                TITLE = 'Boresight vector'
C                CALL VEQU ( BSIGHT, DVEC )
C
C             END IF
C
C    C
C    C        Compute the surface intercept point using
C    C        the specified aberration corrections.
C    C
C             CALL SINCPT ( METHOD, TARGET, ET,     'IAU_MARS',
C         .                 ABCORR, OBSRVR, DREF,   DVEC,
C         .                 SPOINT, TRGEPC, SRFVEC, FOUND      )
C
C             IF ( FOUND ) THEN
C    C
C    C           Compute range from observer to apparent intercept.
C    C
C                DIST = VNORM ( SRFVEC )
C    C
C    C           Convert rectangular coordinates to planetocentric
C    C           latitude and longitude. Convert radians to degrees.
C    C
C                CALL RECLAT ( SPOINT, RADIUS, LON, LAT )
C
C                LON = LON * DPR ()
C                LAT = LAT * DPR ()
C    C
C    C           Display the results.
C    C
C                WRITE (*,*) ' '
C                WRITE (*,*) TITLE
C
C                TITLE = '  Vector in # frame = '
C                CALL REPMC ( TITLE, '#', DREF, TITLE )
C
C                WRITE (*,*) ' '
C                WRITE (*,*) TITLE
C
C                IF ( I .LE. NCORNR ) THEN
C                   WRITE (*,*) '  ', ( BOUNDS(J,I), J = 1, 3 )
C                ELSE
C                   WRITE (*,*) '  ', BSIGHT
C                END IF
C
C                WRITE (*,*) ' '
C                WRITE (*,*) '  Intercept:'
C                WRITE (*,*)
C         .      '     Radius                   (km)  = ', RADIUS
C                WRITE (*,*)
C         .      '     Planetocentric Latitude  (deg) = ', LAT
C                WRITE (*,*)
C         .      '     Planetocentric Longitude (deg) = ', LON
C                WRITE (*,*)
C         .      '     Range                    (km)  = ', DIST
C                WRITE (*,*) ' '
C
C             ELSE
C
C                WRITE (*,*) ' '
C                WRITE (*,*) 'Intercept not found.'
C                WRITE (*,*) ' '
C
C             END IF
C
C          END DO
C
C          END
C
C
C     When this program was executed on a PC/Linux/g77 platform, the
C     output was:
C
C        Surface Intercept Locations for Camera
C        FOV Boundary and Boresight Vectors
C
C           Instrument:            MGS_MOC_NA
C           Epoch:                 2003 OCT 13 06:00:00 UTC
C           Aberration correction: CN+S
C
C
C        Corner vector 1
C
C          Vector in MGS_MOC_NA frame =
C            1.85713838E-06 -0.00380156227  0.999992774
C
C          Intercept:
C             Radius                   (km)  =   3384.94114
C             Planetocentric Latitude  (deg) =  -48.4774819
C             Planetocentric Longitude (deg) =  -123.474079
C             Range                    (km)  =   388.983104
C
C
C        Corner vector 2
C
C          Vector in MGS_MOC_NA frame =
C            1.85713838E-06  0.00380156227  0.999992774
C
C          Intercept:
C             Radius                   (km)  =   3384.9397
C             Planetocentric Latitude  (deg) =  -48.4816363
C             Planetocentric Longitude (deg) =  -123.398823
C             Range                    (km)  =   388.975121
C
C
C        Corner vector 3
C
C          Vector in MGS_MOC_NA frame =
C           -1.85713838E-06  0.00380156227  0.999992774
C
C          Intercept:
C             Radius                   (km)  =   3384.93969
C             Planetocentric Latitude  (deg) =  -48.4816619
C             Planetocentric Longitude (deg) =  -123.398826
C             Range                    (km)  =   388.974662
C
C
C        Corner vector 4
C
C          Vector in MGS_MOC_NA frame =
C           -1.85713838E-06 -0.00380156227  0.999992774
C
C          Intercept:
C             Radius                   (km)  =   3384.94113
C             Planetocentric Latitude  (deg) =  -48.4775075
C             Planetocentric Longitude (deg) =  -123.474082
C             Range                    (km)  =   388.982645
C
C
C        Boresight vector
C
C          Vector in MGS_MOC_NA frame =
C            0.  0.  1.
C
C          Intercept:
C             Radius                   (km)  =   3384.94041
C             Planetocentric Latitude  (deg) =  -48.4795798
C             Planetocentric Longitude (deg) =  -123.436454
C             Range                    (km)  =   388.975736
C
C
C 
C     2) Use SUBPNT to find the sub-spacecraft point on Mars for the
C        Mars Reconnaissance Orbiter spacecraft (MRO) at a specified
C        time, using the "near point: ellipsoid" computation method.
C        Use both LT+S and CN+S aberration corrections to illustrate
C        the differences.
C
C        Convert the spacecraft to sub-observer point vector obtained
C        from SUBPNT into the MRO_HIRISE_LOOK_DIRECTION reference frame
C        at the observation time. Perform a consistency check with this
C        vector: compare the Mars surface intercept of the ray
C        emanating from the spacecraft and pointed along this vector
C        with the sub-observer point.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File: mro_example.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de418.bsp                     Planetary ephemeris
C              pck00008.tpc                  Planet orientation and
C                                                 radii
C              naif0008.tls                  Leapseconds
C              mro_psp4_ssd_mro95a.bsp       MRO ephemeris
C              mro_v11.tf                    MRO frame specifications
C              mro_sclkscet_00022_65536.tsc  MRO SCLK coefficients and
C                                                 parameters
C              mro_sc_psp_070925_071001.bc   MRO attitude
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de418.bsp',
C                                  'pck00008.tpc',
C                                  'naif0008.tls',
C                                  'mro_psp4_ssd_mro95a.bsp',
C                                  'mro_v11.tf',
C                                  'mro_sclkscet_00022_65536.tsc',
C                                  'mro_sc_psp_070925_071001.bc'  )
C           \begintext
C
C
C       Example code begins here.
C
C
C          PROGRAM EX2
C          IMPLICIT NONE
C    C
C    C     SPICELIB functions
C    C
C          DOUBLE PRECISION      DPR
C          DOUBLE PRECISION      VDIST
C          DOUBLE PRECISION      VNORM
C
C    C
C    C     Local parameters
C    C
C          CHARACTER*(*)         META
C          PARAMETER           ( META   = 'mro_example.tm' )
C
C          CHARACTER*(*)         F1
C          PARAMETER           ( F1     = '(A,F21.9)' )
C
C          CHARACTER*(*)         F2
C          PARAMETER           ( F2     = '(A)' )
C
C          INTEGER               FRNMLN
C          PARAMETER           ( FRNMLN = 32 )
C
C          INTEGER               MTHLEN
C          PARAMETER           ( MTHLEN = 50 )
C
C          INTEGER               CORLEN
C          PARAMETER           ( CORLEN = 5 )
C
C          INTEGER               NCORR
C          PARAMETER           ( NCORR  = 2 )
C
C    C
C    C     Local variables
C    C
C          CHARACTER*(CORLEN)    ABCORR ( NCORR )
C          CHARACTER*(FRNMLN)    HIREF
C          CHARACTER*(MTHLEN)    METHOD
C
C          DOUBLE PRECISION      ALT
C          DOUBLE PRECISION      ET
C          DOUBLE PRECISION      LAT
C          DOUBLE PRECISION      LON
C          DOUBLE PRECISION      MROVEC ( 3 )
C          DOUBLE PRECISION      R1     ( 3, 3 )
C          DOUBLE PRECISION      R2     ( 3, 3 )
C          DOUBLE PRECISION      RADIUS
C          DOUBLE PRECISION      SPOINT ( 3 )
C          DOUBLE PRECISION      SRFVEC ( 3 )
C          DOUBLE PRECISION      TRGEPC
C          DOUBLE PRECISION      XFORM  ( 3, 3 )
C          DOUBLE PRECISION      XEPOCH
C          DOUBLE PRECISION      XPOINT ( 3 )
C          DOUBLE PRECISION      XVEC   ( 3 )
C
C          INTEGER               I
C
C          LOGICAL               FOUND
C
C    C
C    C     Initial values
C    C
C          DATA                  ABCORR / 'LT+S', 'CN+S' /
C    C
C    C     Load kernel files via the meta-kernel.
C    C
C          CALL FURNSH ( META )
C
C    C
C    C     Convert the TDB request time string to seconds past
C    C     J2000, TDB.
C    C
C          CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET )
C
C    C
C    C     Compute the sub-spacecraft point using the
C    C     "NEAR POINT: ELLIPSOID" definition.
C    C     Compute the results using both LT+S and CN+S
C    C     aberration corrections.
C    C
C          METHOD = 'Near point: ellipsoid'
C
C          WRITE(*,F2) ' '
C          WRITE(*,F2) 'Computation method = '//METHOD
C
C          DO I = 1, NCORR
C
C             CALL SUBPNT ( METHOD,
C         .                 'Mars', ET,     'IAU_MARS', ABCORR(I),
C         .                 'MRO',  SPOINT, TRGEPC,     SRFVEC    )
C    C
C    C        Compute the observer's altitude above SPOINT.
C    C
C             ALT = VNORM ( SRFVEC )
C    C
C    C        Express SRFVEC in the MRO_HIRISE_LOOK_DIRECTION
C    C        reference frame at epoch ET. Since SRFVEC is expressed
C    C        relative to the IAU_MARS frame at TRGEPC, we must
C    C        call PXFRM2 to compute the position transformation matrix
C    C        from IAU_MARS at TRGEPC to the MRO_HIRISE_LOOK_DIRECTION 
C    C        frame at time ET.
C    C
C    C        To make code formatting a little easier, we'll store
C    C        the long MRO reference frame name in a variable:
C    C
C             HIREF = 'MRO_HIRISE_LOOK_DIRECTION'
C
C             CALL PXFRM2 ( 'IAU_MARS', HIREF,  TRGEPC, ET, XFORM )
C             CALL MXV    (  XFORM,     SRFVEC, MROVEC )
C
C    C
C    C        Convert rectangular coordinates to planetocentric
C    C        latitude and longitude. Convert radians to degrees.
C    C
C             CALL RECLAT ( SPOINT, RADIUS, LON, LAT  )
C
C             LON = LON * DPR ()
C             LAT = LAT * DPR ()
C    C
C    C        Write the results.
C    C
C             WRITE(*,F2) ' '
C             WRITE(*,F2) 'Aberration correction = '//ABCORR(I)
C             WRITE(*,F1) ' '
C             WRITE(*,F2) '  MRO-to-sub-observer vector in'
C             WRITE(*,F2) '  MRO HIRISE look direction frame'
C             WRITE(*,F1) '     X-component             (km) = ',
C         .               MROVEC(1)
C             WRITE(*,F1) '     Y-component             (km) = ',
C         .               MROVEC(2)
C             WRITE(*,F1) '     Z-component             (km) = ',
C         .               MROVEC(3)
C             WRITE(*,F1) '  Sub-observer point radius  (km) = ', RADIUS
C             WRITE(*,F1) '  Planetocentric latitude   (deg) = ', LAT
C             WRITE(*,F1) '  Planetocentric longitude  (deg) = ', LON
C             WRITE(*,F1) '  Observer altitude          (km) = ', ALT
C
C    C
C    C        Consistency check: find the surface intercept on
C    C        Mars of the ray emanating from the spacecraft and having
C    C        direction vector MROVEC in the MRO HIRISE look direction
C    C        reference frame at ET. Call the intercept point
C    C        XPOINT. XPOINT should coincide with SPOINT, up to a
C    C        small round-off error.
C    C
C             CALL SINCPT ( 'Ellipsoid', 'Mars', ET,    'IAU_MARS',
C         .                 ABCORR(I),   'MRO',  HIREF, MROVEC,
C         .                 XPOINT,      XEPOCH, XVEC,  FOUND  )
C
C             IF ( .NOT. FOUND ) THEN
C                WRITE (*,F1) 'Bug: no intercept'
C             ELSE
C    C
C    C           Report the distance between XPOINT and SPOINT.
C    C
C                WRITE (*,F1) '  Intercept comparison error (km) = ',
C         .                   VDIST( XPOINT, SPOINT )
C             END IF
C
C             WRITE(*,F1) ' '
C
C          END DO
C
C          END
C
C
C     When this program was executed on a PC/Linux/gfortran platform,
C     the output was:
C
C
C        Computation method = Near point: ellipsoid
C
C        Aberration correction = LT+S
C
C          MRO-to-sub-observer vector in
C          MRO HIRISE look direction frame
C             X-component             (km) =           0.286931987
C             Y-component             (km) =          -0.260417167
C             Z-component             (km) =         253.816284981
C          Sub-observer point radius  (km) =        3388.299078207
C          Planetocentric latitude   (deg) =         -38.799836879
C          Planetocentric longitude  (deg) =        -114.995294746
C          Observer altitude          (km) =         253.816580760
C          Intercept comparison error (km) =           0.000002144
C
C
C        Aberration correction = CN+S
C
C          MRO-to-sub-observer vector in
C          MRO HIRISE look direction frame
C             X-component             (km) =           0.286931866
C             Y-component             (km) =          -0.260417914
C             Z-component             (km) =         253.816274506
C          Sub-observer point radius  (km) =        3388.299078205
C          Planetocentric latitude   (deg) =         -38.799836883
C          Planetocentric longitude  (deg) =        -114.995294968
C          Observer altitude          (km) =         253.816570285
C          Intercept comparison error (km) =           0.000000001
C
C
C$ Restrictions
C
C     A cautionary note: if aberration corrections are used, and 
C     if DREF is the target body-fixed frame, the epoch at which that
C     frame is evaluated is offset from ET by the light time between
C     the observer and the *center* of the target body. This light time
C     normally will differ from the light time between the observer and
C     intercept point. Consequently the orientation of the target
C     body-fixed frame at TRGEPC will not match that of the target
C     body-fixed frame at the epoch associated with DREF. As a result,
C     various derived quantities may not be as expected: for example,
C     SRFVEC would not be parallel to DVEC.
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
C     S.C. Krening   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 31-MAR-2014 (NJB) (SCK) (BVS)
C
C        Bug fix: FIRST is now set to .FALSE. at the completion
C        of a successful initialization pass. This does not affect
C        the routine's outputs but improves efficiency.
C
C        Bug fix: redundant call to SPKSSB was removed. This does not
C        affect the routine's outputs but improves efficiency.
C
C        References to the new PXFRM2 routine were added, which changed
C        the Detailed Output section and the second example. Some header
C        comment corrections were made.
C
C        Upgrade: this routine now uses ZZVALCOR rather than
C        ZZPRSCOR, simplifying the implementation.
C
C        Upgrade: this routine now saves the input body names and
C        ZZBODTRN state counters and does name-ID conversions only if
C        the counters have changed.
C
C        Upgrade: this routine now saves the input frame names and POOL
C        state counters and does frame name-ID conversions only if the
C        counters have changed.
C
C-    SPICELIB Version 1.2.0, 07-APR-2010 (NJB)
C
C        Code style improvement: re-use of variables in 
C        FRINFO calls has been eliminated. There is no impact 
C        of the behavior of the routine.
C
C-    SPICELIB Version 1.1.0, 17-MAR-2009 (NJB)(EDW) 
C
C        Bug fix: quick test for non-intersection is
C        no longer performed when observer-target distance
C        is less than target's maximum radius.
C
C        Typos in the Detailed Input section's description of DREF
C        were corrected.
C
C        In the header examples, meta-kernel names were updated to use
C        the suffix 
C
C           ".tm"
C
C        Incorrect frame name FIXFRM was changed to FIXREF in
C        documentation.
C
C        Typo correction in Required_Reading, changed FRAME 
C        to FRAMES.
C
C-    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) 
C
C-&
 
C$ Index_Entries
C
C     find surface intercept point
C     find intersection of ray and target body surface
C     find intercept of ray on target body surface
C
C-&
 

C$ Revisions
C
C     None.
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
      
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C     
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'SINCPT' )

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
C     Fraction of target body angular radius used to define
C     region outside of which rays are immediately rejected
C     as non-intersecting.
C
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.01D0 )


      INTEGER               MAXITR
      PARAMETER           ( MAXITR =  10 )

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
      CHARACTER*(CORLEN)    LOCCOR
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ETDIFF
      DOUBLE PRECISION      J2DIR  ( 3 )
      DOUBLE PRECISION      J2EST  ( 3 )
      DOUBLE PRECISION      J2GEOM ( 3 )
      DOUBLE PRECISION      J2POS  ( 3 )
      DOUBLE PRECISION      J2TMAT ( 3, 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTCENT
      DOUBLE PRECISION      LTDIFF
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      NEGPOS ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      PREVET
      DOUBLE PRECISION      PREVLT
      DOUBLE PRECISION      R2JMAT ( 3, 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RAYALT
      DOUBLE PRECISION      REFEPC
      DOUBLE PRECISION      REJECT
      DOUBLE PRECISION      RELERR
      DOUBLE PRECISION      RPOS   ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SRFLEN
      DOUBLE PRECISION      SSBOST ( 6 )
      DOUBLE PRECISION      SSBTST ( 6 )
      DOUBLE PRECISION      STLDIR ( 3 )
      DOUBLE PRECISION      STLERR ( 3 )
      DOUBLE PRECISION      STLTMP ( 3 )
      DOUBLE PRECISION      TPOS   ( 3 )
      DOUBLE PRECISION      TRGDIR ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               DCENTR
      INTEGER               DCLASS
      INTEGER               DTYPID
      INTEGER               DFRCDE
      INTEGER               FXCENT
      INTEGER               FXCLSS
      INTEGER               FXFCDE
      INTEGER               FXTYID
      INTEGER               I
      INTEGER               NITR
      INTEGER               NRADII
      INTEGER               OBSCDE
      INTEGER               TRGCDE
      
      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FIRST
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

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVFREF
      INTEGER               SVFXFC

      INTEGER               SVCTR4 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVDREF
      INTEGER               SVDFRC



C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  LOCCOR
      SAVE                  PRVCOR
      SAVE                  USECN
      SAVE                  USELT
      SAVE                  USESTL
      SAVE                  XMIT

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

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR3
      SAVE                  SVFREF
      SAVE                  SVFXFC

      SAVE                  SVCTR4
      SAVE                  SVDREF
      SAVE                  SVDFRC


 
C
C     Initial values
C
      DATA                  FIRST  / .TRUE.  /
      DATA                  LOCCOR / ' '     /
      DATA                  PRVCOR / ' '     /
      DATA                  USECN  / .FALSE. / 
      DATA                  USELT  / .FALSE. / 
      DATA                  USESTL / .FALSE. / 
      DATA                  XMIT   / .FALSE. / 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )
  
C
C     Nothing has been found yet.
C
      FOUND = .FALSE.

C
C     Counter initialization is done separately.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )
         CALL ZZCTRUIN( SVCTR3 )
         CALL ZZCTRUIN( SVCTR4 )

      END IF

      IF (  FIRST  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
C
C        The aberration correction flag differs from the value it
C        had on the previous call, if any. Analyze the new flag.
C
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        The aberration correction flag is valid; save it.
C
         PRVCOR = ABCORR

C
C        Set logical flags indicating the attributes of the requested
C        correction:
C
C           XMIT is .TRUE. when the correction is for transmitted
C           radiation.
C
C           USELT is .TRUE. when any type of light time correction
C           (normal or converged Newtonian) is specified.
C
C           USECN indicates converged Newtonian light time correction.
C
C           USESTL indicates stellar aberration corrections.
C
C
C        The above definitions are consistent with those used by
C        ZZPRSCOR.
C 
         XMIT    =  ATTBLK ( XMTIDX )
         USELT   =  ATTBLK ( LTIDX  )
         USECN   =  ATTBLK ( CNVIDX )
         USESTL  =  ATTBLK ( STLIDX )

C
C        The variable LOCCOR will contain a representation of
C        the aberration correction specification with stellar
C        aberration omitted.
C         
         IF ( ATTBLK(GEOIDX) ) THEN

            LOCCOR = 'NONE'

         ELSE

            IF ( XMIT ) THEN
               LOCCOR = 'X'
            ELSE
               LOCCOR = ' '
            END IF

            IF ( USECN ) THEN
               CALL SUFFIX ( 'CN', 0, LOCCOR )
            ELSE IF ( USELT ) THEN
               CALL SUFFIX ( 'LT', 0, LOCCOR )
            END IF

         END IF

C
C        At this point, the first pass actions were successful.
C
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
         CALL CHKOUT ( RNAME                                      )
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
         CALL CHKOUT ( RNAME                                      )
         RETURN
      
      END IF
      
 
C
C     Check the input body codes. If they are equal, signal
C     an error.
C
      IF ( OBSCDE .EQ. TRGCDE ) THEN
 
         CALL SETMSG ( 'In computing the surface intercept point, ' //
     .                 'the observing body and target body are the '//
     .                 'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( RNAME                                        )
         RETURN
 
      END IF

C
C     Determine the attributes of the frame designated by FIXREF.
C
      CALL ZZNAMFRM ( SVCTR3, SVFREF, SVFXFC, FIXREF, FXFCDE )

      CALL FRINFO ( FXFCDE, FXCENT, FXCLSS, FXTYID, FND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' //
     .                 'the SPICE frame subsystem. Possibly '    //
     .                 'a required frame definition kernel has ' //
     .                 'not been loaded.'                        )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL SIGERR ( 'SPICE(NOFRAME)'                          )
         CALL CHKOUT ( RNAME                                     )
         RETURN

      END IF

C
C     Make sure that FIXREF is centered at the target body's center.
C
      IF ( FXCENT .NE. TRGCDE ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the ' 
     .   //            'target body #. The ID code of the frame '
     .   //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  FXCENT                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF

C
C     Check for a zero ray direction vector.
C     
      IF ( VZERO(DVEC) ) THEN

         CALL SETMSG ( 'Input ray direction was the zero vector; this '
     .   //            'vector must be non-zero.'                      )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                             )
         CALL CHKOUT ( RNAME                                           )
         RETURN

      END IF

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
C     Determine the position of the observer in target
C     body-fixed coordinates.
C
C         -  Call SPKEZP to compute the position of the target body as
C            seen from the observing body and the light time (LT)
C            between them. We request that the coordinates of POS be
C            returned relative to the body fixed reference frame
C            associated with the target body, using aberration
C            corrections specified by LOCCOR; these are the corrections
C            the input argument ABCORR, minus the stellar aberration
C            correction if it was called for.
C
C         -  Call VMINUS to negate the direction of the vector (OBSPOS)
C            so it will be the position of the observer as seen from
C            the target body in target body fixed coordinates.
C
C            Note that this result is not the same as the result of
C            calling SPKEZP with the target and observer switched. We
C            computed the vector FROM the observer TO the target in
C            order to get the proper light time and stellar aberration
C            corrections (if requested). Now we need the inverse of
C            that corrected vector in order to compute the intercept
C            point.
C

      CALL SPKEZP ( TRGCDE, ET, FIXREF, LOCCOR, OBSCDE, TPOS, LT )
 
C
C     Negate the target's position to obtain the position of the
C     observer relative to the target.
C
      CALL VMINUS ( TPOS, OBSPOS )

C
C     We now need to convert the direction vector into the
C     body fixed frame associated with the target. The target
C     epoch is dependent on the aberration correction. The
C     coefficient S has been set to give us the correct answer
C     for each case.
C
      TRGEPC = ET  +  S*LT

C
C     Determine the attributes of the frame designated by DREF.
C
      CALL ZZNAMFRM ( SVCTR4, SVDREF, SVDFRC, DREF, DFRCDE )

      CALL FRINFO ( DFRCDE, DCENTR, DCLASS, DTYPID, FND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' //
     .                 'the SPICE frame subsystem. Possibly '    //
     .                 'a required frame definition kernel has ' //
     .                 'not been loaded.'                        )
         CALL ERRCH  ( '#',  DREF                                )
         CALL SIGERR ( 'SPICE(NOFRAME)'                          )
         CALL CHKOUT ( RNAME                                     )
         RETURN

      END IF
 
C
C     Transform the direction vector from frame DREF to the body-fixed
C     frame associated with the target. The epoch TRGEPC associated
C     with the body-fixed frame has been set already.
C     
C     We'll compute the transformation in two parts: first
C     from frame DREF to J2000, then from J2000 to the target
C     frame.
C
      IF ( DCLASS .EQ. INERTL ) THEN
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


      ELSE IF ( DCENTR .EQ. OBSCDE ) THEN
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
         CALL SPKEZP ( DCENTR, ET, 'J2000', ABCORR, OBSCDE,
     .                 RPOS,   LTCENT                       )
  
         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
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
C     variable J2DIR. DVEC in the target body-fixed frame will be
C     stored in TRGDIR.
C
C     We may need both versions of DVEC: if we use light time
C     correction, we'll update "intercept epoch", and hence the
C     transformation between J2000 and the target body-fixed frame.
C     The transformation between DREF and J2000 doesn't change, on the
C     other hand, so we don't have to recompute J2DIR. We need TRGDIR
C     in all cases.
C
      CALL PXFORM ( DREF, 'J2000', REFEPC,  R2JMAT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      CALL MXV ( R2JMAT, DVEC, J2DIR )

C
C     Save this version of J2DIR as J2GEOM. Later we'll
C     modify J2DIR, if necessary, to account for stellar
C     aberration. 
C
      CALL VEQU ( J2DIR, J2GEOM )

C
C     Map J2DIR (in the J2000 frame) to the target body-fixed
C     frame.
C
      CALL PXFORM ( 'J2000', FIXREF,  TRGEPC,  J2TMAT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
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
C     the solar system barycenter at ET. We'll use this in
C     several places later.
C
      CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOST )

C
C     If we're using stellar aberration correction, at this point we'll
C     account for it. We're going to find a surface point such that
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
C        We iterate our estimates until we have the desired level
C        of convergence or reach the iteration limit.
C
         NITR = 5

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
C           Now improve our estimate.
C
            RELERR = 1.D0
            I      = 1
            
            DO WHILE ( ( I .LE. NITR ) .AND. ( RELERR .GT. CNVLIM ) )
C
C              Estimate the error in our previous approximation 
C              by applying the reception stellar aberration
C              to STLDIR and finding the difference with J2DIR.
C
               CALL STLABX ( STLDIR, SSBOST(4), J2EST  )            
               CALL VSUB   ( J2DIR,  J2EST,     STLERR )

C
C              Adding the error in the reception mapping to STLDIR
C              will give us an improved estimate of the inverse.
C
               CALL VADD ( STLERR, STLDIR, STLTMP )
               CALL VEQU ( STLTMP,         STLDIR )

               RELERR = VNORM(STLERR) / VNORM(STLDIR)
               I      = I + 1

            END DO

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
C           Now improve our estimate.
C
            RELERR = 1.D0
            I      = 1
            
            DO WHILE ( ( I .LE. NITR ) .AND. ( RELERR .GT. CNVLIM ) )
C
C              Estimate the error in our previous approximation 
C              by applying the reception stellar aberration
C              to STLDIR and finding the difference with J2DIR.
C
               CALL STELAB ( STLDIR, SSBOST(4), J2EST  )            
               CALL VSUB   ( J2DIR,  J2EST,     STLERR )

C
C              Adding the error in the reception mapping to STLDIR
C              will give us an improved estimate of the inverse.
C
               CALL VADD ( STLERR, STLDIR, STLTMP )
               CALL VEQU ( STLTMP,         STLDIR )

               RELERR = VNORM(STLERR) / VNORM(STLDIR)
               I      = I + 1

            END DO

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
C        we've already computed. If we're not using light
C        time correction, this is all we must do. Otherwise,
C        our result will give us an initial estimate of the
C        target epoch, which we'll then improve.
C
C        Get the radii of the target body from the kernel pool.
C
         CALL BODVCD ( TRGCDE, 'RADII', 3, NRADII, RADII )

C
C        Make an easy test to see whether we can quit now because
C        an intercept cannot exist. If the ray is separated from
C        the observer-target center vector by more than (MARGIN *
C        the maximum triaxial radius), we're done. Let REJECT be
C        the angular separation limit.
C        
         MAXRAD = MAX  ( RADII(1), RADII(2), RADII(3) )

         RANGE  = VNORM( OBSPOS )

         IF ( RANGE .EQ. 0.D0 ) THEN
C
C           We've already ensured that observer and target are
C           distinct, so this should be a very unusual occurrence.
C
            CALL SETMSG ( 'Observer-target distance is zero. '
     .      //            'Observer is #; target is #.'       )
            CALL ERRCH  ( '#', OBSRVR                         )
            CALL ERRCH  ( '#', TARGET                         )
            CALL SIGERR ( 'SPICE(NOSEPARATION)'               )
            CALL CHKOUT ( RNAME                               )
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
               CALL CHKOUT ( RNAME )
               RETURN

            END IF

         END IF

C
C        Locate the intercept of the ray with the target; if there's no
C        intercept, find the closest point on the target to the ray.
C
         CALL SURFPT ( OBSPOS, TRGDIR, RADII(1), RADII(2), RADII(3), 
     .                 SPOINT, FOUND                                 )
   
         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        If we found an intercept, and if we're not using light time
C        corrections, we're almost done now. We still need SRFVEC.
C        SPOINT, TRGEPC, and FOUND have already been set.
C        
         IF (  FOUND  .AND.  ( .NOT. USELT )  ) THEN

            CALL VSUB ( SPOINT, OBSPOS, SRFVEC )

            CALL CHKOUT ( RNAME )
            RETURN

         END IF

C
C        From this point onward, we're dealing with a case calling for
C        light time and possibly stellar aberration corrections.
C
         IF ( .NOT. FOUND ) THEN
C
C           If there's no intercept, we're probably done. However,
C           we need to guard against the possibility that the ray does
C           intersect the ellipsoid but we haven't discovered it
C           because our first light time estimate was too poor. 
C
C           We'll make an improved light time estimate as follows:
C           Find the nearest point on the ellipsoid to the ray. Find
C           the light time between the observer and this point.  
C
C           If we're using converged Newtonian corrections, we
C           iterate this procedure up to three times.
C
            IF ( USECN ) THEN
               NITR = 3
            ELSE
               NITR = 1
            END IF

            I = 1

            DO WHILE (  ( I .LE. NITR ) .AND. ( .NOT. FOUND )  )

               CALL NPEDLN ( RADII(1), RADII(2), RADII(3), OBSPOS,  
     .                       TRGDIR,   PNEAR,    RAYALT           )

               LT =  VDIST ( OBSPOS, PNEAR ) / CLIGHT()

C
C              Use the new light time estimate to repeat the intercept 
C              computation.
C
               TRGEPC  =  ET  +  S*LT

C
C              Get the J2000-relative state of the target relative to
C              the solar system barycenter at the target epoch.
C
               CALL SPKSSB ( TRGCDE, TRGEPC, 'J2000', SSBTST )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

C
C              Find the position of the observer relative to the target.
C              Convert this vector from the J2000 frame to the target
C              frame at TRGEPC.
C
               CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
               CALL PXFORM ( 'J2000', FIXREF, TRGEPC, XFORM )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
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
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

         END IF

C
C        Making it to this point means we've got an intersection. 
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
C        Compute new light time estimate and new target epoch.
C
         DIST  =   VDIST ( OBSPOS, SPOINT )
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
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

C
C           Find the position of the observer relative to the target.
C           Convert this vector from the J2000 frame to the target
C           frame at TRGEPC.
C
C           Note that SSBOST contains the J2000-relative state of the
C           observer relative to the solar system barycenter at ET.
C
            CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
            CALL PXFORM ( 'J2000', FIXREF, TRGEPC, XFORM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
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
                CALL CHKOUT ( RNAME )
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
C           can prevent a difference from rounding to zero. Simply
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
         CALL CHKOUT ( RNAME                                           )
         RETURN
         
      END IF

C
C     FOUND, SPOINT, TRGEPC, and OBSPOS have been set at this point.
C     We need SRFVEC. Since OBSPOS doesn't take into account stellar
C     aberration, we can' derive SRFVEC from OBSPOS as is done in
C     the related routines SUBPNT and SUBSLR. Here, we derive
C     SRFVEC from J2GEOM, which is the input ray direction expressed in 
C     the J2000 frame. We use XFORM, which is computed in the loop
C     above, to convert J2GEOM to FIXREF, evaluated at TRGEPC.
C     
      CALL MXV    ( XFORM, J2GEOM, UDIR )
      CALL VHATIP ( UDIR )

C
C     Let SRFLEN be the length of SRFVEC; we CAN get this
C     length from OBSPOS and SPOINT, since stellar 
C     aberration correction (as implemented in SPICE)
C     doesn't change the length of the vector SPOINT-OBSPOS.
C     
      SRFLEN = VDIST ( SPOINT, OBSPOS )

C
C     Scale UDIR to obtain the desired value of SRFVEC.
C
      CALL VSCL ( SRFLEN, UDIR, SRFVEC )
      

      CALL CHKOUT ( RNAME )
      RETURN
      END
