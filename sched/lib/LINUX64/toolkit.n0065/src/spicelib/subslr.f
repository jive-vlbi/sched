C$Procedure SUBSLR ( Sub-solar point )
 
      SUBROUTINE SUBSLR ( METHOD, TARGET, ET,     FIXREF,  
     .                    ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      
C$ Abstract
C
C     Compute the rectangular coordinates of the sub-solar point on
C     a target body at a specified epoch, optionally corrected for
C     light time and stellar aberration.
C
C     This routine supersedes SUBSOL.
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
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SRFVEC ( 3 )
 
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
C     SPOINT     O   Sub-solar point on the target body.
C     TRGEPC     O   Sub-solar point epoch.
C     SRFVEC     O   Vector from observer to sub-solar point.
C
C$ Detailed_Input
C
C     METHOD      is a short string providing parameters defining
C                 the computation method to be used. 
C
C                 The supported values of METHOD are listed below.
C                 Please note that the colon is a required delimiter;
C                 using a blank will not work.
C
C                    'Near point: ellipsoid'   The sub-solar point
C                                              computation uses a
C                                              triaxial ellipsoid to
C                                              model the surface of the
C                                              target body. The
C                                              sub-solar point is
C                                              defined as the nearest
C                                              point on the target
C                                              relative to the Sun.
C
C                    'Intercept: ellipsoid'    The sub-solar point
C                                              computation uses a
C                                              triaxial ellipsoid to
C                                              model the surface of the
C                                              target body. The
C                                              sub-solar point is
C                                              defined as the target
C                                              surface intercept of the
C                                              line containing the Sun
C                                              and the target's center.
C
C                 Neither case nor white space are significant in
C                 METHOD. For example, the string 
C
C                   ' nearpoint:ELLIPSOID '
C
C                 is valid.
C
C                 
C     TARGET      is the name of the target body. The target body is 
C                 an ephemeris object (its trajectory is given by
C                 SPK data), and is an extended object.
C
C                 The string TARGET is case-insensitive, and leading
C                 and trailing blanks in TARGET are not significant.
C                 Optionally, you may supply a string containing the
C                 integer ID code for the object. For example both
C                 'MOON' and '301' are legitimate strings that indicate
C                 the Moon is the target body.
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
C                 the epoch at which the position and orientation of
C                 the target body and the position of the Sun are
C                 computed.
C
C                 When aberration corrections are used, ET is the epoch
C                 at which the observer's state relative to the solar
C                 system barycenter is computed; in this case the
C                 position and orientation of the target body are
C                 computed at ET-LT, where LT is the one-way light time
C                 between the sub-solar point and the observer. See the
C                 description of ABCORR below for details.
C
C
C     FIXREF      is the name of the body-fixed, body-centered
C                 reference frame associated with the target body. The
C                 output sub-solar point SPOINT will be expressed
C                 relative to this reference frame. The string FIXREF
C                 is case-insensitive, and leading and trailing blanks
C                 in FIXREF are not significant.
C
C                 
C     ABCORR      indicates the aberration correction to be applied
C                 when computing the target position and orientation
C                 and the position of the Sun.
C
C                 For remote sensing applications, where the apparent
C                 sub-solar point seen by the observer is desired,
C                 normally either of the corrections
C              
C                    'LT+S' 
C                    'CN+S'
C     
C                 should be used. These and the other supported options
C                 are described below. ABCORR may be any of the 
C                 following:
C
C                    'NONE'     Apply no correction. Return the
C                               geometric sub-solar point on the target
C                               body.
C
C                 Let LT represent the one-way light time between the
C                 observer and the sub-solar point (note: NOT between
C                 the observer and the target body's center). The
C                 following values of ABCORR apply to the "reception"
C                 case in which photons depart from the sub-solar
C                 point's location at the light-time corrected epoch
C                 ET-LT and *arrive* at the observer's location at ET:
C
C                    'LT'       Correct for one-way light time (also
C                               called "planetary aberration") using a
C                               Newtonian formulation. This correction
C                               yields the location of sub-solar
C                               point at the moment it emitted photons
C                               arriving at the observer at ET.
C 
C                               The light time correction uses an
C                               iterative solution of the light time
C                               equation. The solution invoked by the
C                               'LT' option uses one iteration.
C
C                               The target position and orientation as
C                               seen by the observer are corrected for
C                               light time. The position of the Sun
C                               relative to the target is corrected for
C                               one-way light time between the Sun and
C                               target.
C
C                    'LT+S'     Correct for one-way light time and
C                               stellar aberration using a Newtonian
C                               formulation. This option modifies the
C                               sub-solar point obtained with the 'LT'
C                               option to account for the observer's
C                               velocity relative to the solar system
C                               barycenter. These corrections yield
C                               the apparent sub-solar point.
C
C                    'CN'       Converged Newtonian light time
C                               correction. In solving the light time
C                               equation, the 'CN' correction iterates
C                               until the solution converges. Both the
C                               position and rotation of the target
C                               body, and the position of the Sun, are
C                               corrected for light time.
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
C                 Neither case nor white space are significant in
C                 ABCORR. For example, the string 
C
C                   'Lt + s'
C
C                 is valid.
C
C
C     OBSRVR      is the name of the observing body. The observing body
C                 is an ephemeris object: it typically is a spacecraft,
C                 the earth, or a surface point on the earth. OBSRVR is
C                 case-insensitive, and leading and trailing blanks in
C                 OBSRVR are not significant. Optionally, you may
C                 supply a string containing the integer ID code for
C                 the object. For example both 'MOON' and '301' are
C                 legitimate strings that indicate the Moon is the
C                 observer.
C
C                 The observer may coincide with the target.
C
C$ Detailed_Output
C
C
C     SPOINT      is the sub-solar point on the target body. 
C
C                 The sub-solar point is defined either as the point
C                 on the target body that is closest to the Sun,
C                 or the target surface intercept of the line from the
C                 Sun to the target's center; the input argument
C                 METHOD selects the definition to be used. 
C 
C                 SPOINT is expressed in Cartesian coordinates,
C                 relative to the body-fixed target frame designated by
C                 FIXREF. The body-fixed target frame is evaluated at
C                 the sub-solar point epoch TRGEPC (see description
C                 below).
C
C                 When aberration corrections are used, SPOINT is
C                 computed using target body position and orientation
C                 that have been adjusted for the corrections
C                 applicable to SPOINT itself rather than to the target
C                 body's center. In particular, if the stellar
C                 aberration correction applicable to SPOINT is
C                 represented by a shift vector S, then the light-time
C                 corrected position of the target is shifted by S
C                 before the sub-solar point is computed.
C                 
C                 The components of SPOINT have units of km.
C
C
C     TRGEPC      is the "sub-solar point epoch." TRGEPC is defined as
C                 follows: letting LT be the one-way light time between
C                 the observer and the sub-solar point, TRGEPC is
C                 either the epoch ET-LT or ET depending on whether the
C                 requested aberration correction is, respectively, for
C                 received radiation or omitted. LT is computed using
C                 the method indicated by ABCORR.
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
C     3)  If the input target body-fixed frame FIXREF is not
C         recognized, the error SPICE(NOFRAME) is signaled. A frame
C         name may fail to be recognized because a required frame
C         specification kernel has not been loaded; another cause is a
C         misspelling of the frame name.
C
C     4)  If the input frame FIXREF is not centered at the target body,
C         the error SPICE(INVALIDFRAME) is signaled.
C
C     5)  If the input argument METHOD is not recognized, the error
C         SPICE(INVALIDMETHOD) is signaled.
C
C     6)  If insufficient ephemeris data have been loaded prior to
C         calling SUBSLR, the error will be diagnosed and signaled by a
C         routine in the call tree of this routine. Note that when
C         light time correction is used, sufficient ephemeris data must
C         be available to propagate the states of observer, target, and
C         the Sun to the solar system barycenter.
C
C     7)  If the computation method specifies an ellipsoidal target
C         shape and triaxial radii of the target body have not been
C         loaded into the kernel pool prior to calling SUBSLR, the
C         error will be diagnosed and signaled by a routine in the call
C         tree of this routine.
C
C     8)  The target must be an extended body: if any of the radii of
C         the target body are non-positive, the error will be
C         diagnosed and signaled by routines in the call tree of this
C         routine.
C
C     9)  If PCK data specifying the target body-fixed frame
C         orientation have not been loaded prior to calling SUBSLR,
C         the error will be diagnosed and signaled by a routine in the
C         call tree of this routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target, observer, and
C          Sun must be loaded. If aberration corrections are used, the
C          states of target, observer, and the Sun relative to the
C          solar system barycenter must be calculable from the
C          available ephemeris data. Typically ephemeris data are made
C          available by loading one or more SPK files via FURNSH.
C
C        - PCK data: if the target body shape is modeled as an
C          ellipsoid, triaxial radii for the target body must be loaded
C          into the kernel pool. Typically this is done by loading a
C          text PCK file via FURNSH.
C
C        - Further PCK data: rotation data for the target body must be
C          loaded. These may be provided in a text or binary PCK file.
C
C        - Frame data: if a frame definition is required to convert the
C          observer and target states to the body-fixed frame of the
C          target, that definition must be available in the kernel
C          pool. Typically the definition is supplied by loading a
C          frame kernel via FURNSH.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     There are two different popular ways to define the sub-solar
C     point: "nearest point on target to the Sun" or "target surface
C     intercept of the line containing the Sun and target." These
C     coincide when the target is spherical and generally are distinct
C     otherwise.
C
C     This routine computes light time corrections using light time
C     between the observer and the sub-solar point, as opposed to the
C     center of the target. Similarly, stellar aberration corrections
C     done by this routine are based on the direction of the vector
C     from the observer to the light-time corrected sub-solar point,
C     not to the target center. This technique avoids errors due to the
C     differential between aberration corrections across the target
C     body. Therefore it's valid to use aberration corrections with
C     this routine even when the observer is very close to the
C     sub-solar point, in particular when the observer to sub-solar
C     point distance is much less than the observer to target center
C     distance.
C
C     The definition of the aberration-corrected sub-solar point is
C     implicit: SPOINT is defined by an equation of the general form
C
C        SPOINT = F ( SPOINT )
C
C     Because of the contraction properties of both light time and
C     stellar aberration corrections---that is, the difference in the
C     corrections for two vectors is much smaller than the difference
C     between the vectors themselves---it's easy to solve this equation
C     accurately and fairly quickly.
C     
C     When comparing sub-solar point computations with results from
C     sources other than SPICE, it's essential to make sure the same
C     geometric definitions are used.
C
C$ Examples
C
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C
C     1) Find the sub-solar point on Mars as seen from the Earth for a
C        specified time. Perform the computation twice, using both the
C        "intercept" and "near point" options. Display the locations of
C        the Sun and the sub-solar point using both planetocentric
C        and planetographic coordinates.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
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
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00010.tpc',
C                                  'naif0010.tls'  )
C
C           \begintext
C
C
C       Example code begins here.
C
C
C           PROGRAM EX1
C           IMPLICIT NONE
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      DPR
C     C
C     C     Local parameters
C     C
C           CHARACTER*(*)         META
C           PARAMETER           ( META   = 'subslr.tm' )
C
C           CHARACTER*(*)         FM
C           PARAMETER           ( FM     =  '(A,F21.9)' )
C
C           INTEGER               MTHLEN
C           PARAMETER           ( MTHLEN = 50 )
C     C
C     C     Local variables
C     C
C           CHARACTER*(MTHLEN)    METHOD ( 2 )
C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      F
C           DOUBLE PRECISION      RADII  ( 3 )
C           DOUBLE PRECISION      RE
C           DOUBLE PRECISION      RP
C           DOUBLE PRECISION      SPCLAT
C           DOUBLE PRECISION      SPCLON
C           DOUBLE PRECISION      SPCRAD
C           DOUBLE PRECISION      SPGALT
C           DOUBLE PRECISION      SPGLAT
C           DOUBLE PRECISION      SPGLON
C           DOUBLE PRECISION      SPOINT ( 3 )
C           DOUBLE PRECISION      SRFVEC ( 3 )
C           DOUBLE PRECISION      SUNLT
C           DOUBLE PRECISION      SUNPOS ( 3 )
C           DOUBLE PRECISION      SUNST  ( 6 )
C           DOUBLE PRECISION      SUPCLN
C           DOUBLE PRECISION      SUPCLT
C           DOUBLE PRECISION      SUPCRD
C           DOUBLE PRECISION      SUPGAL
C           DOUBLE PRECISION      SUPGLN
C           DOUBLE PRECISION      SUPGLT
C           DOUBLE PRECISION      TRGEPC
C
C           INTEGER               I
C           INTEGER               N
C     C
C     C     Saved variables
C     C
C           SAVE                  METHOD
C     C
C     C     Initial values
C     C
C           DATA                  METHOD / 'Intercept:  ellipsoid',
C          .                               'Near point: ellipsoid' /
C     C
C     C     Load kernel files via the meta-kernel.
C     C
C           CALL FURNSH ( META )
C
C     C
C     C     Convert the UTC request time to ET (seconds past
C     C     J2000, TDB).
C     C
C           CALL STR2ET ( '2008 AUG 11 00:00:00', ET )
C
C     C
C     C     Look up the target body's radii. We'll use these to
C     C     convert Cartesian to planetographic coordinates. Use
C     C     the radii to compute the flattening coefficient of
C     C     the reference ellipsoid.
C     C
C           CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII )
C
C     C
C     C     Let RE and RP be, respectively, the equatorial and
C     C     polar radii of the target.
C     C
C           RE = RADII( 1 )
C           RP = RADII( 3 )
C
C           F  = ( RE - RP ) / RE
C
C     C
C     C     Compute the sub-solar point using light time and stellar
C     C     aberration corrections. Use the "target surface intercept"
C     C     definition of sub-solar point on the first loop
C     C     iteration, and use the "near point" definition on the
C     C     second.
C     C
C           DO I = 1, 2
C
C              CALL SUBSLR ( METHOD(I),
C          .                'MARS',  ET,     'IAU_MARS', 'LT+S',
C          .                'EARTH', SPOINT, TRGEPC,     SRFVEC )
C     C
C     C        Convert the sub-solar point's rectangular coordinates
C     C        to planetographic longitude, latitude and altitude.
C     C        Convert radians to degrees.
C     C
C              CALL RECPGR ( 'MARS', SPOINT, RE,    F,
C          .                 SPGLON, SPGLAT, SPGALT   )
C
C              SPGLON = SPGLON * DPR ()
C              SPGLAT = SPGLAT * DPR ()
C
C     C
C     C        Convert sub-solar point's rectangular coordinates to
C     C        planetocentric radius, longitude, and latitude. Convert
C     C        radians to degrees.
C     C
C              CALL RECLAT ( SPOINT, SPCRAD, SPCLON, SPCLAT )
C
C              SPCLON = SPCLON * DPR ()
C              SPCLAT = SPCLAT * DPR ()
C
C     C
C     C        Compute the Sun's apparent position relative to the
C     C        sub-solar point at TRGEPC. Add the position of
C     C        the sub-solar point relative to the target's center
C     C        to obtain the position of the sun relative to the
C     C        target's center. Express the latter position in
C     C        planetographic coordinates.
C     C
C              CALL SPKCPO ( 'SUN',  TRGEPC, 'IAU_MARS', 'OBSERVER',
C          .                 'LT+S', SPOINT, 'MARS',     'IAU_MARS',
C          .                 SUNST,  SUNLT                            )
C
C              CALL VADD ( SUNST, SPOINT, SUNPOS )
C
C              CALL RECPGR ( 'MARS', SUNPOS, RE,    F,
C          .                 SUPGLN, SUPGLT, SUPGAL   )
C
C              SUPGLN = SUPGLN * DPR ()
C              SUPGLT = SUPGLT * DPR ()
C
C     C
C     C        Convert the Sun's rectangular coordinates to
C     C        planetocentric radius, longitude, and latitude.
C     C        Convert radians to degrees.
C     C
C              CALL RECLAT ( SUNPOS, SUPCRD, SUPCLN, SUPCLT )
C
C              SUPCLN = SUPCLN * DPR ()
C              SUPCLT = SUPCLT * DPR ()
C
C     C
C     C        Write the results.
C     C
C              WRITE(*,FM) ' '
C              WRITE(*,* ) 'Computation method = ', METHOD(I)
C              WRITE(*,FM) ' '
C              WRITE(*,FM)
C          .   '  Sub-solar point altitude            (km) = ', SPGALT
C              WRITE(*,FM)
C          .   '  Sub-solar planetographic longitude (deg) = ', SPGLON
C              WRITE(*,FM)
C          .   '  Sun''s planetographic longitude     (deg) = ', SUPGLN
C              WRITE(*,FM)
C          .   '  Sub-solar planetographic latitude  (deg) = ', SPGLAT
C              WRITE(*,FM)
C          .   '  Sun''s planetographic latitude      (deg) = ', SUPGLT
C              WRITE(*,FM)
C          .   '  Sub-solar planetocentric longitude (deg) = ', SPCLON
C              WRITE(*,FM)
C          .   '  Sun''s planetocentric longitude     (deg) = ', SUPCLN
C              WRITE(*,FM)
C          .   '  Sub-solar planetocentric latitude  (deg) = ', SPCLAT
C              WRITE(*,FM)
C          .   '  Sun''s planetocentric latitude      (deg) = ', SUPCLT
C              WRITE(*,FM) ' '
C
C           END DO
C
C           END
C
C
C     When this program was executed on a PC/Linux/gfortran platform,
C     the output was:
C
C
C      Computation method = Intercept:  ellipsoid
C
C       Sub-solar point altitude            (km) =           0.000000000
C       Sub-solar planetographic longitude (deg) =         175.810675510
C       Sun's planetographic longitude     (deg) =         175.810675508
C       Sub-solar planetographic latitude  (deg) =          23.668550281
C       Sun's planetographic latitude      (deg) =          23.420823362
C       Sub-solar planetocentric longitude (deg) =        -175.810675510
C       Sun's planetocentric longitude     (deg) =        -175.810675508
C       Sub-solar planetocentric latitude  (deg) =          23.420819936
C       Sun's planetocentric latitude      (deg) =          23.420819936
C
C
C      Computation method = Near point: ellipsoid
C
C       Sub-solar point altitude            (km) =           0.000000000
C       Sub-solar planetographic longitude (deg) =         175.810675410
C       Sun's planetographic longitude     (deg) =         175.810675408
C       Sub-solar planetographic latitude  (deg) =          23.420823362
C       Sun's planetographic latitude      (deg) =          23.420823362
C       Sub-solar planetocentric longitude (deg) =        -175.810675410
C       Sun's planetocentric longitude     (deg) =        -175.810675408
C       Sub-solar planetocentric latitude  (deg) =          23.175085578
C       Sun's planetocentric latitude      (deg) =          23.420819936
C
C
C$ Restrictions
C
C    None.
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
C        Bug fix: stellar aberration is no longer applied to the
C        observer-to-estimated sub-solar point vector while solving for
C        the sub-solar point. This correction involved unnecessary code
C        but did not affect this routine's outputs.
C
C        Bug fix: FIRST is now set to .FALSE. at the completion of a
C        successful initialization pass. This does not affect the
C        routine's outputs but improves efficiency.
C
C        Exceptions removed: the observer and target are now 
C        permitted to coincide.
C
C        Upgrade: the algorithm for finding the apparent state of the
C        sun as seen from the estimated sub-solar point has been
C        improved.
C
C        Upgrade: this routine now uses ZZVALCOR rather than ZZPRSCOR,
C        simplifying the implementation.
C
C        The header example program was updated to reflect the new
C        method of computing the apparent sun location, and the set
C        of kernels referenced by the example meta-kernel were updated.
C        The display of the program's output was updated accordingly.
C
C        References to the new PXFRM2 routine were added, which changed
C        the Detailed Output section.
C
C        Updated to save the input body names and ZZBODTRN state
C        counters and to do name-ID conversions only if the counters
C        have changed.
C
C        Updated to save the input frame name and POOL state counter
C        and to do frame name-ID conversion only if the counter has
C        changed.
C
C        Updated to call LJUCRS instead of CMPRSS/UCASE. 
C
C-    SPICELIB Version 1.1.0, 18-MAY-2010 (NJB) 
C
C        Bug fix: calls to FAILED() have been added after
C        SPK calls, target radius lookup, near point
C        and surface intercept computations.
C
C-    SPICELIB Version 1.0.1, 17-MAR-2009 (NJB) 
C
C        Typo correction: changed FIXFRM to FIXREF in header
C        documentation. Meta-kernel name suffix was changed to
C        ".tm" in header code example.
C
C        Typo correction in Required_Reading, changed 
C        FRAME to FRAMES.
C
C-    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) 
C
C-&
 
C$ Index_Entries
C
C     find sub-solar point on target body
C     find nearest point to sun on target body
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
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDIST
      
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  =  'SUBSLR' )

C
C     This value will become system-dependent when systems
C     using 128-bit d.p. numbers are supported by SPICELIB.
C     CNVLIM, when added to 1.0D0, should yield 1.0D0. 
C
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-17 )
     

      INTEGER               MAXITR
      PARAMETER           ( MAXITR =  10 )

      INTEGER               MAXW
      PARAMETER           ( MAXW   =  2 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               WORDLN
      PARAMETER           ( WORDLN = 32 )

      INTEGER               SUN
      PARAMETER           ( SUN    = 10 )

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
      CHARACTER*(LNSIZE)    LOCMTH
      CHARACTER*(CORLEN)    PRVCOR
      CHARACTER*(LNSIZE)    PRVMTH
      CHARACTER*(WORDLN)    WORDS  ( MAXW )

 
      DOUBLE PRECISION      ALTSUN
      DOUBLE PRECISION      ETDIFF
      DOUBLE PRECISION      J2POS  ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTDIFF
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      OBSRNG
      DOUBLE PRECISION      PREVET
      DOUBLE PRECISION      PREVLT
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SDIR   ( 3 )
      DOUBLE PRECISION      SLT
      DOUBLE PRECISION      SPOS   ( 3 )
      DOUBLE PRECISION      SSBOST ( 6 )
      DOUBLE PRECISION      SSBTST ( 6 )
      DOUBLE PRECISION      SSLRLT
      DOUBLE PRECISION      SSLRST ( 6 )
      DOUBLE PRECISION      SUNST  ( 6 )
      DOUBLE PRECISION      TPOS   ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
 
      INTEGER               CENTER
      INTEGER               I
      INTEGER               NITR
      INTEGER               NRADII
      INTEGER               NW
      INTEGER               OBSCDE
      INTEGER               REFCDE
      INTEGER               TRGCDE
      INTEGER               TYPE
      INTEGER               TYPEID
     
      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               ELIPSD
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               NEAR
      LOGICAL               USECN
      LOGICAL               USELT
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
      INTEGER               SVREFC


 

C
C     Saved variables
C
      SAVE                  ELIPSD
      SAVE                  FIRST
      SAVE                  NEAR
      SAVE                  PRVCOR
      SAVE                  PRVMTH
      SAVE                  USECN
      SAVE                  USELT
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
      SAVE                  SVREFC



C
C     Initial values
C
      DATA                  ELIPSD  / .TRUE. /
      DATA                  FIRST   / .TRUE. /
      DATA                  NEAR    / .TRUE. /
      DATA                  PRVCOR  / ' '    /
      DATA                  PRVMTH  / 'Near point: Ellipsoid' /
 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )
  
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
C        The aberration correction flag is recognized; save it.
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
C        The above definitions are consistent with those used by
C        ZZPRSCOR.
C 
         XMIT    =  ATTBLK ( XMTIDX )
         USELT   =  ATTBLK ( LTIDX  )
         USECN   =  ATTBLK ( CNVIDX )

C
C        Reject an aberration correction flag calling for transmission
C        corrections.
C
         IF ( XMIT ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for '
     .      //            'transmission-style corrections.'        )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         END IF

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
C     Determine the attributes of the frame designated by FIXREF.
C
      CALL ZZNAMFRM ( SVCTR3, SVFREF, SVREFC, FIXREF, REFCDE )

      CALL FRINFO ( REFCDE, CENTER, TYPE, TYPEID, FND )

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
      IF ( CENTER .NE. TRGCDE ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the ' 
     .   //            'target body #. The ID code of the frame '
     .   //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  CENTER                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF

C
C     If necessary, parse the method specification. PRVMTH
C     and the derived flags NEAR and ELIPSD start out with
C     valid values. PRVMTH records the last valid value of
C     METHOD; NEAR and ELIPSD are the corresponding flags.
C
      IF ( METHOD .NE. PRVMTH ) THEN
C
C        Parse the computation method specification. Work with a local
C        copy of the method specification that contains no leading or
C        embedded blanks.
C
         CALL LJUCRS ( 0, METHOD, LOCMTH )
  
         CALL LPARSE ( LOCMTH, ':', MAXW, NW, WORDS )

         IF ( NW .NE. 2 ) THEN

            CALL SETMSG ( 'Computation method argument was <#>; this ' 
     .      //            'string must specify a supported shape '   
     .      //            'model and computation type. See the '
     .      //            'header of SUBSLR for details.'            )
            CALL ERRCH  ( '#',  METHOD                               )
            CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                     )
            CALL CHKOUT ( RNAME                                      )
            RETURN

         END IF
 
C
C        The text preceding the first delimiter indicates the
C        sub-observer point definition: "nearpoint" or "intercept." The
C        second word designates the target shape model. Recall that
C        we've removed all blanks from the input string, so we won't
C        see the string "near point."
C
C        Check the sub-observer point definition.
C
         IF (       ( WORDS(1) .NE. 'NEARPOINT' ) 
     .        .AND. ( WORDS(1) .NE. 'INTERCEPT' )  ) THEN 

            CALL SETMSG ( 'Computation method argument was <#>; this ' 
     .      //            'string must specify a supported shape '
     .      //            'model and computation type. See the '
     .      //            'header of SUBSLR for details.'            )
            CALL ERRCH  ( '#',  METHOD                               )
            CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                     )
            CALL CHKOUT ( RNAME                                      )
            RETURN

         END IF

C
C        Check the shape specification.
C
         IF ( WORDS(2) .NE. 'ELLIPSOID' ) THEN

            CALL SETMSG ( 'Computation method argument was <#>; this ' 
     .      //            'string must specify a supported shape '
     .      //            'model and computation type. See the '
     .      //            'header of SUBSLR for details.'            )
            CALL ERRCH  ( '#',  METHOD                               )
            CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                     )
            CALL CHKOUT ( RNAME                                      )
            RETURN

         END IF

C
C        At this point the method specification has passed our tests.
C        Use the flag NEAR to indicate whether the computation type is
C        "near point." Use the flag ELIPSD to indicate that the shape
C        is modeled as an ellipsoid (which is true, for now).
C
         NEAR   = WORDS(1) .EQ. 'NEARPOINT'
         ELIPSD = .TRUE.

C
C        Save the current value of METHOD. 
C
         PRVMTH = METHOD
         
      END IF

C
C     Get the sign S prefixing LT in the expression for TRGEPC.
C     When light time correction is not used, setting S = 0
C     allows us to seamlessly set TRGEPC equal to ET.
C
      IF ( USELT ) THEN
         S = -1.D0         
      ELSE
         S =  0.D0
      END IF
 
C
C     Determine the position of the observer in target body-fixed
C     coordinates. This is a first estimate.
C
C         -  Call SPKEZP to compute the position of the target body as
C            seen from the observing body and the light time (LT)
C            between them. We request that the coordinates of POS be
C            returned relative to the body fixed reference frame
C            associated with the target body, using aberration
C            corrections specified by the input argument ABCORR.
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
C            that corrected vector in order to compute the sub-solar
C            point.
C
      CALL SPKEZP ( TRGCDE, ET, FIXREF, ABCORR, OBSCDE, TPOS, LT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C
C     Negate the target's position to obtain the position of the
C     observer relative to the target.
C
      CALL VMINUS ( TPOS, OBSPOS )

C
C     Make a first estimate of the target epoch.
C
      TRGEPC = ET + S*LT

C
C     Find the sub-solar point and distance from observer to 
C     sub-solar point using the specified geometric definition.
C
      IF ( ELIPSD ) THEN 
C
C        Find the sub-solar point given the target epoch,
C        observer-target position, and target body orientation
C        we've already computed. If we're not using light
C        time correction, this is all we need do. Otherwise,
C        our result will give us an initial estimate of the
C        target epoch, which we'll then improve.
C
C        Get the radii of the target body from the kernel pool.
C
         CALL BODVCD ( TRGCDE, 'RADII', 3, NRADII, RADII )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        Get the position of the Sun SPOS as seen from the target
C        in the target body-fixed frame at TRGEPC. 
C
         CALL SPKEZP ( SUN, TRGEPC, FIXREF, ABCORR, TRGCDE, SPOS, SLT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        Make a first estimate of the sub-solar point. The algorithm
C        we use depends on the sub-solar point definition.
C
         IF ( NEAR ) THEN
C
C           Locate the nearest point to the Sun on the target.
C
            CALL NEARPT ( SPOS,   RADII(1), RADII(2), RADII(3),
     .                    SPOINT, ALTSUN                       )

         ELSE
C
C           Locate the surface intercept of the ray from the 
C           Sun to the target center.
C
            CALL VMINUS ( SPOS,   SDIR )
            CALL SURFPT ( SPOS,   SDIR, RADII(1), RADII(2), RADII(3), 
     .                    SPOINT, FND                                )
               
            IF ( .NOT. FND ) THEN
C
C              If there's no intercept, we have a numerical problem.
C              
               CALL SETMSG ( 'No intercept of observer-target '
     .         //            'ray was found.'                  )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'           )
               CALL CHKOUT ( RNAME                             )
               RETURN

            END IF

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         OBSRNG = VDIST ( OBSPOS, SPOINT )

C
C        Compute the one-way light time and target epoch based on our
C        first computation of SPOINT. The coefficient S has been
C        set to give us the correct answer for each aberration
C        correction case.
C
         LT     = OBSRNG / CLIGHT()
         TRGEPC = ET  + S*LT

C
C        If we're not using light time and stellar aberration
C        corrections, we're almost done now. Note that we need only
C        check for use of light time corrections, because use of
C        stellar aberration corrections alone has been prevented by an
C        earlier check.
C        
         IF ( .NOT. USELT ) THEN
C
C           The TRGEPC value we'll return comes from our value of
C           OBSRNG computed above. The previous call to SPKEZP call
C           yielded the vector OBSPOS. SPOINT was set immediately
C           above. The only output left to compute is SRFVEC.
C
            CALL VSUB ( SPOINT, OBSPOS, SRFVEC )

            CALL CHKOUT ( RNAME )
            RETURN

         END IF

C
C        We'll now make an improved sub-solar point estimate using the
C        previous estimate of the sub-solar point. The number of
C        iterations depends on the light time correction type.
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

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        Initialize the variables required to evaluate the 
C        loop termination condition.
C
         I      = 0
         LTDIFF = 1.D0
         ETDIFF = 1.D0
         PREVLT = LT
         PREVET = TRGEPC

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
            CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
            CALL PXFORM ( 'J2000', FIXREF, TRGEPC, XFORM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXV ( XFORM, J2POS, OBSPOS )

C
C           Note: if we're using stellar aberration correction, we do
C           not apply the stellar aberration correction of the
C           estimated sub-solar point as seen by the observer to the
C           next estimate of the sun-solar point. The location of this
C           point depends on the illumination of the target, which is a
C           function of the observer-surface point light time. This is
C           the only way in which the observer plays a role in
C           determining the sub-solar point.
C
C           Stellar aberration of the sun's position relative to the
C           sub-solar point *is* used.
C
C           First find the apparent position of the sun
C           as seem from the estimated sub-solar point.
C 
            CALL SPKCPO ( 'SUN',  TRGEPC, FIXREF, 'OBSERVER',
     .                    ABCORR, SPOINT, TARGET, FIXREF, SUNST, SLT )

C
C           Create the target-center to sun vector.
C
            CALL VADD ( SUNST, SPOINT, SPOS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF
C
C           Find the sub-solar point using the current estimated
C           geometry.
C
            IF ( NEAR ) THEN
C
C              Locate the nearest point to the observer on the target.
C
               CALL NEARPT ( SPOS,   RADII(1), RADII(2), RADII(3),
     .                       SPOINT, ALTSUN                        )
            ELSE
C
C              Locate the surface intercept of the ray from the 
C              Sun to the target center.
C
               CALL VMINUS ( SPOS,     SDIR )
               CALL SURFPT ( SPOS,     SDIR,   RADII(1), RADII(2), 
     .                       RADII(3), SPOINT, FND                )
                  
               IF ( .NOT. FND ) THEN
C
C                 If there's no intercept, we have a numerical problem.
C              
                  CALL SETMSG ( 'No intercept of observer-target '
     .            //            'ray was found.'                  )
                  CALL SIGERR ( 'SPICE(DEGENERATECASE)'           )
                  CALL CHKOUT ( RNAME                             )
                  RETURN

               END IF

            END IF

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            OBSRNG = VDIST ( OBSPOS, SPOINT )

C
C           Compute a new light time estimate and new target epoch.
C
            LT      =  OBSRNG / CLIGHT()
            TRGEPC  =  ET  + S*LT

C
C           At this point, we have new estimates of the sub-solar point
C           SPOINT, the observer altitude OBSRNG, the target epoch
C           TRGEPC, and the position of the observer relative to the
C           target OBSPOS.
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
C
C        We've already checked the computation method input argument,
C        so we don't expect to arrive here. This code is present for
C        safety.
C      
         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( RNAME                                           )
         RETURN
         
      END IF

C
C     SPOINT and TRGEPC have been set. Compute SRFVEC.
C     
      CALL SPKCPT ( SPOINT,   TARGET, FIXREF, ET,     FIXREF,
     .              'TARGET', ABCORR, OBSRVR, SSLRST, SSLRLT )

      CALL VEQU ( SSLRST, SRFVEC )

      CALL CHKOUT ( RNAME )
      RETURN
      END
