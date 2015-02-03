C$Procedure      SUBPNT ( Sub-observer point )
 
      SUBROUTINE SUBPNT ( METHOD, TARGET, ET,     FIXREF,  
     .                    ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      
C$ Abstract
C
C     Compute the rectangular coordinates of the sub-observer point on
C     a target body at a specified epoch, optionally corrected for
C     light time and stellar aberration.
C
C     This routine supersedes SUBPT.
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
C     SPOINT     O   Sub-observer point on the target body.
C     TRGEPC     O   Sub-observer point epoch.
C     SRFVEC     O   Vector from observer to sub-observer point.
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
C                    'Near point: ellipsoid'   The sub-observer point
C                                              computation uses a
C                                              triaxial ellipsoid to
C                                              model the surface of the
C                                              target body. The
C                                              sub-observer point is
C                                              defined as the nearest
C                                              point on the target
C                                              relative to the
C                                              observer. 
C
C                    'Intercept: ellipsoid'    The sub-observer point
C                                              computation uses a
C                                              triaxial ellipsoid to
C                                              model the surface of the
C                                              target body. The
C                                              sub-observer point is
C                                              defined as the target
C                                              surface intercept of the
C                                              line containing the
C                                              observer and the
C                                              target's center.
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
C                 the target body are computed.
C
C                 When aberration corrections are used, the position
C                 and orientation of the target body are computed at
C                 ET-LT or ET+LT, where LT is the one-way light time
C                 between the sub-observer point and the observer, and
C                 the sign applied to LT depends on the selected
C                 correction. See the description of ABCORR below for
C                 details.
C
C
C     FIXREF      is the name of the body-fixed, body-centered
C                 reference frame associated with the target body. 
C                 The output sub-observer point SPOINT will be 
C                 expressed relative to this reference frame.
C                 The string FIXREF is case-insensitive, and leading
C                 and trailing blanks in FIXREF are not significant.
C                 
C
C     ABCORR      indicates the aberration corrections to be applied
C                 when computing the target's position and orientation.
C         
C                 For remote sensing applications, where the apparent
C                 sub-observer point seen by the observer is desired,
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
C                               geometric sub-observer point on the
C                               target body.
C
C                 Let LT represent the one-way light time between the
C                 observer and the sub-observer point (note: NOT
C                 between the observer and the target body's center).
C                 The following values of ABCORR apply to the
C                 "reception" case in which photons depart from the
C                 sub-observer point's location at the light-time
C                 corrected epoch ET-LT and *arrive* at the observer's
C                 location at ET:
C
C
C                    'LT'       Correct for one-way light time (also
C                               called "planetary aberration") using a
C                               Newtonian formulation. This correction
C                               yields the location of sub-observer
C                               point at the moment it emitted photons
C                               arriving at the observer at ET.
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
C                               sub-observer point obtained with the
C                               'LT' option to account for the
C                               observer's velocity relative to the
C                               solar system barycenter. These
C                               corrections yield the apparent
C                               sub-observer point.
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
C                               with the `LT+S' option. Whether the
C                               'CN+S' solution is substantially more
C                               accurate depends on the geometry of the
C                               participating objects and on the
C                               accuracy of the input data. In all
C                               cases this routine will execute more
C                               slowly when a converged solution is
C                               computed.
C
C
C                 The following values of ABCORR apply to the
C                 "transmission" case in which photons *depart* from
C                 the observer's location at ET and arrive at the
C                 sub-observer point at the light-time corrected epoch
C                 ET+LT:
C
C                    'XLT'      "Transmission" case: correct for
C                               one-way light time using a Newtonian
C                               formulation. This correction yields the
C                               sub-observer location at the moment it
C                               receives photons emitted from the
C                               observer's location at ET. 
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
C                    'XLT+S'    "Transmission" case: correct for
C                               one-way light time and stellar
C                               aberration using a Newtonian
C                               formulation  This option modifies the
C                               sub-observer point obtained with the
C                               'XLT' option to account for the
C                               observer's velocity relative to the
C                               solar system barycenter.
C
C                    'XCN'      Converged Newtonian light time
C                               correction. This is the same as 'XLT'
C                               correction but with further iterations
C                               to a converged Newtonian light time
C                               solution. 
C
C                    'XCN+S'    "Transmission" case: converged 
C                               Newtonian light time and stellar 
C                               aberration corrections.
C
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
C$ Detailed_Output
C
C
C     SPOINT      is the sub-observer point on the target body.
C
C                 The sub-observer point is defined either as the point
C                 on the target body that is closest to the observer,
C                 or the target surface intercept of the line from the
C                 observer to the target's center; the input argument
C                 METHOD selects the definition to be used. 
C
C                 SPOINT is expressed in Cartesian coordinates,
C                 relative to the body-fixed target frame designated by
C                 FIXREF. The body-fixed target frame is evaluated at
C                 the sub-observer epoch TRGEPC (see description below).
C
C                 When light time correction is used, the duration of
C                 light travel between SPOINT to the observer is
C                 considered to be the one way light time.
C
C                 When aberration corrections are used, SPOINT is
C                 computed using target body position and orientation
C                 that have been adjusted for the corrections
C                 applicable to SPOINT itself rather than to the target
C                 body's center. In particular, if the stellar
C                 aberration correction applicable to SPOINT is
C                 represented by a shift vector S, then the light-time
C                 corrected position of the target is shifted by S
C                 before the sub-observer point is computed.
C                 
C                 The components of SPOINT have units of km.
C
C
C     TRGEPC      is the "sub-observer point epoch." TRGEPC is defined
C                 as follows: letting LT be the one-way light time
C                 between the observer and the sub-observer point,
C                 TRGEPC is the epoch ET-LT, ET+LT, or ET depending on
C                 whether the requested aberration correction is,
C                 respectively, for received radiation, transmitted
C                 radiation, or omitted. LT is computed using the
C                 method indicated by ABCORR.
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
C$ Parameters
C
C     None.
C
C$ Exceptions
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
C         calling SUBPNT, the error will be diagnosed and signaled by a
C         routine in the call tree of this routine. Note that when
C         light time correction is used, sufficient ephemeris data must
C         be available to propagate the states of both observer and
C         target to the solar system barycenter.
C
C     9)  If the computation method specifies an ellipsoidal target
C         shape and triaxial radii of the target body have not been
C         loaded into the kernel pool prior to calling SUBPNT, the
C         error will be diagnosed and signaled by a routine in the call
C         tree of this routine.
C
C     10)  The target must be an extended body: if any of the radii of
C          the target body are non-positive, the error will be
C          diagnosed and signaled by routines in the call tree of this
C          routine.
C
C     11)  If PCK data specifying the target body-fixed frame
C          orientation have not been loaded prior to calling SUBPNT,
C          the error will be diagnosed and signaled by a routine in the
C          call tree of this routine.
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
C     There are two different popular ways to define the sub-observer
C     point: "nearest point on the target to the observer" or "target
C     surface intercept of the line containing observer and target."
C     These coincide when the target is spherical and generally are
C     distinct otherwise.
C
C     This routine computes light time corrections using light time
C     between the observer and the sub-observer point, as opposed to
C     the center of the target. Similarly, stellar aberration
C     corrections done by this routine are based on the direction of
C     the vector from the observer to the light-time corrected
C     sub-observer point, not to the target center. This technique
C     avoids errors due to the differential between aberration
C     corrections across the target body. Therefore it's valid to use
C     aberration corrections with this routine even when the observer
C     is very close to the sub-observer point, in particular when the
C     observer to sub-observer point distance is much less than the
C     observer to target center distance.
C
C     The definition of the aberration-corrected sub-observer point is
C     implicit: SPOINT is defined by an equation of the form
C
C        SPOINT = F ( SPOINT )
C
C     Because of the contraction properties of both light time and
C     stellar aberration corrections---that is, the difference in the
C     corrections for two vectors is much smaller than the difference
C     between the vectors themselves---it's easy to solve this equation
C     accurately and fairly quickly.
C     
C     When comparing sub-observer point computations with results from
C     sources other than SPICE, it's essential to make sure the same
C     geometric definitions are used.
C
C$ Examples
C
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C 
C     1) Find the sub-Earth point on Mars for a specified time. Perform
C        the computation twice, using both the "intercept" and "near
C        point" options. Display the location of both the Earth and the
C        sub-Earth point using both planetocentric and planetographic
C        coordinates.
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
C              KERNELS_TO_LOAD = ( 'de418.bsp',
C                                  'pck00008.tpc',
C                                  'naif0008.tls'  )
C
C           \begintext
C
C
C       Example code begins here.
C
C
C          PROGRAM EX1
C          IMPLICIT NONE
C    C
C    C     SPICELIB functions
C    C
C          DOUBLE PRECISION      DPR
C          DOUBLE PRECISION      VNORM
C    C
C    C     Local parameters
C    C
C          CHARACTER*(*)         META
C          PARAMETER           ( META   = 'example.tm' )
C
C          CHARACTER*(*)         FM
C          PARAMETER           ( FM     =  '(A,F21.9)' )
C
C          INTEGER               MTHLEN
C          PARAMETER           ( MTHLEN = 50 )
C    C
C    C     Local variables
C    C
C          CHARACTER*(MTHLEN)    METHOD ( 2 )
C
C          DOUBLE PRECISION      ET
C          DOUBLE PRECISION      F
C          DOUBLE PRECISION      OBSPOS ( 3 )
C          DOUBLE PRECISION      ODIST
C          DOUBLE PRECISION      OPCLAT
C          DOUBLE PRECISION      OPCLON
C          DOUBLE PRECISION      OPCRAD
C          DOUBLE PRECISION      OPGALT
C          DOUBLE PRECISION      OPGLAT
C          DOUBLE PRECISION      OPGLON
C          DOUBLE PRECISION      RADII  ( 3 )
C          DOUBLE PRECISION      RE
C          DOUBLE PRECISION      RP
C          DOUBLE PRECISION      SPCLAT
C          DOUBLE PRECISION      SPCLON
C          DOUBLE PRECISION      SPCRAD
C          DOUBLE PRECISION      SPGALT
C          DOUBLE PRECISION      SPGLAT
C          DOUBLE PRECISION      SPGLON
C          DOUBLE PRECISION      SPOINT ( 3 )
C          DOUBLE PRECISION      SRFVEC ( 3 )
C          DOUBLE PRECISION      TRGEPC
C
C          INTEGER               I
C          INTEGER               N
C    C
C    C     Saved variables
C    C
C          SAVE                  METHOD
C    C
C    C     Initial values
C    C
C          DATA                  METHOD / 'Intercept:  ellipsoid',
C         .                               'Near point: ellipsoid' /
C    C
C    C     Load kernel files via the meta-kernel.
C    C
C          CALL FURNSH ( META )
C
C    C
C    C     Convert the UTC request time string seconds past
C    C     J2000, TDB.
C    C
C          CALL STR2ET ( '2008 AUG 11 00:00:00', ET )
C
C    C
C    C     Look up the target body's radii. We'll use these to
C    C     convert Cartesian to planetographic coordinates. Use
C    C     the radii to compute the flattening coefficient of
C    C     the reference ellipsoid.
C    C
C          CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII )
C
C    C
C    C     Let RE and RP be, respectively, the equatorial and
C    C     polar radii of the target.
C    C
C          RE = RADII( 1 )
C          RP = RADII( 3 )
C
C          F  = ( RE - RP ) / RE
C
C    C
C    C     Compute sub-observer point using light time and stellar
C    C     aberration corrections. Use the "target surface intercept"
C    C     definition of sub-observer point on the first loop
C    C     iteration, and use the "near point" definition on the
C    C     second.
C    C
C          DO I = 1, 2
C
C             CALL SUBPNT ( METHOD(I),
C         .                'MARS',  ET,     'IAU_MARS', 'LT+S',
C         .                'EARTH', SPOINT, TRGEPC,     SRFVEC )
C    C
C    C        Compute the observer's distance from SPOINT.
C    C
C             ODIST  = VNORM ( SRFVEC )
C
C    C
C    C        Convert the sub-observer point's rectangular coordinates
C    C        to planetographic longitude, latitude and altitude.
C    C        Convert radians to degrees.
C    C
C             CALL RECPGR ( 'MARS', SPOINT, RE,    F,
C         .                 SPGLON, SPGLAT, SPGALT   )
C
C             SPGLON = SPGLON * DPR ()
C             SPGLAT = SPGLAT * DPR ()
C
C    C
C    C        Convert sub-observer point's rectangular coordinates to
C    C        planetocentric radius, longitude, and latitude. Convert
C    C        radians to degrees.
C    C
C             CALL RECLAT ( SPOINT, SPCRAD, SPCLON, SPCLAT )
C
C             SPCLON = SPCLON * DPR ()
C             SPCLAT = SPCLAT * DPR ()
C
C    C
C    C        Compute the observer's position relative to the center
C    C        of the target, where the center's location has been
C    C        adjusted using the aberration corrections applicable
C    C        to the sub-point. Express the observer's location in
C    C        planetographic coordinates.
C    C
C             CALL VSUB ( SPOINT, SRFVEC, OBSPOS )
C
C             CALL RECPGR ( 'MARS', OBSPOS, RE,    F,
C         .                 OPGLON, OPGLAT, OPGALT   )
C
C             OPGLON = OPGLON * DPR ()
C             OPGLAT = OPGLAT * DPR ()
C
C    C
C    C        Convert the observer's rectangular coordinates to
C    C        planetocentric radius, longitude, and latitude.
C    C        Convert radians to degrees.
C    C
C             CALL RECLAT ( OBSPOS, OPCRAD, OPCLON, OPCLAT )
C
C             OPCLON = OPCLON * DPR ()
C             OPCLAT = OPCLAT * DPR ()
C
C    C
C    C        Write the results.
C    C
C             WRITE(*,FM) ' '
C             WRITE(*,* ) 'Computation method = ', METHOD(I)
C             WRITE(*,FM) ' '
C             WRITE(*,FM)
C         .   '  Observer altitude                      (km) = ', OPGALT
C             WRITE(*,FM)
C         .   '  Length of SRFVEC                       (km) = ', ODIST
C             WRITE(*,FM)
C         .   '  Sub-observer point altitude            (km) = ', SPGALT
C             WRITE(*,FM)
C         .   '  Sub-observer planetographic longitude (deg) = ', SPGLON
C             WRITE(*,FM)
C         .   '  Observer planetographic longitude     (deg) = ', OPGLON
C             WRITE(*,FM)
C         .   '  Sub-observer planetographic latitude  (deg) = ', SPGLAT
C             WRITE(*,FM)
C         .   '  Observer planetographic latitude      (deg) = ', OPGLAT
C             WRITE(*,FM)
C         .   '  Sub-observer planetocentric longitude (deg) = ', SPCLON
C             WRITE(*,FM)
C         .   '  Observer planetocentric longitude     (deg) = ', OPCLON
C             WRITE(*,FM)
C         .   '  Sub-observer planetocentric latitude  (deg) = ', SPCLAT
C             WRITE(*,FM)
C         .   '  Observer planetocentric latitude      (deg) = ', OPCLAT
C             WRITE(*,FM) ' '
C
C          END DO
C
C          END
C
C
C     When this program was executed on a PC/Linux/g77 platform, the
C     output was:
C
C
C   Computation method = Intercept:  ellipsoid
C
C    Observer altitude                      (km) =   349199089.542324781
C    Length of SRFVEC                       (km) =   349199089.579020321
C    Sub-observer point altitude            (km) =           0.000000000
C    Sub-observer planetographic longitude (deg) =         199.302305055
C    Observer planetographic longitude     (deg) =         199.302305055
C    Sub-observer planetographic latitude  (deg) =          26.262401212
C    Observer planetographic latitude      (deg) =          25.994936725
C    Sub-observer planetocentric longitude (deg) =         160.697694945
C    Observer planetocentric longitude     (deg) =         160.697694945
C    Sub-observer planetocentric latitude  (deg) =          25.994934146
C    Observer planetocentric latitude      (deg) =          25.994934146
C
C
C   Computation method = Near point: ellipsoid
C
C    Observer altitude                      (km) =   349199089.542316496
C    Length of SRFVEC                       (km) =   349199089.542316496
C    Sub-observer point altitude            (km) =           0.000000000
C    Sub-observer planetographic longitude (deg) =         199.302305055
C    Observer planetographic longitude     (deg) =         199.302305055
C    Sub-observer planetographic latitude  (deg) =          25.994936725
C    Observer planetographic latitude      (deg) =          25.994936725
C    Sub-observer planetocentric longitude (deg) =         160.697694945
C    Observer planetocentric longitude     (deg) =         160.697694945
C    Sub-observer planetocentric latitude  (deg) =          25.729407202
C    Observer planetocentric latitude      (deg) =          25.994934146
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
C-    SPICELIB Version 1.3.0, 31-MAR-2014 (BVS)
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
C-    SPICELIB Version 1.2.0, 02-APR-2012 (NJB) (SCK)
C
C        Bug fix: FIRST is now set to .FALSE. at the completion
C        of a successful initialization pass. This does not affect
C        the routine's outputs but improves efficiency.
C
C        References to the new PXFRM2 routine were added, which changed
C        the Detailed Output section and the second example.  
C
C        Upgrade: this routine now uses ZZVALCOR rather than
C        ZZPRSCOR, simplifying the implementation.
C
C-    SPICELIB Version 1.1.0, 18-MAY-2010 (NJB) 
C
C        Bug fix: calls to FAILED() have been added after
C        SPK calls, target radius lookup, near point
C        and surface intercept computations.
C
C-    SPICELIB Version 1.0.1, 06-FEB-2009 (NJB) 
C
C        Typo correction: changed FIXFRM to FIXREF in header
C        documentation. Meta-kernel name suffix was changed to
C        ".tm" in header code example.
C
C-    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) 
C
C-&
 
C$ Index_Entries
C
C     find sub-observer point on target body
C     find sub-spacecraft point on target body
C     find nearest point to observer on target body
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
      DOUBLE PRECISION      VNORM
      
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  =  'SUBPNT' )

C
C     This value will become system-dependent when systems
C     using 128-bit d.p. numbers are supported by SPICELIB.
C     CNVLIM, when added to 1.0D0, should yield 1.0D0. 
C
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-17 )
     

      INTEGER               MAXITR
      PARAMETER           ( MAXITR =  5 )

      INTEGER               MAXW
      PARAMETER           ( MAXW   =  2 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               WORDLN
      PARAMETER           ( WORDLN = 32 )

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

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      CORPOS ( 3 )
      DOUBLE PRECISION      CORVJ2 ( 3 )
      DOUBLE PRECISION      ETDIFF
      DOUBLE PRECISION      J2POS  ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTDIFF
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      PREVET
      DOUBLE PRECISION      PREVLT
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SSBOST ( 6 )
      DOUBLE PRECISION      SSBTST ( 6 )
      DOUBLE PRECISION      SUBVEC ( 3 )
      DOUBLE PRECISION      SUBVJ2 ( 3 )
      DOUBLE PRECISION      STLOFF ( 3 )
      DOUBLE PRECISION      TPOS   ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
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
      SAVE                  SVREFC



C
C     Initial values
C
      DATA                  ELIPSD  / .TRUE. /
      DATA                  FIRST   / .TRUE. /
      DATA                  NEAR    / .TRUE. /
      DATA                  PRVCOR  / ' '    /
      DATA                  PRVMTH  / 'Ellipsoid, near point' /
 

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
C        At this point, the first pass actions were successful.
C
         FIRST   = .FALSE.

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
C     Check the input body codes.  If they are equal, signal
C     an error.
C
      IF ( OBSCDE .EQ. TRGCDE ) THEN
 
         CALL SETMSG ( 'In computing the sub-observer point, '   
     .   //            'the observing body and target body are the '
     .   //            'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( RNAME                                        )
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
     .   //            'the target body #. The ID code of the '
     .   //            'frame center is #.'                       )
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
     .      //            'header of SUBPNT for details.'            )
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
     .      //            'header of SUBPNT for details.'            )
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
     .      //            'header of SUBPNT for details.'            )
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

         IF ( XMIT ) THEN
            S   =  1.D0
         ELSE
            S   = -1.D0
         END IF
         
      ELSE
         S = 0.D0
      END IF
 
C
C     Determine the position of the observer in the target body-fixed
C     frame. This is a first estimate.
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
C            that corrected vector in order to compute the sub-observer
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
C     Find the sub-observer point and distance from observer to 
C     sub-observer point using the specified geometric definition.
C
      IF ( ELIPSD ) THEN 
C
C        Find the sub-observer point given the target epoch,
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


         RANGE  = VNORM(OBSPOS)

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

C
C        Make a first estimate of the sub-observer point. The algorithm
C        we use depends on the sub-observer point definition.
C
         IF ( NEAR ) THEN
C
C           Locate the nearest point to the observer on the target.
C
            CALL NEARPT ( OBSPOS, RADII(1), RADII(2), RADII(3),
     .                    SPOINT, ALT                           )

         ELSE
C
C           Locate the surface intercept of the ray from the 
C           observer to the target center.
C
            CALL SURFPT ( OBSPOS, TPOS, RADII(1), RADII(2), RADII(3), 
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

            ALT = VDIST ( OBSPOS, SPOINT )

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF


C
C        Compute the one-way light time and target epoch based on our
C        first computation of SPOINT. The coefficient S has been
C        set to give us the correct answer for each aberration
C        correction case.
C
         LT     = ALT / CLIGHT()
         TRGEPC = ET  + S*LT

C
C        If we're not using light time and stellar aberration
C        corrections, we're almost done now. Note that we need only
C        check for use of light time corrections, because use of
C        stellar aberration corrections alone has been prevented by an
C        earlier check.
        
         IF ( .NOT. USELT ) THEN
C
C           The TRGEPC value we'll return comes from our value of ALT
C           computed above. The previous call to SPKEZP call yielded
C           the vector OBSPOS. SPOINT was set immediately above. The
C           only output left to compute is SRFVEC.
C
            CALL VSUB ( SPOINT, OBSPOS, SRFVEC )

            CALL CHKOUT ( RNAME )
            RETURN

         END IF

C
C        We'll now make an improved sub-observer point estimate using
C        the previous estimate of the sub-observer point. The number of
C        iterations depends on the light time correction type.

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
C           If we're using stellar aberration corrections, adjust the
C           observer position to account for the stellar aberration
C           correction applicable to SPOINT.
C
            IF ( USESTL ) THEN
C
C              We want to apply the stellar aberration correction that
C              applies to our current estimate of the sub-observer point
C              location, NOT the correction for the target body's
C              center. In most cases the two corrections will be
C              similar, but they might not be---consider the case of a
C              highly prolate target body where the observer is close
C              to one "end" of the body.
C 
C              Find the vector from the observer to the estimated
C              sub-observer point. Find the stellar aberration offset
C              STLOFF for this vector. Note that all vectors are
C              expressed relative to the target body-fixed frame at
C              TRGEPC. We must perform our corrections in an inertial
C              frame.
C
               CALL VSUB ( SPOINT, OBSPOS, SUBVEC )

               CALL MTXV ( XFORM,  SUBVEC, SUBVJ2 )

               IF ( XMIT ) THEN
                  CALL STLABX ( SUBVJ2, SSBOST(4), CORVJ2 )
               ELSE
                  CALL STELAB ( SUBVJ2, SSBOST(4), CORVJ2 )
               END IF

               CALL MXV  ( XFORM,  CORVJ2, CORPOS )
               CALL VSUB ( CORPOS, SUBVEC, STLOFF )

C
C              In principle, we want to shift the target body position
C              relative to the solar system barycenter by STLOFF, but
C              we can skip this step and just re-compute the observer's
C              location relative to the target body's center by
C              subtracting off STLOFF.
C             
               CALL VSUB ( OBSPOS, STLOFF, VTEMP  )
               CALL VEQU ( VTEMP,          OBSPOS )

            END IF

C
C           Find the sub-observer point using the current estimated
C           geometry.
C
            IF ( NEAR ) THEN
C
C              Locate the nearest point to the observer on the target.
C
               CALL NEARPT ( OBSPOS, RADII(1), RADII(2), RADII(3),
     .                       SPOINT, ALT                           )

            ELSE
C
C              Locate the surface intercept of the ray from the 
C              observer to the target center.
C
               CALL VMINUS ( OBSPOS,   TPOS )
               CALL SURFPT ( OBSPOS,   TPOS,   RADII(1), RADII(2), 
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

               ALT = VDIST ( OBSPOS, SPOINT )

            END IF

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF


C
C           Compute a new light time estimate and new target epoch.
C
            LT      =  ALT / CLIGHT()
            TRGEPC  =  ET  + S*LT

C
C           At this point, we have new estimates of the sub-observer
C           point SPOINT, the observer altitude ALT, the target epoch
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
C     SPOINT, TRGEPC, and OBSPOS have been set at this point. Compute
C     SRFVEC.
C     
      CALL VSUB ( SPOINT, OBSPOS, SRFVEC )

      CALL CHKOUT ( RNAME )
      RETURN
      END
