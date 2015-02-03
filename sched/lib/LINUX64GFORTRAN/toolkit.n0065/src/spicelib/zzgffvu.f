C$Procedure ZZGFFVU ( GF, instrument FOV utilities )

      SUBROUTINE ZZGFFVU ( INST,   TSHAPE, RAYDIR, TARGET,
     .                     TFRAME, ABCORR, OBSRVR, TIME,   VISTAT )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine contains the entry points that produce the
C     computations needed for solving for target visibility states
C     in the geometry finding routines.
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
C     CK
C     GF
C     IK
C     NAIF_IDS
C     PCK
C     SCLK
C     SPK
C     TIME
C
C$ Keywords
C
C     EVENT
C     FOV
C     GEOMETRY
C     INSTRUMENT
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzocced.inc'
      INCLUDE 'gf.inc'

      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )

      CHARACTER*(*)         INST
      CHARACTER*(*)         TSHAPE
      DOUBLE PRECISION      RAYDIR ( 3 )
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         TFRAME
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      TIME
      LOGICAL               VISTAT

C$ Brief_I/O
C
C     VARIABLE  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     UBEL       P   All
C     UBPL       P   All
C     INST       I   ZZGFFVIN
C     TSHAPE     I   ZZGFFVIN
C     RAYDIR     I   ZZGFFVIN
C     TARGET     I   ZZGFFVIN
C     TFRAME     I   ZZGFFVIN
C     ABCORR     I   ZZGFFVIN
C     OBSRVR     I   ZZGFFVIN
C     TIME       I   ZZGFFVST
C     OCSTAT     O   ZZGFFVST
C
C$ Detailed_Input
C
C     See entry points.
C
C$ Detailed_Output
C
C     See entry points.
C
C$ Parameters
C
C     See INCLUDE files
C
C        gf.inc
C        zzgf.inc
C
C$ Exceptions
C
C     See entry points.
C
C$ Files
C
C     Appropriate SPK and instrument kernels must be loaded by the
C     calling program before this routine is called.  PCK, CK and SCLK
C     kernels may be required as well.
C
C     The following data are required:
C
C        - SPK data:  ephemeris data for target and observer that
C          describes the ephemeris of these objects for the period
C          defined by the confinement window, 'CNFINE' must be
C          loaded.  If aberration corrections are used, the states of
C          target and observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C          Typically ephemeris data are made available by loading one
C          or more SPK files via FURNSH.
C
C        - Frame data:  if a frame definition is required to convert
C          the observer and target states to the body-fixed frame of
C          the target, that definition must be available in the kernel
C          pool. Typically the definitions of frames not already
C          built-in to SPICE are supplied by loading a frame kernel.
C
C          Data defining the reference frame associated with the
C          instrument designated by INST must be available in the kernel
C          pool. Additionally the name INST must be associated with an
C          ID code. Normally these data are  made available by loading
C          a frame kernel via FURNSH.
C
C        - IK data: the kernel pool must contain data such that
C          the SPICELIB routine GETFOV may be called to obtain
C          parameters for INST. Normally such data are provided by
C          an IK via FURNSH.
C
C     The following data may be required:
C
C        - PCK data: bodies modeled as triaxial ellipsoids must have
C          orientation data provided by variables in the kernel pool.
C          Typically these data are made available by loading a text
C          PCK file via FURNSH.
C
C          Bodies modeled as triaxial ellipsoids must have semi-axis
C          lengths provided by variables in the kernel pool. Typically
C          these data are made available by loading a text PCK file via
C          FURNSH.
C
C        - CK data: if the instrument frame is fixed to a spacecraft,
C          at least one CK file will be needed to permit transformation
C          of vectors between that frame and both J2000 and the target
C          body-fixed frame.
C
C        - SCLK data:  if a CK file is needed, an associated SCLK
C          kernel is required to enable conversion between encoded SCLK
C          (used to time-tag CK data) and barycentric dynamical time
C          (TDB).
C
C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C     This routine is designed to determine whether a specified
C     target intersects the space bounded by the FOV of a specified
C     instrument at a specified epoch. The target may be represented
C     by a ray, or the target may be an ephemeris object.
C
C     This routine contains two entry points that support searches
C     for target visibility periods performed using ZZGFSOLV:
C
C        ZZGFFVIN   Saves the user-supplied inputs defining the
C                   visibility computation to be performed.
C                   Initializes the visibility search.
C
C        ZZGFFVST   Returns the visibility state for a specified
C                   time.
C
C$ Examples
C
C     See GFFOVE.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 04-JUN-2013 (EDW)
C
C        Edit to ZZGFFVIN: 
C
C        ABCORR now stripped of all spaces before saving.
C        Specifically, the call
C
C        CALL LJUST ( ABCORR, SVCORR )
C
C        replaced with
C
C        CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
C
C-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW)
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      INTEGER               ZZOCCED
      INTEGER               ZZWIND2D

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO


C
C     Local parameters
C

C
C     ATOL is a tolerance value for computing FOV angular radius.
C     The angular radius must not exceed pi/2 - ATOL radians.
C
      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL  = 1.D-6 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local variables
C
      CHARACTER*(FRNMLN)    SVIFRM
      CHARACTER*(BDNMLN)    SVINAM
      CHARACTER*(SHPLEN)    SVISHP
      CHARACTER*(FRNMLN)    SVTFRM
      CHARACTER*(BDNMLN)    SVTNAM
      CHARACTER*(SHPLEN)    SVTSHP
      CHARACTER*(CORLEN)    SVCORR

      DOUBLE PRECISION      BSITE  ( 3 )
      DOUBLE PRECISION      COORD  ( 2 )
      DOUBLE PRECISION      CTREXT
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      ETTARG
      DOUBLE PRECISION      FOVPT  ( 3 )
      DOUBLE PRECISION      FOVRAD ( 3 )
      DOUBLE PRECISION      FVLIMB ( UBEL )
      DOUBLE PRECISION      INSMAT ( 3, 3 )
      DOUBLE PRECISION      L
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      M1     ( 3, 3 )
      DOUBLE PRECISION      M2     ( 3, 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      PNT2D  ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      SEMIPT ( 3, 2 )
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      SVARAD
      DOUBLE PRECISION      SVBNDS ( 3, MAXVRT )
      DOUBLE PRECISION      SVEDCT ( 3 )
      DOUBLE PRECISION      SVFAXI ( 3 )
      DOUBLE PRECISION      SVFOVM ( 3, 3 )
      DOUBLE PRECISION      SVFPOL ( 2, MAXVRT )
      DOUBLE PRECISION      SVFSMX ( 3, 3 )
      DOUBLE PRECISION      SVFVCT ( 3 )
      DOUBLE PRECISION      SVORIG ( 3 )
      DOUBLE PRECISION      SVPLAN ( UBPL )
      DOUBLE PRECISION      SVRDIR ( 3 )
      DOUBLE PRECISION      SVSEMI ( 3, 2 )
      DOUBLE PRECISION      SVTRAD ( 3 )
      DOUBLE PRECISION      SVXMAG ( 2 )
      DOUBLE PRECISION      TRGCTR ( 3 )
      DOUBLE PRECISION      TRGSMX ( 3, 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VTEMP2 ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               CLSSID
      INTEGER               FRAMID
      INTEGER               FRCENT
      INTEGER               FRCLSS
      INTEGER               I
      INTEGER               N
      INTEGER               NXPTS
      INTEGER               OCSTAT
      INTEGER               SVINST
      INTEGER               SVNVRT
      INTEGER               SVOBS
      INTEGER               SVTARG
      INTEGER               W

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND
      LOGICAL               SVURAY
      LOGICAL               SVUSTL
      LOGICAL               SVXMIT
      LOGICAL               SVXTRG

C
C     Saved variables
C
      SAVE                  SVARAD
      SAVE                  SVBNDS
      SAVE                  SVCORR
      SAVE                  SVEDCT
      SAVE                  SVFAXI
      SAVE                  SVFOVM
      SAVE                  SVFPOL
      SAVE                  SVFSMX
      SAVE                  SVFVCT
      SAVE                  SVIFRM
      SAVE                  SVINAM
      SAVE                  SVINST
      SAVE                  SVISHP
      SAVE                  SVNVRT
      SAVE                  SVOBS
      SAVE                  SVORIG
      SAVE                  SVPLAN
      SAVE                  SVRDIR
      SAVE                  SVSEMI
      SAVE                  SVTARG
      SAVE                  SVTFRM
      SAVE                  SVTNAM
      SAVE                  SVTRAD
      SAVE                  SVTSHP
      SAVE                  SVURAY
      SAVE                  SVUSTL
      SAVE                  SVXMAG
      SAVE                  SVXMIT
      SAVE                  SVXTRG

C
C     Initial values
C
      DATA                  SVORIG / 3 * 0.D0 /

C
C     Below we initialize the list of visibility types.
C

C
C     This routine should never be called directly.
C
      CALL CHKIN  ( 'ZZGFFVU' )

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'ZZGFFVU' )
      RETURN




C$Procedure  ZZGFFVIN ( GF, visibility initialization )

      ENTRY ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                 TARGET, TFRAME, ABCORR, OBSRVR )

C$ Abstract
C
C    Perform initialization functions for visibility state
C    determination.
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
C     FRAMES
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     SEARCH
C     GEOMETRY
C     VISIBILITY
C
C$ Declarations
C
C     CHARACTER*(*)         INST
C     CHARACTER*(*)         TSHAPE
C     DOUBLE PRECISION      RAYDIR ( 3 )
C     CHARACTER*(*)         TARGET
C     CHARACTER*(*)         TFRAME
C     CHARACTER*(*)         ABCORR
C     CHARACTER*(*)         OBSRVR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INST       I   Name of the instrument.
C     TSHAPE     I   Type of shape model used for target body.
C     RAYDIR     I   Ray's direction vector.
C     TARGET     I   Name of the target body.
C     TFRAME     I   Body-fixed, body-centered frame for target body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C
C$ Detailed_Input
C
C
C     INST       indicates the name of the instrument, such as a
C                spacecraft-mounted framing camera, the field of view
C                (FOV) of which is to be used for a target intersection
C                search: times when the specified target intersects the
C                region of space corresponding to the FOV are sought.
C
C                The position of the instrument designated by INST is
C                considered to coincide with that of the ephemeris
C                object designated by the input argument OBSRVR (see
C                description below).
C
C                INST must have a corresponding NAIF ID and a frame
C                defined, as is normally done in a frame kernel. It
C                must also have an associated reference frame and a FOV
C                shape, boresight and boundary vertices (or reference
C                vector and reference angles) defined, as is usually
C                done in an instrument kernel.
C
C                See the header of the SPICELIB routine GETFOV for a
C                description of the required parameters associated with
C                an instrument.
C
C
C     TSHAPE     is a string indicating the geometric model used to
C                represent the location and shape of the target body.
C                The target body may be represented by either an
C                ephemeris object or a ray emanating from the observer.
C
C                The supported values of TSHAPE are:
C
C                   'ELLIPSOID'     The target is an ephemeris object.
C
C                                   The target's shape is represented
C                                   using triaxial ellipsoid model,
C                                   with radius values provided via the
C                                   kernel pool. A kernel variable
C                                   having a name of the form
C
C                                      'BODYnnn_RADII'
C
C                                   where nnn represents the NAIF
C                                   integer code associated with the
C                                   body, must be present in the kernel
C                                   pool. This variable must be
C                                   associated with three numeric
C                                   values giving the lengths of the
C                                   ellipsoid's X, Y, and Z semi-axes.
C
C                   'POINT'         The target is an ephemeris object.
C                                   The body is treated as a single
C                                   point.
C
C                   'RAY'           The target is NOT an ephemeris
C                                   object. Instead, the target is
C                                   considered to be represented by the
C                                   ray emanating from the observer's
C                                   location and having direction
C                                   vector RAYDIR. The target is
C                                   considered to be visible if and
C                                   only if the ray is contained within
C                                   the space bounded by the instrument
C                                   FOV.
C
C                Case and leading or trailing blanks are not
C                significant in the string TSHAPE.
C
C
C     RAYDIR     is the direction vector associated with a ray
C                representing the target. RAYDIR is used if and only
C                if TSHAPE (see description above) indicates the
C                target is modeled as a ray.
C
C
C     TARGET     is the name of the target body, the appearances of
C                which in the specified instrument's field of view are
C                sought. The body must be an ephemeris object.
C
C                Optionally, you may supply the integer NAIF ID code
C                for the body as a string. For example both 'MOON' and
C                '301' are legitimate strings that designate the Moon.
C
C                Case and leading or trailing blanks are not
C                significant in the string TARGET.
C
C                The input argument TARGET is used if and only if the
C                target is NOT modeled as ray; equivalently, the input
C                argument TSHAPE (see description above) does not
C                contain a string equivalent---that is, ignoring case
C                and leading and trailing blanks---to 'RAY'.
C
C                TARGET may be set to a blank string if the target is
C                modeled as a ray.
C
C
C     TFRAME     is the name of the reference frame associated with the
C                target. Examples of such names are 'IAU_SATURN'
C                (for Saturn) and 'ITRF93' (for the Earth).
C
C                If the target is an ephemeris object modeled as an
C                ellipsoid, TFRAME must designate a body-fixed
C                reference frame centered on the target body.
C
C                If the target is an ephemeris object modeled as a
C                point, TFRAME is ignored; TFRAME should be left blank.
C
C                If the target is modeled as a ray, TFRAME may
C                designate any reference frame. Since light time
C                corrections are not supported for rays, the
C                orientation of the frame is always evaluated at the
C                epoch associated with the observer, as opposed to the
C                epoch associated with the light-time corrected
C                position of the frame center.
C
C                Case and leading or trailing blanks bracketing a
C                non-blank frame name are not significant in the string
C                TFRAME.
C
C
C     ABCORR     indicates the aberration corrections to be applied
C                when computing the target's position and orientation.
C                The supported values of ABCORR depend on the target
C                representation.
C
C                If the target is represented by a ray, the aberration
C                correction options are
C
C                   'NONE'          No correction.
C                   'S'             Stellar aberration correction,
C                                   reception case.
C                   'XS'            Stellar aberration correction,
C                                   transmission case.
C
C                If the target is an ephemeris object, the aberration
C                correction options are those supported by the SPICE
C                SPK system. For remote sensing applications, where the
C                apparent position and orientation of the target seen
C                by the observer are desired, normally either of the
C                corrections
C
C                   'LT+S'
C                   'CN+S'
C
C                should be used. These and the other supported options
C                are described below.
C
C                Supported aberration correction options for
C                observation (case where radiation is received by
C                observer at ET) are:
C
C                   'NONE'       No correction.
C                   'LT'         Light time only
C                   'LT+S'       Light time and stellar aberration.
C                   'CN'         Converged Newtonian (CN) light time.
C                   'CN+S'       CN light time and stellar aberration.
C
C                Supported aberration correction options for
C                transmission (case where radiation is emitted from
C                observer at ET) are:
C
C                   'XLT'        Light time only.
C                   'XLT+S'      Light time and stellar aberration.
C                   'XCN'        Converged Newtonian (CN) light time.
C                   'XCN+S'      CN light time and stellar aberration.
C
C                For detailed information, see the geometry finder
C                required reading, gf.req.
C
C                Case, leading and trailing blanks are not significant
C                in the string ABCORR.
C
C
C     OBSRVR     is the name of the body from which the target is
C                observed. The instrument designated by INST is treated
C                as if it were co-located with the observer.
C
C                Optionally, you may supply the integer NAIF ID code
C                for the body as a string.
C
C                Case and leading or trailing blanks are not
C                significant in the string OBSRVR.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the observer's name cannot be mapped to an ID code, the
C         error SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If the target is an ephemeris object and its name cannot be
C         mapped to an ID code, the error SPICE(IDCODENOTFOUND) is
C         signaled. If the target is represented by a ray, the input
C         target name argument is ignored.
C
C     3)  If target is an ephemeris object, and the observer and
C         target have the same ID codes, the error
C         SPICE(BODIESNOTDISTINCT) is signaled.
C
C     4)  If target is an ephemeris object, and the target shape
C         is not equivalent to PTSHAP (point) or EDSHAP (ellipsoid),
C         the error SPICE(INVALIDSHAPE) is signaled.
C
C     5)  If target is an ephemeris object, the target shape is
C         equivalent to EDSHAP (ellipsoid), and the reference frame
C         argument TFRAME is blank, the error SPICE(INVALIDFRAME) is
C         signaled.
C
C     6)  If target is an ephemeris object, the target shape is
C         equivalent to EDSHAP (ellipsoid), and the reference frame
C         argument TFRAME cannot be mapped to a frame ID code, the
C         error SPICE(INVALIDFRAME) is signaled.
C
C     7)  If target is an ephemeris object, the target shape is
C         equivalent to EDSHAP (ellipsoid), and the reference frame
C         argument TFRAME's ID cannot be mapped to a frame description,
C         the error SPICE(FRAMEINFONOTFOUND) is signaled.
C
C     8)  If target is an ephemeris object, the target shape is
C         equivalent to EDSHAP (ellipsoid), and the reference frame
C         specified by TFRAME is not centered on the target body, the
C         error SPICE(INVALIDFRAME) is signaled.
C
C     9)  If the target is represented by a ray and the aberration
C         correction flag calls for light time correction, the error
C         SPICE(INVALIDOPTION) is signaled.
C
C     10) If target is an ephemeris object and the aberration
C         correction flag calls for a correction not supported by
C         the SPICE SPK system, the error is diagnosed by a routine
C         in the call tree of this routine.
C
C     11) If target is an ephemeris object, the target shape is
C         equivalent to EDSHAP (ellipsoid), and the kernel pool
C         does not contain radii for the target body,
C         not  target body, the error is diagnosed by a routine
C         in the call tree of this routine.
C
C     12) If target is an ephemeris object, the target shape is
C         equivalent to EDSHAP (ellipsoid), and the kernel pool
C         contains the wrong number of radii for the target body, the
C         error SPICE(INVALIDDIMENSION) is signaled.
C
C     13) If target is an ephemeris object, the target shape is
C         equivalent to EDSHAP (ellipsoid), and the kernel pool
C         contains one or more non-positive radii for the target body,
C         the error SPICE(BADAXISLENGTH) is signaled.
C
C     14) If the target is represented by a ray and the ray's
C         direction vector is zero, the error SPICE(ZEROVECTOR) is
C         signaled.
C
C     15) If the instrument name INST cannot be mapped to an ID code,
C         the error SPICE(IDCODENOTFOUND) is signaled.
C
C     16) If an error occurs while fetching the instrument parameters
C         from the kernel pool, the error will be diagnosed by a
C         routine in the call tree of this routine.
C
C     17) If any ray defined by the observer's position and one of
C         the instrument FOV's boundary vectors fails to intersect
C         the "FOV plane"---a plane normal to the instrument FOV axis
C         and intersected by the FOV axis at distance 1 km from the
C         observer---the error SPICE(DEGENERATECASE) is signaled.
C
C     18) If the FOV is circular or elliptical and the FOV's radius
C         or one of the FOV's semi-axis lengths is zero, the error
C         SPICE(DEGENERATECASE) is signaled.
C
C     19) If the maximum angular separation of the instrument
C         FOV axis and any FOV boundary vector exceeds the limit
C         (which is slightly less than 90 degrees), either the error
C         SPICE(FOVTOOWIDE) will be signaled or the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C
C$ Files
C
C     See the header of the umbrella routine ZZGFFVU.
C
C$ Particulars
C
C     This entry point initializes the parameters needed by the
C     occultation state determination entry point ZZGFFVST.
C
C$ Examples
C
C     See implementation of GFFOVE.
C
C$ Restrictions
C
C     1) The reference frame associated with INST must be
C        centered at the observer or must be inertial. No check is done
C        to ensure this.
C
C     2) This is a SPICELIB private routine; it should not be called by
C        user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 04-JUN-2013 (EDW)
C
C        ABCORR now stripped of all spaces before saving.
C        Specifically, the call
C
C        CALL LJUST ( ABCORR, SVCORR )
C
C        replaced with
C
C        CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
C
C-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW)
C
C-&


C$ Revisions
C
C     None.
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFFVIN' )

C
C     To avoid portability problems, initialize all
C     saved variables that aren't initialized via DATA
C     statements and aren't guaranteed to be initialized
C     for all cases.
C
      CALL CLEARD ( 3*MAXVRT, SVBNDS )
      CALL CLEARD ( 3,        SVEDCT )
      CALL CLEARD ( 3,        SVFAXI )
      CALL CLEARD ( 2*MAXVRT, SVFPOL )
      CALL CLEARD ( 9,        SVFSMX )
      CALL CLEARD ( UBPL,     SVPLAN )
      CALL CLEARD ( 3,        SVRDIR )
      SVTARG = 0
      SVTFRM = ' '
      SVTNAM = ' '
      CALL CLEARD ( 3,        SVTRAD )
      SVUSTL = .FALSE.
      SVXMIT = .FALSE.

C
C     Find the NAIF ID for OBSRVR.
C
      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFFVIN'                                 )
         RETURN

      END IF

C
C     Process the target shape specifier here.
C
C     Save a left-justified, upper case version of the target shape
C     specifier.
C
      CALL LJUST ( TSHAPE, SVTSHP )
      CALL UCASE ( SVTSHP, SVTSHP )

C
C     Note for maintenance programmer: these checks will
C     require modification to handle DSK-based shapes.
C
      IF (      ( SVTSHP .NE. PTSHAP )
     .    .AND. ( SVTSHP .NE. EDSHAP )
     .    .AND. ( SVTSHP .NE. RYSHAP )  ) THEN

         CALL SETMSG ( 'The target shape specification, '
     .   //            '''#'', is not recognized.'       )
            CALL ERRCH  ( '#', TSHAPE                    )
         CALL SIGERR ( 'SPICE(INVALIDSHAPE)'             )
         CALL CHKOUT ( 'ZZGFFVIN'                        )
         RETURN

      END IF

C
C     We'll use the logical variable USERAY to indicate that the
C     target is modeled as ray.
C
      SVURAY = SVTSHP .EQ. RYSHAP

C
C     Indicate whether we have an extended target. SVXTRG is .TRUE.
C     if and only we have one.
C
      SVXTRG = SVTSHP .EQ. EDSHAP

C
C     If the target is an ephemeris object, obtain its ID code.
C     Save the target object's name, if applicable.
C
      IF ( .NOT. SVURAY ) THEN

         CALL BODS2C ( TARGET, SVTARG, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'The target object, '
     .      //            '''#'', is not a recognized name for an '
     .      //            'ephemeris object. The cause of this '
     .      //            'problem may be that you need an updated '
     .      //            'version of the SPICE Toolkit. '           )
            CALL ERRCH  ( '#', TARGET                                )
            CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
            CALL CHKOUT ( 'ZZGFFVIN'                                 )
            RETURN

         END IF

C
C        Save the target's name.
C
         SVTNAM = TARGET

C
C        Make sure the observer and target are distinct.
C
         IF ( SVTARG .EQ. SVOBS ) THEN

            CALL SETMSG ( 'The observer and target must be '
     .      //            'distinct objects, but are not: '
     .      //            'OBSRVR = #; TARGET = #;'         )
            CALL ERRCH  ( '#', OBSRVR                       )
            CALL ERRCH  ( '#', TARGET                       )
            CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'        )
            CALL CHKOUT ( 'ZZGFFVIN'                        )
            RETURN

         END IF

      END IF

C
C     Process the target frame. The target frame is defined except
C     when the target is an ephemeris object modeled as a point.
C
      IF ( SVURAY .OR. SVXTRG ) THEN
C
C        We'll use the target frame argument. Look up the target
C        frame's ID code. But first, check for a blank frame name,
C        since this may be a common problem for the GF FOV system.
C
         IF ( TFRAME .EQ. ' ' ) THEN

            CALL SETMSG ( 'The target is not modeled as a '
     .      //            'point, but the associated '
     .      //            'frame name is blank.'           )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZGFFVIN'                       )
            RETURN

         END IF

         CALL NAMFRM ( TFRAME, FRAMID )

         IF ( FRAMID .EQ. 0 ) THEN

            CALL SETMSG ( 'The target frame name # is not recognized.' )
            CALL ERRCH  ( '#',  TFRAME                                 )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'                        )
            CALL CHKOUT ( 'ZZGFFVIN'                                   )
            RETURN

         END IF

C
C        Save the target frame name.
C
         CALL LJUST ( TFRAME, SVTFRM )
         CALL UCASE ( SVTFRM, SVTFRM )

C
C        Obtain the center of the frame. If the target is an ephemeris
C        object, we must verify the frame center is the target.
C
         CALL FRINFO ( FRAMID, FRCENT, FRCLSS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN
C
C           Since we mapped the frame name to an ID code, we expect to
C           find the frame info. Getting here may be a sign of an
C           invalid frame kernel.
C
            CALL SETMSG ( 'Frame ID found for # body-fixed '
     .      //            'frame # but FRINFO couldn''t find frame '
     .      //            'info. This may be due to a frame kernel '
     .      //            'error.'                                  )
            CALL ERRCH  ( '#',  TARGET                              )
            CALL SIGERR ( 'SPICE(FRAMEINFONOTFOUND)'                )
            CALL CHKOUT ( 'ZZGFFVIN'                                )
            RETURN

         END IF

         IF ( SVXTRG ) THEN
C
C           We have an extended target. Check the target frame's center.
C
            IF ( FRCENT .NE. SVTARG ) THEN
C
C              The supposed body-fixed frame for the target isn't
C              actually centered on the target.
C
               CALL SETMSG ( 'Supposed body-fixed frame # for '
     .         //            'target # is actually centered '
     .         //            'on body #.'                       )
               CALL ERRCH  ( '#',  TFRAME                       )
               CALL ERRCH  ( '#',  TARGET                       )
               CALL ERRINT ( '#',  FRCENT                       )
               CALL SIGERR ( 'SPICE(INVALIDFRAME)'              )
               CALL CHKOUT ( 'ZZGFFVIN'                         )
               RETURN

            END IF

         END IF

      END IF

C
C     Process the aberration correction specifier.
C
      IF ( SVURAY ) THEN
C
C        The target is represented by a ray. Check and save the
C        aberration correction.
C
         CALL ZZPRSCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVIN' )
            RETURN
         END IF

C
C        Reject aberration correction flags calling for any type of
C        light time correction. However, stellar aberration corrections
C        are allowed: note this is the reverse of the situation for
C        ephemeris objects. The allowed aberration correction flags are
C
C           'NONE', 'S', 'XS'
C
         IF ( ATTBLK(LTIDX) ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for '
     .      //            'light time corrections; these are not '
     .      //            'supported for targets represented by '
     .      //            'rays.'                                  )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                   )
            CALL CHKOUT ( 'ZZGFFVIN'                               )
            RETURN

         END IF

C
C        Save flags indicating whether to use stellar aberration
C        corrections and indicating the sense of radiation travel.
C
         SVUSTL = ATTBLK(STLIDX)
         SVXMIT = ATTBLK(XMTIDX)


      ELSE
C
C        The target is an ephemeris object.
C
C        Check the aberration correction. If SPKEZR can't handle it,
C        neither can we.
C
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVIN' )
            RETURN
         END IF

      END IF

C
C     Remove all spaces from ABCORR then convert to uppercase. Save
C     this version of the aberration correction specifier.
C
      CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
      CALL UCASE ( SVCORR, SVCORR )

C
C     Process the target body's radii, if applicable.
C
      IF ( SVXTRG ) THEN
C
C        Fetch and check the radii.
C
         CALL BODVCD ( SVTARG, 'RADII', 3, N, SVTRAD )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVIN' )
            RETURN
         END IF

C
C        Check the count of the radii.
C
         IF ( N .NE. 3 )  THEN

            CALL SETMSG ( 'Target # should have 3 radii but '
     .      //            'actually has #. This may be due '
     .      //            'to an error in a PCK file used '
     .      //            'to provide the radii.'             )
            CALL ERRCH  ( '#', TARGET                         )
            CALL ERRINT ( '#', N                              )
            CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'           )
            CALL CHKOUT ( 'ZZGFFVIN'                          )
            RETURN

         END IF

C
C        Check to make sure the current target has 3 positive
C        semi-axis lengths.
C
         IF (     ( SVTRAD(1) .LE. 0.D0 )
     .       .OR. ( SVTRAD(2) .LE. 0.D0 )
     .       .OR. ( SVTRAD(3) .LE. 0.D0 ) ) THEN

            CALL SETMSG ( 'One or more semi-axis lengths of '
     .      //            'the target body # are non-'
     .      //            'positive: 1 = #, 2 = #, 3 = #. '  )
            CALL ERRCH  ( '#', TARGET                        )
            CALL ERRDP  ( '#', SVTRAD (1)                    )
            CALL ERRDP  ( '#', SVTRAD (2)                    )
            CALL ERRDP  ( '#', SVTRAD (3)                    )
            CALL SIGERR ( 'SPICE(BADAXISLENGTH)'             )
            CALL CHKOUT ( 'ZZGFFVIN'                         )
            RETURN

         END IF

C
C        Checks of radii have been completed.
C
      ELSE
C
C        We don't have an extended target body: zero out radius values
C        for this target.
C
         CALL CLEARD ( 3, SVTRAD )

      END IF

C
C     Check the direction vector, if applicable.
C
      IF ( SVURAY ) THEN
C
C        Make sure the direction vector is non-zero. Save a unit-length
C        copy of the vector.
C
         IF ( VZERO( RAYDIR ) ) THEN

            CALL SETMSG ( 'Input ray direction was the zero vector; '
     .      //            'this vector must be non-zero.'            )
            CALL SIGERR ( 'SPICE(ZEROVECTOR)'                        )
            CALL CHKOUT ( 'ZZGFFVIN'                                 )
            RETURN

         END IF

         CALL VHAT ( RAYDIR, SVRDIR )

      END IF

C
C     Look up the instrument's ID code.
C
      CALL BODS2C ( INST, SVINST, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( '''#'' is not a recognized name for an '
     .   //            'instrument. The cause of this '
     .   //            'problem may be that you have not loaded '
     .   //            'a required frame kernel or instrument '
     .   //            'kernel.'                                  )
         CALL ERRCH  ( '#', INST                                  )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFFVIN'                                 )
         RETURN

      END IF

C
C     Save the instrument's name.
C
      CALL LJUST ( INST,   SVINAM )
      CALL UCASE ( SVINAM, SVINAM )

C
C     Look up the instrument parameters.
C
      CALL GETFOV ( SVINST, MAXVRT, SVISHP,
     .              SVIFRM, BSITE,  SVNVRT, SVBNDS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFFVIN' )
         RETURN
      END IF

C
C     Make sure the intrument shape specifier is left-justified
C     and in upper case.
C
      CALL LJUST ( SVISHP, SVISHP )
      CALL UCASE ( SVISHP, SVISHP )

C
C     If the instrument's shape is 'RECTANGLE', map it to
C     'POLYGON'
C
      IF (  SVISHP .EQ. RECFOV ) THEN

         SVISHP = POLFOV

      END IF

C
C     Save an axis vector for the FOV. For circular and ellipsoidal
C     FOVs, the boresight serves as this axis. For polygonal FOVs
C     (rectangular FOVs are included), we'll generate an axis vector.
C
      IF ( SVISHP .EQ. POLFOV ) THEN

         CALL ZZFOVAXI ( INST,  SVNVRT, SVBNDS, SVFAXI )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVIN' )
            RETURN
         END IF

      ELSE

         CALL VEQU ( BSITE, SVFAXI )

      END IF

C
C     Check the angular radius of the FOV.
C
C     Compute the angular radius of the FOV. We'll use this to define a
C     "bounding cone" centered on the FOV axis and having its apex at
C     the observer. This cone will be used for a preliminary FOV
C     exclusion test.
C
      SVARAD = 0.D0

      DO I = 1, SVNVRT

         SVARAD = MAX (  SVARAD,  VSEP( SVBNDS(1,I), SVFAXI )  )

      END DO

C
C     Our algorithms can't handle FOVs with angular radius of 90
C     degrees.
C
      IF (  SVARAD   .GT. ( HALFPI()-ATOL )  ) THEN

         CALL SETMSG ( 'FOV angular radius of # degrees '
     .   //            'exceeds limit of # degrees.'     )
         CALL ERRDP  ( '#',  SVARAD*DPR()                )
         CALL ERRDP  ( '#',  (HALFPI()-ATOL)*DPR()       )
         CALL SIGERR ( 'SPICE(FOVTOOWIDE)'               )
         CALL CHKOUT ( 'ZZGFFVIN'                        )
         RETURN

      END IF

C
C     Convert the FOV shape specifier to a left-justified, upper
C     case form.
C
      CALL LJUST ( SVISHP, SVISHP )
      CALL UCASE ( SVISHP, SVISHP )

C
C     We can make the search more efficient by computing any
C     required, time-invariant quantities here in the initialization
C     routine.
C
C     Compute the FOV plane SVPLAN, which is represented in the
C     instrument frame. The origin will be considered to be located at
C     the observer. The plane is normal to the FOV axis, at distance 1
C     unit from the observer.
C
      CALL NVC2PL ( SVFAXI, 1.D0, SVPLAN )

C
C     Find the point on the plane closest to the origin. This is
C     the center of the FOV.
C
      CALL VHAT ( SVFAXI, SVFVCT )

C
C     If applicable, perform the computations required for an
C     elliptical FOV, where the target representation is arbitrary, or
C     a circular FOV when the target is an extended object.
C
      IF (        ( SVISHP .EQ. ELLFOV )
     .     .OR. ( ( SVISHP .EQ. CIRFOV ) .AND. SVXTRG )  ) THEN
C
C        Also compute the center, semi-axis vectors, and semi-axis
C        lengths of the FOV. If the FOV is circular, we create an
C        artificial, second semi-axis vector.
C
         IF ( SVISHP .EQ. CIRFOV ) THEN
C
C           We have a circular FOV. We'll create an artificial, second
C           boundary vector, which will give rise to a second
C           semi-axis.
C
            CALL VROTV ( SVBNDS(1,1), SVFAXI, HALFPI(), SVBNDS(1,2) )

         END IF

C
C        Now find the endpoints of the semi-axes in this plane.
C
         DO I = 1, 2

            CALL INRYPL ( SVORIG, SVBNDS(1,I),
     .                    SVPLAN, NXPTS,       SEMIPT(1,I) )

            IF ( NXPTS .NE. 1 ) THEN

               CALL SETMSG ( 'Error creating FOV semi-axis '
     .         //            'vectors, NXPTS = #. This '
     .         //            'may indicate an error in the '
     .         //            'IK parameters for #.'         )
               CALL ERRINT ( '#', NXPTS                     )
               CALL ERRCH  ( '#', INST                      )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'        )
               CALL CHKOUT ( 'ZZGFFVIN'                     )
               RETURN

            END IF

C
C           Compute and find the length of each semi-axis vector.
C
            CALL VSUB ( SEMIPT(1,I), SVFVCT, SVSEMI(1,I) )

            SVXMAG(I) = VNORM ( SVSEMI(1,I) )

            IF ( SVXMAG(I) .EQ. 0.D0 ) THEN

               CALL SETMSG ( 'FOV semi-axis #* for @ '
     .         //            'has zero length.'        )
               CALL ERRINT ( '*', I                    )
               CALL ERRCH  ( '@', INST                 )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'   )
               CALL CHKOUT ( 'ZZGFFVIN'                )
               RETURN

            END IF

         END DO

      END IF

C
C     If we have an ellipsoidal target, and the FOV is circular or
C     elliptical, we'll create an ellipsoid whose limb coincides with
C     the FOV. This allows use to later use ZZOCCED to determine the
C     target's visibility.
C
      IF (   (      ( SVISHP .EQ. CIRFOV )
     .         .OR. ( SVISHP .EQ. ELLFOV ) )  .AND.  SVXTRG  ) THEN
C
C        Create an ellipsoid whose semi-axes are consistent with
C        ellipse in SVPLAN defined by SEMIPT. To start out, select the
C        center of the ellipsoid. We place the center along the
C        direction defined by the FOV axis, at a distance beyond
C        SVPLAN (that is, on the side of the plane opposite the
C        observer), such that a sphere centered at this point would
C        have a limb consisting of a circle of radius SVXMAG(1). If
C        CTREXT is the distance of the ellipsoid center from SVFVCT,
C        then the limb geometry requires
C
C           CTREXT / SVXMAG(1) = SVXMAG(1) / 1
C
C
         CTREXT = SVXMAG(1) ** 2

C
C        The ellipsoid's center is SVEDCT.
C
         CALL VSCL ( 1.D0 + CTREXT, SVFVCT, SVEDCT )

C
C        NOTE: in the code and discussion that follow, there are
C        references to both the FOV center SVFVCT and the ellipsoid
C        center SVEDCT. Note that the directions of the ellipsoid's
C        semi-axes point from the FOV center, NOT the ellipsoid center,
C        toward the intercepts of the FOV boundary vectors on the
C        FOV plane.
C
C        Compute the radius of the sphere centered at SVEDCT. The
C        ellipsoid's semi-axes pointing in the FOV axis direction and
C        in the direction from SVFVCT toward SEMIPT(*,1) will have this
C        length.
C
         FOVRAD(3) = SVXMAG(1) * SQRT ( 1.D0 + SVXMAG(1)**2.D0 )
         FOVRAD(1) = FOVRAD(3)

C
C        Compute the corresponding columns of the FOV semi-axis matrix.
C
C        The ellipsoid's third axis points along the FOV axis:
C
         CALL VSCL ( FOVRAD(3), SVFVCT, SVFSMX(1,3) )

C
C        The first ellipsoid semi-axis is associated with SEMIPT(*,1)
C        and also has length FOVRAD(3):
C
         CALL VHAT ( SVSEMI(1,1), VTEMP )
         CALL VSCL ( FOVRAD(1),   VTEMP,  SVFSMX(1,1) )

C
C        The ellipsoid's second semi-axis points from SVFVCT toward
C        SEMIPT(*,2). The ratio of its length to that of the other
C        semi-axis is the ratio of the length of the FOV's second
C        semi-axis to that of its first. Note that we've already ruled
C        out divide-by-zero errors here.
C
         FOVRAD(2) = ( SVXMAG(2)/SVXMAG(1) ) * FOVRAD(3)

C
C        We define the third axis using a cross product to
C        ensure we produce a matrix with positive determinant.
C
         CALL UCRSS  ( SVFSMX(1,3), SVFSMX(1,1), VTEMP )
         CALL VSCL   ( FOVRAD(2),   VTEMP,       SVFSMX(1,2) )

      END IF


      IF ( ( SVISHP .EQ. CIRFOV ) .AND. ( .NOT. SVXTRG ) ) THEN
C
C        We have a circular FOV and a point or ray target model.
C        In this case, our FOV inclusion test is simple as can
C        be: we just compare the angular separation of the
C        target and FOV axis against the angular radius of the
C        FOV. Compute and save this angular radius.
C
         SVARAD = VSEP ( SVFAXI, SVBNDS(1,1) )


      ELSE IF (       (      ( SVISHP .EQ. RECFOV )
     .                  .OR. ( SVISHP .EQ. POLFOV ) )
     .          .AND. ( .NOT.  SVXTRG               )   ) THEN
C
C        We have a rectangular or polygonal FOV and a ray or point
C        target.
C
C        We're going to represent the FOV boundary by a polygon
C        in the FOV plane SVPLAN. We want to be able to use a
C        2-dimensional winding number computation to decide whether
C        the target is within the FOV. We'll need a reference
C        frame with the Z-axis parallel to the FOV axis vector;
C        we'll represent the intersections of the boundary vectors
C        with the FOV plane in this frame. Then our 2D polygon
C        will have vertices given by the (X,Y) components of each
C        intersection.
C
         CALL VEQU ( SVFAXI, Z )

         CALL FRAME ( Z, X, Y )

         DO I = 1, 3
            SVFOVM(1,I) = X(I)
            SVFOVM(2,I) = Y(I)
            SVFOVM(3,I) = Z(I)
         END DO

C
C        Compute the intersections of the FOV boundary vectors with the
C        FOV plane. For each intercept, find the vector pointing from
C        the FOV center to that intercept. Transform each such
C        difference vector into the FOV frame. Save the projection onto
C        the FOV frame's X-Y plane.
C
         DO I = 1, SVNVRT

            CALL INRYPL ( SVORIG, SVBNDS(1,I),
     .                    SVPLAN, NXPTS,       XPT)

            IF ( NXPTS .NE. 1 ) THEN

               CALL SETMSG ( 'Error finding FOV plane intercept '
     .         //            'of FOV boundary vector #, NXPTS = #. '
     .         //            'This may indicate an error '
     .         //            'in the IK parameters for #.'          )
               CALL ERRINT ( '#', I                                 )
               CALL ERRINT ( '#', NXPTS                             )
               CALL ERRCH  ( '#', INST                              )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'                )
               CALL CHKOUT ( 'ZZGFFVIN'                             )
               RETURN

            END IF

            CALL VSUB ( XPT,   SVFVCT, VTEMP  )
            CALL MXV ( SVFOVM, VTEMP,  VTEMP2 )

            SVFPOL(1,I) = VTEMP2(1)
            SVFPOL(2,I) = VTEMP2(2)

         END DO

      END IF

      CALL CHKOUT ( 'ZZGFFVIN' )
      RETURN



C$Procedure ZZGFFVST ( GF, "is target in FOV?"  )

      ENTRY ZZGFFVST ( TIME, VISTAT )

C$ Abstract
C
C     Indicate whether the target is currently in the instrument FOV.
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
C     SPK
C     TIME
C
C$ Keywords
C
C     SEARCH
C     GEOMETRY
C
C$ Declarations
C
C     DOUBLE PRECISION      TIME
C     LOGICAL               VISTAT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TIME       I   TDB epoch (in seconds past J2000)
C     VISTAT     O   .TRUE. if the object is visible, .FALSE.
C                    otherwise.
C
C$ Detailed_Input
C
C     TIME       is the epoch of interest in TDB seconds past the
C                J2000 epoch.
C
C$ Detailed_Output
C
C     VISTAT     is a logical flag indicating the state of visibility.
C                If the target is in the instrument FOV at epoch TIME,
C                where target and instrument are those specified by the
C                last call to ZZGFFVIN, VISTAT is returned with the
C                value .TRUE.; otherwise VISTAT is .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If any SPK lookup fails, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C     2) If any frame transformation lookup fails, the error will be
C        diagnosed by routines in the call tree of this routine.
C
C     3) If the FOV is polygonal, the target is an ellipsoid,
C        and while testing whether the target is visible, an error
C        occurs due to FOV errors not detected in the initialization
C        step, the error will be diagnosed by routines in the call tree
C        of this routine.
C
C     4) If the FOV is circular or elliptical, the target is an
C        ellipsoid, and while testing whether the target is visible, an
C        error occurs due to degenerate geometry of the limb, FOV, or
C        both, the error will be diagnosed by routines in the call tree
C        of this routine.
C
C     5) If the target shape is not recognized, the error will be
C        diagnosed by routines in the call tree of this routine.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFFVU.
C
C$ Particulars
C
C     This routine determines the visibility state of the
C     configuration specified by the last call to ZZGFFVIN and the
C     input time value.
C
C$ Examples
C
C     See the umbrella routine ZZGFFVU.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW)
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFFVST' )

C
C     Initialize the state output.
C
      VISTAT = .FALSE.

C
C     The algorithm for the state determination depends on the
C     target model and the FOV shape.
C
      IF ( SVXTRG ) THEN
C
C        The target is an ephemeris object modeled as an extended
C        body. There are two branches here: one for a rectangular/
C        polygonal FOV and one for a circular/elliptical FOV.
C
C        Start by finding the observer-target position vector in the
C        target body-fixed frame.
C
         CALL SPKEZP ( SVTARG, TIME, SVTFRM, SVCORR, SVOBS, POS, LT )

C
C        Compute the target epoch.
C
         CALL ZZCOREPC ( SVCORR, TIME, LT, ETTARG )

C
C        Find the transformation from the target frame at ETTARG to the
C        instrument frame at TIME. We'll need to use J2000 as an
C        intermediate frame.
C
         CALL PXFORM ( SVTFRM,  'J2000', ETTARG, M1 )
         CALL PXFORM ( 'J2000', SVIFRM,  TIME,   M2 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVST' )
            RETURN
         END IF

         CALL MXM ( M2, M1, INSMAT )


         IF ( ( SVISHP .EQ. RECFOV ) .OR. ( SVISHP .EQ. POLFOV ) ) THEN
C
C           The FOV is a rectangle or other polygon; we treat both
C           cases the same way.
C
C           Negate POS to obtain the position of the observer with
C           respect to the target.
C
            CALL VMINUS ( POS, OBSPOS )

C
C           Find the limb in the target body-fixed frame.
C
            CALL EDLIMB ( SVTRAD(1), SVTRAD(2), SVTRAD(3),
     .                    OBSPOS,    LIMB                  )

C
C           Transform the limb from the target frame at ETTARG
C           to the instrument frame at TIME. The matrix INSMAT
C           effects just this transformation. We unpack the center
C           and semi-axis vectors of LIMB, transform them, and
C           pack the results into FVLIMB. Below, M1 and M2 are
C           simply temporary 3x3 matrices.
C
            CALL EL2CGV ( LIMB, M1(1,1), M1(1,2), M1(1,3) )

C
C           Before performing the frame transformation on the
C           limb's center, translate the center so that the
C           observer is at the origin. Since POS is expressed
C           in the target body-fixed frame, this is a convenient
C           place for the translation.
C
            CALL VADD ( POS,   M1(1,1), VTEMP )
            CALL VEQU ( VTEMP, M1(1,1)        )

            DO I = 1, 3
               CALL MXV ( INSMAT, M1(1,I), M2(1,I) )
            END DO

            CALL CGV2EL (  M2(1,1), M2(1,2), M2(1,3), FVLIMB )

C
C           All geometric objects in the following call are expressed
C           in the instrument reference frame.
C
C           The target is in the FOV if and only if ZZELVUPY finds an
C           intersection, so we use VISTAT as the "found" flag.
C
            CALL ZZELVUPY ( FVLIMB, SVORIG, SVFAXI,
     .                      SVNVRT, SVBNDS, VISTAT )


         ELSE IF (      ( SVISHP .EQ. CIRFOV )
     .             .OR. ( SVISHP .EQ. ELLFOV )  ) THEN
C
C           The FOV is a circle or ellipse. For both FOV shapes,
C           we represent the FOV by an ellipsoid in the FOV
C           frame. We can then use ZZOCCED to determine whether
C           there's any overlap of this ellipsoid and the target.
C
C           We'll perform the occultation test in the instrument frame,
C           so we'll need to represent the observer-target position
C           and target semi-axes in that frame.
C
C           Transform the target position to the instrument frame.
C
            CALL MXV ( INSMAT, POS, TRGCTR )

C
C           The columns of INSMAT are the target body's semi-axis
C           direction vectors; we scale these by the target radii
C           to obtain the semi-axis matrix for the target.
C
            DO I = 1, 3
               CALL VSCL ( SVTRAD(I), INSMAT(1,I), TRGSMX(1,I) )
            END DO

            OCSTAT = ZZOCCED ( SVORIG, SVEDCT, SVFSMX, TRGCTR, TRGSMX )

C
C           A return code of zero indicates no occultation. Any other
C           return code indicates a non-empty intersection of the
C           target and FOV.
C
            VISTAT = OCSTAT .NE. 0

         ELSE
C
C           This is an unexpected FOV shape. We should have prevented
C           this problem in the initialization step, but we repeat the
C           check here for safety.
C
            CALL SETMSG ( 'The target body # has shape #; the only '
     .      //            'supported shapes are ELLIPSOID, POINT, '
     .      //            'and RAY.'                                )
            CALL ERRCH  ( '#',  SVTNAM                              )
            CALL ERRCH  ( '#',  SVISHP                              )
            CALL SIGERR ( 'SPICE(INVALIDSHAPE)'                     )
            CALL CHKOUT ( 'ZZGFFVST'                                )
            RETURN

         END IF

C
C        This is the end of the ellipsoidal target case. At this
C        point, VISTAT is set.
C

      ELSE
C
C        The target is a ray or an ephemeris object modeled as a point.
C        In either case, we want to obtain the aberration-corrected
C        observer-target vector.
C
         IF ( SVURAY ) THEN
C
C           The target is represented by a ray expressed in the
C           frame SVTFRM.
C
C           Normally we'd need to correct the orientation of SVTFRM
C           for light time between the center of that frame and the
C           observer. But since light time corrections are not allowed
C           for targets represented by rays, we evaluate SVTFRM
C           at the current epoch TIME.
C
            CALL PXFORM ( SVTFRM, SVIFRM, TIME, INSMAT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFFVST' )
               RETURN
            END IF

C
C           Transform the ray's direction vector to the instrument
C           frame.
C
            CALL MXV ( INSMAT, SVRDIR, DIR )

C
C           If we need to correct the ray's direction for stellar
C           aberration, do it now.
C
            IF ( SVUSTL ) THEN
C
C              Find the state of the observer relative to the
C              solar system barycenter in the J2000 frame.
C
               CALL SPKSSB ( SVOBS, TIME, 'J2000', STOBS )

C
C              Convert the direction vector to the J2000 frame.
C
               CALL PXFORM ( SVIFRM, 'J2000', TIME, M1 )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZGFFVST' )
                  RETURN
               END IF

               CALL MXV ( M1, DIR, VTEMP )

C
C              Apply the stellar aberration correction.
C
               IF ( SVXMIT ) THEN
C
C                 Use the transmission correction.
C
                  CALL STLABX ( VTEMP, STOBS(4), VTEMP2 )
               ELSE
                  CALL STELAB ( VTEMP, STOBS(4), VTEMP2 )
               END IF

C
C              Map the direction vector back to the instrument
C              frame.
C
               CALL MTXV ( M1, VTEMP2, DIR )

            END IF
C
C           The target direction in the instrument frame DIR has
C           been computed.
C

         ELSE
C
C           The target is an ephemeris object. Look up the
C           target's position relative to the observer.
C
C           Note for the maintenance programmer: don't think of
C           changing this call to look up the position in the
C           instrument frame. :) Since we don't have a guarantee that
C           the instrument frame is centered on the observer (the frame
C           could be J2000, for example), and since we don't want to
C           correct the orientation of the instrument frame for light
C           time, we look up the direction vector in the J2000 frame
C           and then map it to the instrument frame.
C
            CALL SPKEZP ( SVTARG,  TIME,   'J2000',
     .                    SVCORR,  SVOBS,  VTEMP, LT )

            CALL PXFORM ( 'J2000', SVIFRM, TIME,  M1 )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFFVST' )
               RETURN
            END IF

            CALL MXV ( M1, VTEMP, DIR )

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVST' )
            RETURN
         END IF

C
C        The observer-target direction vector DIR is set.
C
C        The determination of whether the ray is in the FOV depends
C        on the FOV shape.
C
         SEP = VSEP( DIR, SVFAXI )

         IF ( SVISHP .EQ. CIRFOV ) THEN
C
C           Just compare the angular separation of POS with the
C           FOV axis direction against the FOV angular radius SVARAD.
C
            VISTAT =  SEP .LE. SVARAD


         ELSE IF ( SEP .GT. SVARAD ) THEN
C
C           The FOV is an ellipse or polygon.
C
C           The angular separation of target and FOV axis is
C           greater than the angular radius of the exclusion
C           cone. The target can't be seen.
C
            VISTAT = .FALSE.

         ELSE
C
C           The FOV is an ellipse or polygon.
C
C           The angular separation of target and FOV axis is
C           less than or equal to than the angular radius of the
C           exclusion code, so the target may be visible.
C
C           Find the intersection of the ray emanating from the
C           observer, and having direction vector POS, with the FOV
C           plane.
C
            CALL INRYPL ( SVORIG, DIR, SVPLAN, NXPTS, XPT )

C
C           If there's no intersection, the target isn't visible.
C
            IF ( NXPTS .EQ. 0 ) THEN

               VISTAT = .FALSE.

            ELSE IF ( NXPTS .NE. 1 ) THEN
C
C              "This can't happen." :)
C
               CALL SETMSG ( 'By construction, the vertex of '
     .         //            'the observer-target ray can''t '
     .         //            'lie in the FOV plane. If '
     .         //            'somehow it does, we have a serious '
     .         //            'problem.'                           )
               CALL SIGERR ( 'SPICE(BUG)'                         )
               CALL CHKOUT ( 'ZZGFFVST'                           )
               RETURN

            ELSE
C
C              NXPTS is 1.
C
C              Find the vector from the center of the FOV to XPT.
C              Call this vector FOVPT.
C
               CALL VSUB ( XPT, SVFVCT, FOVPT )

               IF ( SVISHP .EQ. ELLFOV ) THEN
C
C                 The FOV shape is elliptical. To decide whether FOVPT
C                 is within the FOV, compute the level surface
C                 parameter
C
C                                   2              2
C                    L  =  ( x / a )   +  ( y / b )
C
C                 and compare L to 1. We'll use the variable COORD
C                 to represent the coordinates (x,y).
C
C                 We've already eliminated zero divisors in the
C                 initialization routine.
C
                  DO I = 1, 2
                     COORD(I) = VDOT( FOVPT, SVSEMI(1,I) )  /  SVXMAG(I)
                  END DO

                  L =   ( COORD(1) / SVXMAG(1) )**2.D0
     .                + ( COORD(2) / SVXMAG(2) )**2.D0

C
C                 The target is visible if FOVPT is inside the FOV
C                 ellipse; this condition is indicated by L <= 1.
C
                  VISTAT = L .LE. 1.D0


               ELSE IF ( SVISHP .EQ. POLFOV ) THEN
C
C                The FOV is a polygon. Convert FOVPT to the FOV frame,
C                then find the winding number of the FOV about the X-Y
C                projection of FOVPT.
C
                  CALL MXV ( SVFOVM, FOVPT, VTEMP )

                  PNT2D(1) = VTEMP(1)
                  PNT2D(2) = VTEMP(2)

                  W        = ZZWIND2D ( SVNVRT, SVFPOL, PNT2D )

C
C                 Any non-zero winding number indicates that the
C                 FOV polygon wraps around the point representing
C                 the intercept of the target direction with the
C                 FOV plane.
C
                  VISTAT   = W .NE. 0

               ELSE
C
C                 This is an unexpected FOV shape. We should have
C                 prevented this problem in the initialization step,
C                 but we repeat the check here for safety.
C
                  CALL SETMSG ( 'Instrument #''s FOV has shape #; '
     .            //            'the only supported shapes are '
     .            //            'ELLIPSE, CIRCLE, and POLYGON.'    )
                  CALL ERRCH  ( '#',  SVINAM                       )
                  CALL ERRCH  ( '#',  SVISHP                       )
                  CALL SIGERR ( 'SPICE(INVALIDSHAPE)'              )
                  CALL CHKOUT ( 'ZZGFFVST'                         )
                  RETURN

               END IF
C
C              We've performed visibility tests for elliptical or
C              polygonal FOVs. VISTAT is set.
C
            END IF
C
C           We've processed the intercept found by the INRYPL call,
C           or, if the intercept count was not 1, indicated that the
C           target is not visible. VISTAT is set.
C
         END IF
C
C        We've processed both the ray and point ephemeris object
C        cases. VISTAT is set.
C
      END IF
C
C     We've processed all target representation/FOV shape cases.
C

      CALL CHKOUT ( 'ZZGFFVST' )
      RETURN
      END
