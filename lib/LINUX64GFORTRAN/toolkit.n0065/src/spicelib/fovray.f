C$Procedure      FOVRAY ( Is ray in FOV at time? )

      SUBROUTINE FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .                    OBSRVR, ET,     VISIBL        )

C$ Abstract
C
C     Determine if a specified ray is within the field-of-view (FOV) of
C     a specified instrument at a given time.
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
C     FRAMES
C     KERNEL
C     NAIF_IDS
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     EVENT
C     FOV
C     GEOMETRY
C     INSTRUMENT
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'

      CHARACTER*(*)         INST
      DOUBLE PRECISION      RAYDIR ( 3 )
      CHARACTER*(*)         RFRAME
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      ET
      LOGICAL               VISIBL

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     INST       I   Name or ID code string of the instrument.
C     RAYDIR     I   Ray's direction vector.
C     RFRAME     I   Reference frame of ray's direction vector.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name or ID code string of the observer.
C     ET         I   Time of the observation (seconds past J2000).
C     VISIBL     O   Visibility flag (true/false).
C
C$ Detailed_Input
C
C     INST       indicates the name of an instrument, such as a
C                spacecraft-mounted framing camera. The field of view
C                (FOV) of the instrument will be used to determine if
C                the direction from the observer to a target,
C                represented as a ray, is visible with respect to the
C                instrument.
C
C                The position of the instrument INST is considered to
C                coincide with that of the ephemeris object OBSRVR (see
C                description below).
C
C                The size of the instrument's FOV is constrained by the
C                following: There must be a vector A such that all of
C                the instrument's FOV boundary vectors have an angular
C                separation from A of less than (pi/2)-MARGIN radians
C                (see description below). For FOVs that are circular or
C                elliptical, the vector A is the boresight. For FOVs
C                that are rectangular or polygonal, the vector A is
C                calculated.
C
C                See the header of the SPICELIB routine GETFOV for a
C                description of the required parameters associated with
C                an instrument.
C
C                Both object names and NAIF IDs are accepted. For
C                example, both 'CASSINI_ISS_NAC' and '-82360' are
C                accepted. Case and leading or trailing blanks are not
C                significant in the string.
C
C     RAYDIR     is the direction vector defining a ray of interest.
C                The ray emanates from the location of the ephemeris
C                object designated by the input argument OBSRVR and
C                is expressed relative to the reference frame
C                designated by RFRAME (see descriptions below).
C
C     RFRAME     is the name of the reference frame associated with
C                the input ray's direction vector RAYDIR. Note: RFRAME
C                does not need to be the instrument's reference frame.
C
C                Since light time corrections are not supported for
C                rays, the orientation of the frame is always evaluated
C                at the epoch associated with the observer, as opposed
C                to the epoch associated with the light-time corrected
C                position of the frame center.
C
C                Case, leading and trailing blanks are not significant
C                in the string.
C
C     ABCORR     indicates the aberration corrections to be applied
C                when computing the ray's direction.
C
C                The supported aberration correction options are:
C
C                   'NONE'          No correction.
C                   'S'             Stellar aberration correction,
C                                   reception case.
C                   'XS'            Stellar aberration correction,
C                                   transmission case.
C
C                For detailed information, see the geometry finder
C                required reading, gf.req.
C
C                Case, leading and trailing blanks are not significant
C                in the string.
C
C     OBSRVR     is the name of the body from which the target
C                represented by RAYDIR is observed. The instrument
C                designated by INST is treated as if it were co-located
C                with the observer.
C
C                Both object names and NAIF IDs are accepted. For
C                example, both 'CASSINI' and '-82' are accepted. Case
C                and leading or trailing blanks are not significant in
C                the string.
C
C     ET         is the observation time in seconds past the J2000
C                epoch.
C
C$ Detailed_Output
C
C     VISIBL     is .TRUE. if the ray is "visible", or in the
C                field-of-view, of INST at the time ET. Otherwise,
C                VISIBL is .FALSE.
C
C$ Parameters
C
C     MAXVRT     is the maximum number of vertices that may be used
C                to define the boundary of the specified instrument's
C                field of view.
C
C     MARGIN     is a small positive number used to constrain the
C                orientation of the boundary vectors of polygonal
C                FOVs. Such FOVs must satisfy the following constraints:
C
C                   1)  The boundary vectors must be contained within
C                       a right circular cone of angular radius less
C                       than than (pi/2) - MARGIN radians; in other
C                       words, there must be a vector A such that all
C                       boundary vectors have angular separation from
C                       A of less than (pi/2)-MARGIN radians.
C
C                   2)  There must be a pair of boundary vectors U, V
C                       such that all other boundary vectors lie in
C                       the same half space bounded by the plane
C                       containing U and V. Furthermore, all other
C                       boundary vectors must have orthogonal
C                       projections onto a specific plane normal to
C                       this plane (the normal plane contains the angle
C                       bisector defined by U and V) such that the
C                       projections have angular separation of at least
C                       2*MARGIN radians from the plane spanned by U
C                       and V.
C
C                MARGIN is currently set to 1.D-6.
C
C     See INCLUDE file gf.inc for declarations and descriptions of
C     parameters used throughout the GF system.
C
C$ Exceptions
C
C     1)  If the observer's name cannot be mapped to a NAIF ID code, the
C         error SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If the aberration correction flag calls for light time
C         correction, the error SPICE(INVALIDOPTION) is signaled.
C
C     3)  If the ray's direction vector is zero, the error
C         SPICE(ZEROVECTOR) is signaled.
C
C     4)  If the instrument name INST does not have corresponding NAIF
C         ID code, the error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     5)  If the FOV parameters of the instrument are not present in
C         the kernel pool, the error will be diagnosed by routines
C         in the call tree of this routine.
C
C     6)  If the FOV boundary has more than MAXVRT vertices, the error
C         will be diagnosed by routines in the call tree of this
C         routine.
C
C     7)  If the instrument FOV shape is a polygon or rectangle, and
C         this routine cannot find a ray R emanating from the FOV
C         vertex such that maximum angular separation of R and any FOV
C         boundary vector is within the limit (pi/2)-MARGIN radians,
C         the error will be diagnosed by a routine in the call tree of
C         this routine. If the FOV is any other shape, the same error
C         check will be applied with the instrument boresight vector
C         serving the role of R.
C
C     8)  If the loaded kernels provide insufficient data to compute a
C         requested state vector, the error will be diagnosed by a
C         routine in the call tree of this routine.
C
C     9)  If an error occurs while reading an SPK or other kernel file,
C         the error will be diagnosed by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     Appropriate SPICE kernels must be loaded by the calling program
C     before this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for the observer at the time
C          ET. If aberration corrections are used, the state of the
C          observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C
C        - Data defining the reference frame in which the instrument's
C          FOV is defined must be available in the kernel pool.
C          Additionally the name INST must be associated with an ID
C          code.
C
C        - IK data: the kernel pool must contain data such that
C          the SPICELIB routine GETFOV may be called to obtain
C          parameters for INST.
C
C     The following data may be required:
C
C        - CK data: if the frame in which the instrument's FOV is
C          defined is fixed to a spacecraft, at least one CK file will
C          be needed to permit transformation of vectors between that
C          frame and the J2000 frame.
C
C        - SCLK data: if a CK file is needed, an associated SCLK
C          kernel is required to enable conversion between encoded SCLK
C          (used to time-tag CK data) and barycentric dynamical time
C          (TDB).
C
C        - Since the input ray direction may be expressed in any
C          frame, additional FKs, CKs, SCLK kernels, PCKs, and SPKs
C          may be required to map the direction to the J2000 frame.
C
C     Kernel data are normally loaded via FURNSH once per program run,
C     NOT every time this routine is called.
C
C$ Particulars
C
C     To treat the target as an ephemeris object rather than a ray, use
C     the higher-level SPICELIB routine FOVTRG. FOVTRG may be used to
C     determine if ephemeris objects such as Saturn are visible in an
C     instrument's FOV at a given time.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The Cassini Ultraviolet Imaging Spectrograph (UVIS)
C        has been used to measure variations in starlight as
C        rings and moons occult Cassini's view of the stars.
C        One of these events happened at 2008-054T21:31:55.158 UTC.
C        Let's verify that Epsilon CMa (Adhara) was in the
C        Cassini UVIS field-of-view at the observation time.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File name: fovray_ex.tm
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
C              File name                      Contents
C              ---------                      --------
C              naif0010.tls                   Leapseconds
C              cpck26Jan2007.tpc              Satellite orientation and
C                                             radii
C              cas00145.tsc                   Cassini SCLK
C              cas_v40.tf                     Cassini frames
C              cas_uvis_v06.ti                Cassini UVIS instrument
C              080428R_SCPSE_08045_08067.bsp  Merged spacecraft,
C                                             planetary, and satellite
C                                             ephemeris
C              08052_08057ra.bc               Orientation for Cassini
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'cpck26Jan2007.tpc'
C                                  'naif0010.tls'
C                                  'cas00145.tsc'
C                                  'cas_v40.tf'
C                                  'cas_uvis_v06.ti'
C                                  '080428R_SCPSE_08045_08067.bsp'
C                                  '08052_08057ra.bc')
C
C           \begintext
C
C        Example code begins here.
C
C           PROGRAM FOVRAY_EX
C           IMPLICIT NONE
C     C
C     C     SPICELIB functions
C     C
C     C     Returns radians per degree.
C     C
C           DOUBLE PRECISION      RPD
C
C     C
C     C     Local parameters
C     C
C           CHARACTER*(*)         META
C           PARAMETER           ( META   =  'fovray_ex.tm' )
C
C           CHARACTER*(*)         TIMFMT
C           PARAMETER           ( TIMFMT =
C          .      'YYYY-MON-DD HR:MN:SC.##::TDB (TDB)' )
C
C     C
C     C     This is the UTC time of the observation.
C     C
C           CHARACTER*(*)         TIME
C           PARAMETER           ( TIME = '2008-054T21:31:55.158' )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(30)        TIMSTR
C
C           DOUBLE PRECISION      DEC
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      RA
C           DOUBLE PRECISION      RAYDIR ( 3 )
C
C           LOGICAL               VISIBL
C
C     C
C     C     RA and DEC are the right ascension and declination
C     C     of Epsilon CMa in degrees.
C     C
C           RA   = 104.656
C           DEC  = -28.972
C
C     C
C     C     Load the kernels.
C     C
C           CALL FURNSH ( META )
C
C     C
C     C     Convert the observation time from UTC to ET.
C     C
C           CALL STR2ET ( TIME, ET )
C
C     C
C     C     Create a unit direction vector pointing from Cassini
C     C     to the specified star. For details on corrections such
C     C     as parallax, please see the example in GFRFOV.
C     C
C           CALL RADREC ( 1.D0, RA*RPD(), DEC*RPD(), RAYDIR )
C
C     C
C     C     Is the star in the field-of-view of Cassini's UVIS?
C     C
C           CALL FOVRAY ( 'CASSINI_UVIS_FUV_OCC',  RAYDIR,
C          .              'J2000', 'S', 'CASSINI', ET, VISIBL )
C
C     C
C     C     Put the time in a specified format for output.
C     C
C           CALL TIMOUT ( ET, TIMFMT, TIMSTR )
C
C           IF ( VISIBL ) THEN
C              WRITE(*,*) 'Epsilon CMa was visible from the ',
C          .              'Cassini UVIS instrument at '
C              WRITE(*,*) TIMSTR
C           END IF
C
C           END
C
C        When this program was executed using gfortran on a PC Linux
C        64 bit environment, the output was:
C
C           Epsilon CMa was visible from the Cassini UVIS instrument at
C           2008-FEB-23 21:33:00.34 (TDB)
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     S.C. Krening  (JPL)
C     N.J. Bachman  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0  15-FEB-2012 (SCK) (NJB)
C
C-&


C$ Index_Entries
C
C     Ray in instrument FOV at specified time
C     Ray in instrument field_of_view at specified time
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
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'FOVRAY' )

C
C     Note to maintenance programmer: input exception checks
C     are delegated to ZZGFFVU. If the implementation of that
C     routine changes, or if this routine is modified to call
C     a different routine in place of ZZGFFVU, then the error
C     handling performed by ZZGFFVU will have to be performed
C     here or in a routine called by this routine.
C
C     Initialize the visibility calculation.
C
      CALL ZZGFFVIN ( INST,   RYSHAP, RAYDIR, ' ',
     .                RFRAME, ABCORR, OBSRVR )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'FOVRAY' )
         RETURN
      END IF
C
C     Calculate the visibility state.
C
      CALL ZZGFFVST ( ET, VISIBL )

      CALL CHKOUT ( 'FOVRAY' )
      RETURN
      END






