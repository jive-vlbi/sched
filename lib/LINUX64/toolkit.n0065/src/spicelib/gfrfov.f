C$Procedure GFRFOV ( GF, is ray in FOV? )
 
      SUBROUTINE GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR, 
     .                    OBSRVR, STEP,   CNFINE, RESULT )
 
C$ Abstract
C
C     Determine time intervals when a specified ray intersects the
C     space bounded by the field-of-view (FOV) of a specified
C     instrument.
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
C     GF
C     KERNEL
C     NAIF_IDS
C     PCK
C     SPK
C     TIME
C     WINDOWS  
C
C$ Keywords
C
C     EVENT
C     FOV
C     GEOMETRY
C     INSTRUMENT
C     SEARCH
C     WINDOW      
C
C$ Declarations
 
      IMPLICIT NONE
      
      INCLUDE               'gf.inc'
      INCLUDE               'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         INST
      DOUBLE PRECISION      RAYDIR ( 3 )
      CHARACTER*(*)         RFRAME
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      DOUBLE PRECISION      RESULT ( LBCELL : * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MARGIN     P   Minimum complement of FOV cone angle.
C     LBCELL     P   SPICE Cell lower bound.
C     CNVTOL     P   Convergence tolerance.
C     MAXVRT     P   Maximum number of FOV boundary vertices.
C     ZZGET      P   ZZHOLDD retrieves a stored DP value.
C     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance.
C     INST       I   Name of the instrument.
C     RAYDIR     I   Ray's direction vector.
C     RFRAME     I   Reference frame of ray's direction vector.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     STEP       I   Step size in seconds for finding FOV events.
C     CNFINE     I   SPICE window to which the search is restricted.
C     RESULT     O   SPICE window containing results.
C     
C
C$ Detailed_Input
C
C
C     INST       indicates the name of an instrument, such as a
C                spacecraft-mounted framing camera, the field of view
C                (FOV) of which is to be used for an target intersection
C                search: the direction from the observer to a target
C                is represented by a ray, and times when the specified
C                ray intersects the region of space bounded by the FOV
C                are sought.
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
C     RAYDIR     is the direction vector associated with a ray
C                representing a target. The ray emanates from the
C                location of the ephemeris object designated by the
C                input argument OBSRVR and is expressed relative to the
C                reference frame designated by RFRAME (see descriptions
C                below).
C
C
C     RFRAME     is the name of the reference frame associated with 
C                the input ray's direction vector RAYDIR.
C
C                Since light time corrections are not supported for
C                rays, the orientation of the frame is always evaluated
C                at the epoch associated with the observer, as opposed
C                to the epoch associated with the light-time corrected
C                position of the frame center.
C                
C                Case and leading or trailing blanks bracketing a
C                non-blank frame name are not significant in the string
C                RFRAME.
C
C
C     ABCORR     indicates the aberration corrections to be applied
C                when computing the ray's direction.
C
C                The supported aberration correction options are
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
C                in the string ABCORR.
C 
C
C     OBSRVR     is the name of the body from which the target
C                represented by RAYDIR is observed. The instrument
C                designated by INST is treated as if it were co-located
C                with the observer.

C                Optionally, you may supply the integer NAIF ID code
C                for the body as a string.
C
C                Case and leading or trailing blanks are not
C                significant in the string OBSRVR.
C
C
C     STEP       is the step size to be used in the search. STEP must
C                be shorter than any interval, within the confinement
C                window, over which the specified condition is met. In
C                other words, STEP must be shorter than the shortest
C                visibility event that the user wishes to detect. STEP
C                also must be shorter than the minimum duration
C                separating any two visibility events. However, STEP
C                must not be *too* short, or the search will take an
C                unreasonable amount of time.
C
C                The choice of STEP affects the completeness but not
C                the precision of solutions found by this routine; the
C                precision is controlled by the convergence tolerance.
C                See the discussion of the parameter CNVTOL for
C                details.
C
C                STEP has units of seconds. 
C
C
C     CNFINE     is a SPICE window that confines the time period over
C                which the specified search is conducted. CNFINE may
C                consist of a single interval or a collection of 
C                intervals. 
C
C                The endpoints of the time intervals comprising CNFINE
C                are interpreted as seconds past J2000 TDB.
C                
C                See the Examples section below for a code example 
C                that shows how to create a confinement window.
C                
C                CNFINE must be initialized by the caller via the
C                SPICELIB routine SSIZED.
C
C$ Detailed_Output
C
C
C     RESULT     is a SPICE window representing the set of time
C                intervals, within the confinement period, when the
C                input ray is "visible"; that is, when the ray is
C                contained in the space bounded by the specified
C                instrument's field of view.
C
C                The endpoints of the time intervals comprising RESULT
C                are interpreted as seconds past J2000 TDB.
C
C                If RESULT is non-empty on input, its contents
C                will be discarded before GFRFOV conducts its
C                search.
C
C$ Parameters
C
C     LBCELL     is the lower bound for SPICE cell arrays.
C
C     CNVTOL     is the convergence tolerance used for finding
C                endpoints of the intervals comprising the result
C                window. CNVTOL is used to determine when binary
C                searches for roots should terminate: when a root is
C                bracketed within an interval of length CNVTOL; the
C                root is considered to have been found.
C
C                The accuracy, as opposed to precision, of roots found
C                by this routine depends on the accuracy of the input
C                data. In most cases, the accuracy of solutions will be
C                inferior to their precision.
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
C                 MARGIN is currently set to 1.D-12.
C
C
C     See INCLUDE file gf.inc for declarations and descriptions of 
C     parameters used throughout the GF system.
C
C$ Exceptions
C
C
C     1)  In order for this routine to produce correct results,
C         the step size must be appropriate for the problem at hand.
C         Step sizes that are too large may cause this routine to miss
C         roots; step sizes that are too small may cause this routine
C         to run unacceptably slowly and in some cases, find spurious
C         roots.
C
C         This routine does not diagnose invalid step sizes, except
C         that if the step size is non-positive, the error 
C         SPICE(INVALIDSTEPSIZE) will be signaled.
C
C     2)  Due to numerical errors, in particular,
C
C            - Truncation error in time values
C            - Finite tolerance value
C            - Errors in computed geometric quantities
C
C         it is *normal* for the condition of interest to not always be
C         satisfied near the endpoints of the intervals comprising the
C         result window.
C
C         The result window may need to be contracted slightly by the
C         caller to achieve desired results. The SPICE window routine
C         WNCOND can be used to contract the result window.
C
C     3)  If the observer's name cannot be mapped to an ID code, the
C         error SPICE(IDCODENOTFOUND) is signaled.
C
C     4)  If the aberration correction flag calls for light time
C         correction, the error SPICE(INVALIDOPTION) is signaled.
C
C     5)  If the ray's direction vector is zero, the error
C         SPICE(ZEROVECTOR) is signaled.
C
C     6)  If the instrument name INST does not have corresponding NAIF
C         ID code, the error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     7)  If the FOV parameters of the instrument are not present in
C         the kernel pool, the error will be be diagnosed by routines
C         in the call tree of this routine.
C
C     8)  If the FOV boundary has more than MAXVRT vertices, the error
C         will be be diagnosed by routines in the call tree of this
C         routine.
C
C     9)  If the instrument FOV is polygonal, and this routine cannot 
C         find a ray R emanating from the FOV vertex such that maximum
C         angular separation of R and any FOV boundary vector is within
C         the limit (pi/2)-MARGIN radians, the error will be diagnosed
C         by a routine in the call tree of this routine. If the FOV
C         is any other shape, the same error check will be applied with
C         the instrument boresight vector serving the role of R.
C
C     10) If the loaded kernels provide insufficient data to compute a
C         requested state vector, the error will be diagnosed by a
C         routine in the call tree of this routine.
C
C     11) If an error occurs while reading an SPK or other kernel file,
C         the error will be diagnosed by a routine in the call tree 
C         of this routine.
C
C     12) If the output SPICE window RESULT has insufficient capacity
C         to contain the number of intervals on which the specified
C         visibility condition is met, the error will be diagnosed
C         by a routine in the call tree of this routine. If the result
C         window has size less than 2, the error SPICE(WINDOWTOOSMALL)
C         will be signaled by this routine.
C
C$ Files
C
C     Appropriate SPICE kernels must be loaded by the calling program
C     before this routine is called.
C
C     The following data are required:
C
C        - SPK data:  ephemeris data for the observer for the period
C          defined by the confinement window 'CNFINE' must be loaded.
C          If aberration corrections are used, the state of the
C          observer relative to the solar system barycenter must be
C          calculable from the available ephemeris data. Typically
C          ephemeris data are made available by loading one or more SPK
C          files via FURNSH.
C
C        - Data defining the reference frame associated with the
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
C        - CK data: if the instrument frame is fixed to a spacecraft,
C          at least one CK file will be needed to permit transformation
C          of vectors between that frame and the J2000 frame.
C
C        - SCLK data: if a CK file is needed, an associated SCLK
C          kernel is required to enable conversion between encoded SCLK
C          (used to time-tag CK data) and barycentric dynamical time
C          (TDB).
C
C        - Since the input ray direction may be expressed in any 
C          frame, FKs, CKs, SCLK kernels, PCKs, and SPKs may be
C          required to map the direction to the J2000 frame.
C
C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C     This routine determines a set of one or more time intervals when
C     the specified ray in contained within the field of view of a
C     specified instrument. We'll use the term "visibility event" to
C     designate such an appearance. The set of time intervals resulting
C     from the search is returned as a SPICE window.
C
C     This routine provides a simpler, but less flexible, interface
C     than does the SPICELIB routine GFFOVE for conducting searches for
C     visibility events. Applications that require support for progress
C     reporting, interrupt handling, non-default step or refinement
C     functions, or non-default convergence tolerance should call
C     GFFOVE rather than this routine.
C
C     Below we discuss in greater detail aspects of this routine's
C     solution process that are relevant to correct and efficient use
C     of this routine in user applications.
C
C
C     The Search Process
C     ==================
C
C     The search for visibility events is treated as a search for state
C     transitions: times are sought when the state of the ray
C     changes from "not visible" to "visible" or vice versa.
C
C     Step Size
C     =========
C
C     Each interval of the confinement window is searched as follows:
C     first, the input step size is used to determine the time
C     separation at which the visibility state will be sampled.
C     Starting at the left endpoint of an interval, samples will be
C     taken at each step. If a state change is detected, a root has
C     been bracketed; at that point, the "root"--the time at which the
C     state change occurs---is found by a refinement process, for
C     example, via binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the visibility state is constant:
C     the step size should be shorter than the shortest visibility event
C     duration and the shortest period between visibility events, within
C     the confinement window.
C
C     Having some knowledge of the relative geometry of the ray and
C     observer can be a valuable aid in picking a reasonable step size.
C     In general, the user can compensate for lack of such knowledge by
C     picking a very short step size; the cost is increased computation
C     time.
C
C     Note that the step size is not related to the precision with which
C     the endpoints of the intervals of the result window are computed.
C     That precision level is controlled by the convergence tolerance.
C
C
C     Convergence Tolerance
C     =====================
C
C     Once a root has been bracketed, a refinement process is used to
C     narrow down the time interval within which the root must lie.
C     This refinement process terminates when the location of the root
C     has been determined to within an error margin called the
C     "convergence tolerance." The default convergence tolerance 
C     used by this routine is set by the parameter CNVTOL (defined
C     in gf.inc).
C 
C     The value of CNVTOL is set to a "tight" value so that the
C     tolerance doesn't become the limiting factor in the accuracy of
C     solutions found by this routine. In general the accuracy of input
C     data will be the limiting factor.
C
C     The user may change the convergence tolerance from the default
C     CNVTOL value by calling the routine GFSTOL, e.g.
C
C        CALL GFSTOL( tolerance value )
C
C     Call GFSTOL prior to calling this routine. All subsequent
C     searches will use the updated tolerance value.
C
C     Setting the tolerance tighter than CNVTOL is unlikely to be 
C     useful, since the results are unlikely to be more accurate. 
C     Making the tolerance looser will speed up searches somewhat,
C     since a few convergence steps will be omitted. However, in most
C     cases, the step size is likely to have a much greater effect 
C     on processing time than would the convergence tolerance.
C
C
C     The Confinement Window
C     ======================
C
C     The simplest use of the confinement window is to specify a time
C     interval within which a solution is sought. However, the
C     confinement window can, in some cases, be used to make searches
C     more efficient. Sometimes it's possible to do an efficient search
C     to reduce the size of the time period over which a relatively
C     slow search of interest must be performed. For an example, see
C     the program CASCADE in the GF Example Programs chapter of the GF
C     Required Reading, gf.req.
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
C     1) This example is an extension of example #1 in the
C        header of
C
C           GFTFOV
C
C        The problem statement for that example is
C
C           Search for times when Saturn's satellite Phoebe is within
C           the FOV of the Cassini narrow angle camera
C           (CASSINI_ISS_NAC). To simplify the problem, restrict the
C           search to a short time period where continuous Cassini bus
C           attitude data are available.
C
C           Use a step size of 10 seconds to reduce chances of missing
C           short visibility events.
C
C        Here we search the same confinement window for times when a
C        selected background star is visible. We use the FOV of the
C        Cassini ISS wide angle camera (CASSINI_ISS_WAC) to enhance the
C        probability of viewing the star.
C
C        The star we'll use has catalog number 6000 in the Hipparcos
C        Catalog. The star's J2000 right ascension and declination,
C        proper motion, and parallax are taken from that catalog.
C
C        Use the meta-kernel from the GFTFOV example:
C
C
C           KPL/MK
C
C           File name: gftfov_ex1.tm
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
C              naif0009.tls                  Leapseconds
C              cpck05Mar2004.tpc             Satellite orientation and
C                                            radii
C              981005_PLTEPH-DE405S.bsp      Planetary ephemeris
C              020514_SE_SAT105.bsp          Satellite ephemeris
C              030201AP_SK_SM546_T45.bsp     Spacecraft ephemeris
C              cas_v37.tf                    Cassini FK
C              04135_04171pc_psiv2.bc        Cassini bus CK
C              cas00084.tsc                  Cassini SCLK kernel
C              cas_iss_v09.ti                Cassini IK
C              
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'naif0009.tls',
C                                  'cpck05Mar2004.tpc',
C                                  '981005_PLTEPH-DE405S.bsp',
C                                  '020514_SE_SAT105.bsp',
C                                  '030201AP_SK_SM546_T45.bsp',
C                                  'cas_v37.tf',
C                                  '04135_04171pc_psiv2.bc',
C                                  'cas00084.tsc',
C                                  'cas_iss_v09.ti'            )
C           \begintext
C
C
C
C        Example code begins here.
C
C
C           PROGRAM EX1
C           IMPLICIT NONE
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      J1950
C           DOUBLE PRECISION      J2000
C           DOUBLE PRECISION      JYEAR
C           DOUBLE PRECISION      RPD
C
C           INTEGER               WNCARD
C
C     C
C     C     Local parameters
C     C
C           CHARACTER*(*)         META
C           PARAMETER           ( META   = 'gftfov_ex1.tm' )
C
C           CHARACTER*(*)         TIMFMT
C           PARAMETER           ( TIMFMT =
C          .      'YYYY-MON-DD HR:MN:SC.######::TDB (TDB)' )
C
C
C           DOUBLE PRECISION      AU
C           PARAMETER           ( AU     = 149597870.693D0 )
C
C           INTEGER               LBCELL
C           PARAMETER           ( LBCELL = -5 )
C
C           INTEGER               MAXWIN
C           PARAMETER           ( MAXWIN = 10000 )
C
C           INTEGER               CORLEN
C           PARAMETER           ( CORLEN = 10 )
C
C           INTEGER               BDNMLN
C           PARAMETER           ( BDNMLN = 36 )
C
C           INTEGER               FRNMLN
C           PARAMETER           ( FRNMLN = 32 )
C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 35 )
C
C           INTEGER               LNSIZE
C           PARAMETER           ( LNSIZE = 80 )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(CORLEN)    ABCORR
C           CHARACTER*(BDNMLN)    INST
C           CHARACTER*(LNSIZE)    LINE
C           CHARACTER*(BDNMLN)    OBSRVR
C           CHARACTER*(FRNMLN)    RFRAME
C           CHARACTER*(TIMLEN)    TIMSTR ( 2 )
C
C           DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      DEC
C           DOUBLE PRECISION      DECEPC
C           DOUBLE PRECISION      DECPM
C           DOUBLE PRECISION      DECDEG
C           DOUBLE PRECISION      DECDG0
C           DOUBLE PRECISION      DTDEC
C           DOUBLE PRECISION      DTRA
C           DOUBLE PRECISION      ENDPT  ( 2 )
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      ET1
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      PARLAX
C           DOUBLE PRECISION      PLXDEG
C           DOUBLE PRECISION      POS    ( 3 )
C           DOUBLE PRECISION      PSTAR  ( 3 )
C           DOUBLE PRECISION      RA
C           DOUBLE PRECISION      RADEG
C           DOUBLE PRECISION      RADEG0
C           DOUBLE PRECISION      RAEPC
C           DOUBLE PRECISION      RAPM
C           DOUBLE PRECISION      RAYDIR ( 3 )
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      RSTAR
C           DOUBLE PRECISION      STEPSZ
C           DOUBLE PRECISION      T
C
C           INTEGER               CATNO
C           INTEGER               I
C           INTEGER               J
C           INTEGER               N
C
C     C
C     C     Load kernels.
C     C
C           CALL FURNSH ( META )
C
C     C
C     C     Initialize windows.
C     C
C           CALL SSIZED ( MAXWIN, CNFINE )
C           CALL SSIZED ( MAXWIN, RESULT )
C
C     C
C     C     Insert search time interval bounds into the
C     C     confinement window.
C     C
C           CALL STR2ET ( '2004 JUN 11 06:30:00 TDB', ET0 )
C           CALL STR2ET ( '2004 JUN 11 12:00:00 TDB', ET1 )
C
C           CALL WNINSD ( ET0, ET1, CNFINE )
C
C     C
C     C     Initialize inputs for the search.
C     C
C           INST   = 'CASSINI_ISS_WAC'
C
C     C
C     C     Create a unit direction vector pointing from
C     c     observer to star. We'll assume the direction
C     C     is constant during the confinement window, and
C     C     we'll use et0 as the epoch at which to compute the
C     C     direction from the spacecraft to the star.
C     C
C     C     The data below are for the star with catalog
C     C     number 6000 in the Hipparcos catalog. Angular
C     C     units are degrees; epochs have units of Julian
C     C     years and have a reference epoch of J1950.
C     C     The reference frame is J2000.
C     C
C           CATNO  = 6000
C
C           PLXDEG = 0.000001056D0
C
C           RADEG0 = 19.290789927D0
C           RAPM   = -0.000000720D0
C           RAEPC  = 41.2000D0
C
C           DECDG0 =  2.015271007D0
C           DECPM  =  0.000001814D0
C           DECEPC = 41.1300D0
C
C           RFRAME = 'J2000'
C
C     C
C     C     Correct the star's direction for proper motion.
C     C
C     C     The argument t represents et0 as Julian years
C     C     past J1950.
C     C
C           T      =      ET0/JYEAR()
C          .         +  ( J2000()- J1950() ) / 365.25D0
C
C           DTRA   = T - RAEPC
C           DTDEC  = T - DECEPC
C
C           RADEG  = RADEG0  +  DTRA  * RAPM
C           DECDEG = DECDG0  +  DTDEC * DECPM
C
C           RA     = RADEG  * RPD()
C           DEC    = DECDEG * RPD()
C
C           CALL RADREC ( 1.D0, RA, DEC, PSTAR )
C
C     C
C     C     Correct star position for parallax applicable at
C     C     the Cassini orbiter's position. (The parallax effect
C     C     is negligible in this case; we're simply demonstrating
C     C     the computation.)
C     C
C           PARLAX = PLXDEG * RPD()
C           RSTAR  = AU / TAN(PARLAX)
C
C     C
C     C     Scale the star's direction vector by its distance from
C     C     the solar system barycenter. Subtract off the position
C     C     of the spacecraft relative to the solar system barycenter;
C     C     the result is the ray's direction vector.
C     C
C           CALL VSCLIP ( RSTAR, PSTAR )
C
C           CALL SPKPOS ( 'CASSINI', ET0, 'J2000',  'NONE',
C          .              'SOLAR SYSTEM BARYCENTER', POS,  LT )
C
C           CALL VSUB   ( PSTAR, POS, RAYDIR )
C
C     C
C     C     Correct the star direction for stellar aberration when
C     C     we conduct the search.
C     C
C           ABCORR = 'S'
C           OBSRVR = 'CASSINI'
C           STEPSZ = 10.D0
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Instrument:              '//INST
C           WRITE (*,*) 'Star''s catalog number:  ', CATNO
C           WRITE (*,*) ' '
C
C     C
C     C     Perform the search.
C     C
C           CALL GFRFOV ( INST,   RAYDIR, RFRAME, ABCORR,
C          .              OBSRVR, STEPSZ, CNFINE, RESULT )
C
C           N = WNCARD( RESULT )
C
C           IF ( N .EQ. 0 ) THEN
C
C              WRITE (*,*) 'No FOV intersection found.'
C
C           ELSE
C
C              WRITE (*,*) 
C          .   ' Visibility start time              Stop time'
C
C              DO I = 1, N
C
C                 CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
C
C                 DO J = 1, 2
C                    CALL TIMOUT ( ENDPT(J), TIMFMT, TIMSTR(J) )
C                 END DO
C
C                 LINE( :3) = ' '
C                 LINE(2: ) = TIMSTR(1)
C                 LINE(37:) = TIMSTR(2)
C
C                 WRITE (*,*) LINE
C
C              END DO
C
C           END IF
C
C           WRITE (*,*) ' '
C           END
C
C
C        When this program was executed on a PC/Linux/g77 platform, the
C        output was:
C
C
C  Instrument:              CASSINI_ISS_WAC
C  Star's catalog number:   6000
C
C   Visibility start time              Stop time
C   2004-JUN-11 06:30:00.000000 (TDB)  2004-JUN-11 12:00:00.000000 (TDB)
C
C
C     The star is visible throughout the confinement window.
C
C
C$ Restrictions
C
C     The kernel files to be used by GFRFOV must be loaded (normally via
C     the SPICELIB routine FURNSH) before GFRFOV is called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman  (JPL)
C     L.S. Elson    (JPL)
C     E.D. Wright   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0  28-FEB-2012 (EDW)
C
C        Implemented use of ZZHOLDD to allow user to alter convergence
C        tolerance.
C
C        Removed the STEP > 0 error check. The GFSSTP call includes
C        the check.
C
C-    SPICELIB Version 1.0.0  15-APR-2009 (NJB) (LSE) (EDW)
C
C-&

C$ Index_Entries
C
C     GF ray in instrument FOV search
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
      INTEGER               SIZED

      LOGICAL               RETURN

C
C     External routines
C
C
C     Interrupt handler:
C
      LOGICAL               GFBAIL
      EXTERNAL              GFBAIL

C
C     Routines to set step size, refine transition times
C     and report work:
C
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFSTEP

C
C     Local parameters
C
      
C
C     Geometric quantity  bail switch:
C
      LOGICAL               BAIL
      PARAMETER           ( BAIL   = .FALSE. )

C
C     Progress report switch:
C
      LOGICAL               RPT
      PARAMETER           ( RPT    = .FALSE. )
      
C
C     Local variables
C
      DOUBLE PRECISION      TOL
      LOGICAL               OK 

C  
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'GFRFOV' ) 

C
C     Note to maintenance programmer: input exception checks
C     are delegated to GFFOVE. If the implementation of that
C     routine changes, or if this routine is modified to call
C     a different routine in place of GFFOVE, then the error 
C     handling performed by GFFOVE will have to be performed
C     here or in a routine called by this routine.
C
C     Check the result window's size.
C
      IF ( SIZED(RESULT) .LT. 2 ) THEN

         CALL SETMSG ( 'Result window size must be at least 2 '
     .   //            'but was #.'                            )
         CALL ERRINT ( '#',  SIZED(RESULT)                     )
         CALL SIGERR ( 'SPICE(WINDOWTOOSMALL)'                 )
         CALL CHKOUT ( 'GFRFOV'                                )
         RETURN

      END IF

C
C     Set the step size.
C
      CALL GFSSTP (STEP) 

C
C     Retrieve the convergence tolerance, if set.
C
      CALL ZZHOLDD ( ZZGET, GF_TOL, OK, TOL )

C
C     Use the default value CNVTOL if no stored tolerance value.
C
      IF ( .NOT. OK ) THEN

         TOL = CNVTOL

      END IF

C
C     Look for solutions.
C
      CALL GFFOVE ( INST,   RYSHAP, RAYDIR, ' ',    RFRAME, ABCORR,
     .              OBSRVR, TOL,    GFSTEP, GFREFN, RPT,    GFREPI,
     .              GFREPU, GFREPF, BAIL,   GFBAIL, CNFINE, RESULT )

      CALL CHKOUT ( 'GFRFOV' )
      RETURN
      END
