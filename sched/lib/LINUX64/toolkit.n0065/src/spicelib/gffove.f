C$Procedure      GFFOVE ( GF, is target in FOV? )
 
      SUBROUTINE GFFOVE ( INST,    TSHAPE,  RAYDIR,     
     .                    TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL,    
     .                    UDSTEP,  UDREFN,  RPT,     UDREPI,  UDREPU,   
     .                    UDREPF,  BAIL,    UDBAIL,  CNFINE,  RESULT )
 
C$ Abstract
C
C     Determine time intervals when a specified target body or ray
C     intersects the space bounded by the field-of-view (FOV) of a
C     specified instrument. Report progress and handle interrupts if so
C     commanded.
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
      
      INCLUDE 'gf.inc'
      
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         INST
      CHARACTER*(*)         TSHAPE
      DOUBLE PRECISION      RAYDIR ( 3 )
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         TFRAME
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      TOL
      EXTERNAL              UDSTEP
      EXTERNAL              UDREFN
      LOGICAL               RPT
      EXTERNAL              UDREPI
      EXTERNAL              UDREPU
      EXTERNAL              UDREPF
      LOGICAL               BAIL
      LOGICAL               UDBAIL
      EXTERNAL              UDBAIL
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      DOUBLE PRECISION      RESULT ( LBCELL : * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LBCELL     P   SPICE Cell lower bound.
C     MAXVRT     P   Maximum number of FOV boundary vertices.
C     INST       I   Name of the instrument.
C     TSHAPE     I   Type of shape model used for target body.
C     RAYDIR     I   Ray's direction vector.
C     TARGET     I   Name of the target body.
C     TFRAME     I   Body-fixed, body-centered frame for target body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     TOL        I   Convergence tolerance in seconds.
C     UDSTEP     I   Name of routine that returns a time step.
C     UDREFN     I   Name of the routine that computes a refined time.
C     RPT        I   Progress report flag.
C     UDREPI     I   Function that initializes progress reporting.
C     UDREPU     I   Function that updates the progress report.
C     UDREPF     I   Function that finalizes progress reporting.
C     BAIL       I   Logical indicating program interrupt monitoring.
C     UDBAIL     I   Name of a routine that signals a program interrupt.
C     CNFINE     I   SPICE window to which the search is restricted.
C     RESULT     O   SPICE window containing results.
C
C$ Detailed_Input
C
C     INST       indicates the name of an instrument, such as a
C                spacecraft-mounted framing camera, the field of view
C                (FOV) of which is to be used for a target intersection
C                search: times when the specified target intersects the
C                region of space corresponding to the FOV are sought.
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
C                                   represented by the ray emanating
C                                   from the observer's location and
C                                   having direction vector RAYDIR. The
C                                   target is considered to be visible
C                                   if and only if the ray is contained
C                                   within the space bounded by the
C                                   instrument FOV.
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
C                target is NOT modeled as ray, as indicated by the
C                input argument TSHAPE.
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
C                observation (the case where radiation is received by
C                observer at ET) are:
C
C                   'NONE'       No correction.
C                   'LT'         Light time only
C                   'LT+S'       Light time and stellar aberration.
C                   'CN'         Converged Newtonian (CN) light time.
C                   'CN+S'       CN light time and stellar aberration.
C
C                Supported aberration correction options for
C                transmission (the case where radiation is emitted from
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
C
C     TOL        is a tolerance value used to determine convergence of
C                root-finding operations. TOL is measured in TDB
C                seconds and must be greater than zero.
C
C
C     UDSTEP     is an externally specified routine that computes a
C                time step used to find transitions of the state being
C                considered. A state transition occurs where the state
C                changes from being "visible" to being "not visible" or
C                vice versa.
C           
C                This routine relies on UDSTEP returning step sizes
C                small enough so that state transitions within the
C                confinement window are not overlooked.
C
C                The calling sequence for UDSTEP is:
C
C                   CALL UDSTEP ( ET, STEP )
C
C                where:
C
C                   ET      is the input start time from which the
C                           algorithm is to search forward for a state
C                           transition. ET is expressed as seconds past
C                           J2000 TDB. ET is a DOUBLE PRECISION number.
C
C                   STEP    is the output step size.  STEP indicates
C                           how far to advance ET so that ET and
C                           ET+STEP may bracket a state transition and
C                           definitely do not bracket more than one
C                           state transition. STEP is a DOUBLE
C                           PRECISION number. Units are TDB seconds.
C
C                If a constant step size is desired, the routine GFSTEP
C                may be used. If GFSTEP is used, the step size must be
C                set by calling GFSSTP prior to calling this routine.
C
C
C     UDREFN     is the name of the externally specified routine that
C                refines the times that bracket a transition point. In
C                other words, once a pair of times, T1 and T2, that
C                bracket a state transition have been found, UDREFN
C                computes an intermediate time T such that either 
C                [T1, T] or [T, T2] contains the time of the state
C                transition. The calling sequence for UDREFN is:
C
C                   CALL UDREFN ( T1, T2, S1, S2, T )
C
C                where the inputs are:
C
C                   T1    is a time when the visibility state is S1. T1
C                         is expressed as seconds past J2000 TDB.
C
C                   T2    is a time when the visibility state is S2. T2
C                         is expressed as seconds past J2000 TDB. and
C                         is assumed to be larger than T1.
C
C                   S1    is the visibility state at time T1. S1 is a
C                         LOGICAL value.
C
C                   S2    is the visibility state at time T2. S2 is a
C                         LOGICAL value.
C
C                The output is:
C
C                   T     is the next time to check for a state
C                         transition. T is expressed as seconds past
C                         J2000 TDB and is between T1 and T2.
C
C                If a simple bisection method is desired, the routine
C                GFREFN may be used.  
C
C
C     RPT        is a logical variable that controls whether
C                progress reporting is enabled. When RPT is .TRUE.,
C                progress reporting is enabled and the routines
C                UDREPI, UDREPU, and UDPREF (see descriptions below)
C                are used to report progress. 
C 
C
C     UDREPI     is a user-defined subroutine that initializes a
C                progress report. When progress reporting is 
C                enabled, UDREPI is called at the start
C                of a search. The calling sequence of UDREPI is
C 
C                   UDREPI ( CNFINE, SRCPRE, SRCSUF )
C
C                   DOUBLE PRECISION    CNFINE ( LBCELL : * )
C                   CHARACTER*(*)       SRCPRE
C                   CHARACTER*(*)       SRCSUF
C
C                where
C
C                   CNFINE 
C
C                is the confinement window and
C 
C                   SRCPRE
C                   SRCSUF
C
C                are prefix and suffix strings used in the progress
C                report: these strings are intended to bracket a
C                representation of the fraction of work done.  For
C                example, when the SPICELIB progress reporting functions
C                are used, if SRCPRE and SRCSUF are, respectively,
C
C                   'FOV search'
C                   'done.'
C
C                the progress report display at the end of the
C                search will be:
C
C                   FOV search 100.00% done.
C
C                The SPICELIB routine GFREPI may be used as the 
C                actual argument corresponding to UDREPI. If so,
C                the SPICELIB routines GFREPU and GFREPF must be
C                the actual arguments corresponding to UDREPU and
C                UDREPF.
C
C
C     UDREPU     is a user-defined subroutine that updates the 
C                progress report for a search.  The calling sequence 
C                of UDREPU is
C 
C                   UDREPU ( IVBEG, IVEND, ET )
C
C                   DOUBLE PRECISION      IVBEG
C                   DOUBLE PRECISION      IVEND
C                   DOUBLE PRECISION      ET  
C
C                Here IVBEG, IVEND are the bounds of an interval that
C                is contained in some interval belonging to the
C                confinement window. The confinement window is
C                associated with some root finding activity. It is used
C                to determine how much total time is being searched in
C                order to find the events of interest.
C
C                ET is an epoch belonging to the interval [IVBEG,
C                IVEND].
C
C                In order for a meaningful progress report to be
C                displayed, IVBEG and IVEND must satisfy the following
C                constraints:
C
C                 - IVBEG must be less than or equal to IVEND.
C
C                 - The interval [ IVBEG, IVEND ] must be contained in
C                   some interval of the confinement window. It can be
C                   a proper subset of the containing interval; that
C                   is, it can be smaller than the interval of the
C                   confinement window that contains it.
C
C                 - Over a search, the sum of the differences
C
C                      IVEND - IVBEG
C
C                   for all calls to this routine made during the search
C                   must equal the measure of the confinement window.
C
C                The SPICELIB routine GFREPU may be used as the 
C                actual argument corresponding to UDREPU. If so,
C                the SPICELIB routines GFREPI and GFREPF must be
C                the actual arguments corresponding to UDREPI and
C                UDREPF.
C
C
C     UDREPF     is a user-defined subroutine that finalizes a
C                progress report. UDREPF has no arguments.
C
C                The SPICELIB routine GFREPF may be used as the 
C                actual argument corresponding to UDREPF. If so,
C                the SPICELIB routines GFREPI and GFREPU must be
C                the actual arguments corresponding to UDREPI and
C                UDREPU.
C
C
C     BAIL       is a logical variable indicating whether or not
C                interrupt handling is enabled. When BAIL is
C                set to .TRUE., the input function UDBAIL (see
C                description below) is used to determine whether
C                an interrupt has been issued.
C
C
C     UDBAIL     is the name of a user defined logical function that
C                indicates whether an interrupt signal has been 
C                issued (for example, from the keyboard).  UDBAIL
C                has no arguments and returns a LOGICAL value.
C                The return value is .TRUE. if an interrupt has
C                been issued; otherwise the value is .FALSE.
C
C                GFFOVE uses UDBAIL only when BAIL (see above) is set
C                to .TRUE., indicating that interrupt handling is
C                enabled. When interrupt handling is enabled, GFFOVE
C                and routines in its call tree will call UDBAIL to
C                determine whether to terminate processing and return
C                immediately.
C                
C                If interrupt handing is not enabled, a logical
C                function must still be passed to GFFOVE as
C                an input argument. The SPICE function 
C
C                   GFBAIL
C        
C                may be used for this purpose.
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
C     RESULT     is a SPICE window representing the set of time
C                intervals, within the confinement period, when image
C                of the target body is partially or completely within
C                the specified instrument field of view.
C
C                The endpoints of the time intervals comprising RESULT
C                are interpreted as seconds past J2000 TDB.
C
C                If RESULT is non-empty on input, its contents
C                will be discarded before GFFOVE conducts its
C                search.
C
C$ Parameters
C
C     LBCELL     is the lower bound for SPICE cell arrays.
C
C
C     MAXVRT     is the maximum number of vertices that may be used
C                to define the boundary of the specified instrument's
C                field of view.
C
C     See INCLUDE file gf.inc for declarations and descriptions of 
C     parameters used throughout the GF system.
C
C$ Exceptions
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
C     3)  If the name of either the target or observer cannot be
C         translated to a NAIF ID code, the error will be diagnosed by
C         a routine in the call tree of this routine.
C         
C     4)  If the specified aberration correction is not a supported
C         value for the target type (ephemeris object or ray), the
C         error will be diagnosed by a routine in the call tree of this
C         routine.
C
C     5)  If the radii of a target body modeled as an ellipsoid cannot
C         be determined by searching the kernel pool for a kernel
C         variable having a name of the form
C
C            'BODYnnn_RADII' 
C
C         where nnn represents the NAIF integer code associated with
C         the body, the error will be diagnosed by a routine in the
C         call tree of this routine.
C
C     6)  If the target body coincides with the observer body OBSRVR,
C         the error will be diagnosed by a routine in the call tree of
C         this routine.
C         
C     7)  If the body model specifier TSHAPE is not recognized, the
C         error will be diagnosed by a routine in the call tree of this
C         routine.
C
C     8)  If a target body-fixed reference frame associated with a 
C         non-point target is not recognized, the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     9)  If a target body-fixed reference frame is not centered at
C         the corresponding target body,  the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     10) If the instrument name INST does not have corresponding NAIF
C         ID code, the error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     11) If the FOV parameters of the instrument are not present in
C         the kernel pool, the error will be be diagnosed by routines
C         in the call tree of this routine.
C
C     12) If the FOV boundary has more than MAXVRT vertices, the error
C         will be be diagnosed by routines in the call tree of this
C         routine.
C
C     13) If the instrument FOV is polygonal, and this routine cannot
C         find a ray R emanating from the FOV vertex such that maximum
C         angular separation of R and any FOV boundary vector is within
C         the limit (pi/2)-SPICE_GF_MARGIN radians, the error will be
C         diagnosed by a routine in the call tree of this routine. If
C         the FOV is any other shape, the same error check will be
C         applied with the instrument boresight vector serving the role
C         of R.
C
C     14) If the loaded kernels provide insufficient data to compute a
C         requested state vector, the error will be diagnosed by a
C         routine in the call tree of this routine.
C
C     15) If an error occurs while reading an SPK or other kernel file,
C         the error will be diagnosed by a routine in the call tree 
C         of this routine.
C
C     16) If the output SPICE window RESULT has insufficient capacity
C         to contain the number of intervals on which the specified
C         visibility condition is met, the error will be diagnosed
C         by a routine in the call tree of this routine. If the result
C         window has size less than 2, the error SPICE(WINDOWTOOSMALL)
C         will be signaled by this routine.
C
C     17) If the convergence tolerance size is non-positive, the error
C         SPICE(INVALIDTOLERANCE) will be signaled.
C
C     18) If the step size is non-positive, the error 
C         SPICE(INVALIDSTEP) will be signaled. 
C
C     19) If the ray's direction vector is zero, the error 
C         SPICE(ZEROVECTOR) is signaled. 
C
C
C$ Files
C
C     Appropriate SPICE ernels must be loaded by the calling program
C     before this routine is called.
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
C        - Since the input ray direction may be expressed in any 
C          frame, FKs, CKs, SCLK kernels, PCKs, and SPKs may be
C          required to map the direction to the J2000 frame.

C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when a specified ray or any portion
C     of a specified target body appears within the field of view of a
C     specified instrument. We'll use the term "visibility event" to
C     designate such an appearance. The set of time intervals resulting
C     from the search is returned as a SPICE window.
C
C     This routine provides the SPICE GF system's most flexible
C     interface for searching for FOV intersection events.
C
C     Applications that require do not require support for progress
C     reporting, interrupt handling, non-default step or refinement
C     functions, or non-default convergence tolerance normally should
C     call either GFTFOV or GFRFOV rather than this routine.
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
C     transitions: times are sought when the state of the target ray or
C     body changes from "not visible" to "visible" or vice versa.
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
C     Having some knowledge of the relative geometry of the target and
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
C     The times of state transitions are called ``roots.''
C
C     Once a root has been bracketed, a refinement process is used to
C     narrow down the time interval within which the root must lie.
C     This refinement process terminates when the location of the root
C     has been determined to within an error margin called the
C     "convergence tolerance." 
C
C     The convergence tolerance used by high-level GF routines that
C     call this routine is set via the parameter CNVTOL, which is
C     declared in the INCLUDE file gf.inc. The value of CNVTOL is set
C     to a "tight" value so that the tolerance doesn't become the
C     limiting factor in the accuracy of solutions found by this
C     routine. In general the accuracy of input data will be the
C     limiting factor.
C
C     Setting the input tolerance TOL tighter than CNVTOL is unlikely
C     to be useful, since the results are unlikely to be more accurate.
C     Making the tolerance looser will speed up searches somewhat,
C     since a few convergence steps will be omitted. However, in most
C     cases, the step size is likely to have a much greater effect on
C     processing time than would the convergence tolerance.
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
C     1) Search for times when Saturn's satellite Phoebe is within
C        the FOV of the Cassini narrow angle camera (CASSINI_ISS_NAC).
C        To simplify the problem, restrict the search to a short time
C        period where continuous Cassini bus attitude data are
C        available.
C
C        Use default SPICELIB progress reporting.
C
C        Use a step size of 1 second to reduce chances of missing
C        short visibility events and to make the search slow enough
C        so the progress report's updates are visible.
C        
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
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
C           INTEGER               WNCARD
C
C     C
C     C     SPICELIB default functions for
C     C
C     C        - Interrupt handling (no-op function):   GFBAIL
C     C        - Search refinement:                     GFREFN
C     C        - Progress report termination:           GFREPF
C     C        - Progress report initialization:        GFREPI
C     C        - Progress report update:                GFREPU
C     C        - Search step size "get" function:       GFSTEP
C     C
C           EXTERNAL              GFBAIL
C           EXTERNAL              GFREFN
C           EXTERNAL              GFREPF
C           EXTERNAL              GFREPI
C           EXTERNAL              GFREPU
C           EXTERNAL              GFSTEP
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
C           INTEGER               SHPLEN
C           PARAMETER           ( SHPLEN = 25 )
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
C           CHARACTER*(BDNMLN)    TARGET
C           CHARACTER*(FRNMLN)    TFRAME
C           CHARACTER*(TIMLEN)    TIMSTR ( 2 )
C           CHARACTER*(SHPLEN)    TSHAPE
C
C           DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      ENDPT  ( 2 )
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      ET1
C           DOUBLE PRECISION      RAYDIR ( 3 )
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      TOL
C
C           INTEGER               I
C           INTEGER               J
C           INTEGER               N
C
C           LOGICAL               BAIL
C           LOGICAL               RPT
C
C     C
C     C     Since we're treating the target as an ephemeris object,
C     C     the ray direction is unused. We simply initialize the
C     C     direction vector to avoid portability problems.
C     C
C           DATA                  RAYDIR / 3*0.D0 /
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
C           INST   = 'CASSINI_ISS_NAC'
C           TARGET = 'PHOEBE'
C           TSHAPE = 'ELLIPSOID'
C           TFRAME = 'IAU_PHOEBE'
C           ABCORR = 'LT+S'
C           OBSRVR = 'CASSINI'
C
C     C
C     C     Use a particularly short step size to make the progress
C     C     report's updates visible.
C     C
C     C     Pass the step size (1 second) to the GF default step size
C     C     put/get system.
C     C
C           CALL GFSSTP ( 1.D0 )
C
C     C
C     C     Set the convergence tolerance to 1 microsecond.
C     C
C           TOL    = 1.D-6
C
C     C
C     C     Use progress reporting; turn off interrupt handling.
C     C
C           RPT  = .TRUE.
C           BAIL = .FALSE.
C
C           WRITE (*,*) ' '
C           WRITE (*, '(A)' ) 'Instrument: '//INST
C           WRITE (*, '(A)' ) 'Target:     '//TARGET
C
C     C
C     C     Perform the search.
C     C
C           CALL GFFOVE ( INST,    TSHAPE,  RAYDIR,
C          .              TARGET,  TFRAME,  ABCORR,  OBSRVR,  TOL,
C          .              GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU,
C          .              GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )
C
C           N = WNCARD( RESULT )
C
C           IF ( N .EQ. 0 ) THEN
C
C              WRITE (*, '(A)' ) 'No FOV intersection found.'
C
C           ELSE
C
C           WRITE (*, '(A)' )
C          .          '  Visibility start time              Stop time'
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
C        final output (the progress report is overwritten when it is
C        updated, so only the final update is captured here) was:
C
C
C  Instrument: CASSINI_ISS_NAC
C  Target:     PHOEBE
C
C
C Target visibility search 100.00% done.
C
C   Visibility start time              Stop time
C   2004-JUN-11 07:35:49.958590 (TDB)  2004-JUN-11 08:48:27.485965 (TDB)
C   2004-JUN-11 09:03:19.767799 (TDB)  2004-JUN-11 09:35:27.634790 (TDB)
C   2004-JUN-11 09:50:19.585474 (TDB)  2004-JUN-11 10:22:27.854254 (TDB)
C   2004-JUN-11 10:37:19.332696 (TDB)  2004-JUN-11 11:09:28.116016 (TDB)
C   2004-JUN-11 11:24:19.049484 (TDB)  2004-JUN-11 11:56:28.380304 (TDB)
C 
C
C     2)  A variation of example (1): search the same confinement
C         window for times when a selected background star is visible.
C         We use the FOV of the Cassini ISS wide angle camera
C         (CASSINI_ISS_WAC) to enhance the probability of viewing the
C         star.
C
C         The star we'll use has catalog number 6000 in the Hipparcos
C         Catalog. The star's J2000 right ascension and declination,
C         proper motion, and parallax are taken from that catalog.
C
C         Use the meta-kernel from the first example.
C
C        Example code begins here.
C
C
C           PROGRAM EX2
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
C     C     SPICELIB default functions for
C     C
C     C        - Interrupt handling (no-op function):   GFBAIL
C     C        - Search refinement:                     GFREFN
C     C        - Progress report termination:           GFREPF
C     C        - Progress report initialization:        GFREPI
C     C        - Progress report update:                GFREPU
C     C        - Search step size "get" function:       GFSTEP
C     C
C           EXTERNAL              GFBAIL
C           EXTERNAL              GFREFN
C           EXTERNAL              GFREPF
C           EXTERNAL              GFREPI
C           EXTERNAL              GFREPU
C           EXTERNAL              GFSTEP
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
C           INTEGER               SHPLEN
C           PARAMETER           ( SHPLEN = 25 )
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
C           CHARACTER*(BDNMLN)    TARGET
C           CHARACTER*(TIMLEN)    TIMSTR ( 2 )
C           CHARACTER*(SHPLEN)    TSHAPE
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
C           DOUBLE PRECISION      T
C           DOUBLE PRECISION      TOL
C
C           INTEGER               CATNO
C           INTEGER               I
C           INTEGER               J
C           INTEGER               N
C
C           LOGICAL               BAIL
C           LOGICAL               RPT
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
C           TARGET = ' '
C           TSHAPE = 'RAY'
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
C
C     C
C     C     Use a particularly short step size to make the progress
C     C     report's updates visible.
C     C
C     C     Pass the step size (1 second) to the GF default step size
C     C     put/get system.
C     C
C           CALL GFSSTP ( 1.D0 )
C
C     C
C     C     Set the convergence tolerance to 1 microsecond.
C     C
C           TOL = 1.D-6
C
C     C
C     C     Use progress reporting; turn off interrupt handling.
C     C
C           RPT  = .TRUE.
C           BAIL = .FALSE.
C
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Instrument:              '//INST
C           WRITE (*,*) 'Star''s catalog number:  ', CATNO
C
C     C
C     C     Perform the search.
C     C
C           CALL GFFOVE ( INST,    TSHAPE,  RAYDIR,
C          .              TARGET,  RFRAME,  ABCORR,  OBSRVR,  TOL,
C          .              GFSTEP,  GFREFN,  RPT,     GFREPI,  GFREPU,
C          .              GFREPF,  BAIL,    GFBAIL,  CNFINE,  RESULT )
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
C Target visibility search 100.00% done.
C
C   Visibility start time              Stop time
C   2004-JUN-11 06:30:00.000000 (TDB)  2004-JUN-11 12:00:00.000000 (TDB)
C
C
C
C$ Restrictions
C
C     The kernel files to be used by GFFOVE must be loaded (normally via
C     the SPICELIB routine FURNSH) before GFFOVE is called.
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
C-    SPICELIB Version 1.0.0 15-APR-2009 (NJB) (LSE) (EDW)
C
C-&


C$ Index_Entries
C
C     GF mid-level target in instrument FOV search 
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
      INTEGER               WNCARD
 
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     External routines
C
      EXTERNAL              ZZGFFVST

C
C     Local parameters
C
C
C     STEP is a step size initializer for the unused, dummy step size
C     argument to ZZGFSOLV. The routine UDSTEP, which is passed to
C     ZZGFSOLV, will be used by that routine to obtain the step size.
C
      DOUBLE PRECISION      STEP
      PARAMETER           ( STEP   = 1.D0 )

C
C     CSTEP indicates whether a constant step size, provided
C     via the input argument STEP, is to be used by ZZGFSOLV.
C
      LOGICAL               CSTEP
      PARAMETER           ( CSTEP  = .FALSE. )


C
C     Local variables
C
      DOUBLE PRECISION      FINISH      
      DOUBLE PRECISION      START
 
      INTEGER               COUNT
      INTEGER               I


C  
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'GFFOVE' ) 

C
C     Check the result window's size.
C
      IF ( SIZED(RESULT) .LT. 2 ) THEN

         CALL SETMSG ( 'Result window size must be at least 2 '
     .   //            'but was #.'                            )
         CALL ERRINT ( '#',  SIZED(RESULT)                     )
         CALL SIGERR ( 'SPICE(WINDOWTOOSMALL)'                 )
         CALL CHKOUT ( 'GFFOVE'                                )
         RETURN

      END IF

C
C     Empty the RESULT window.
C
      CALL SCARDD ( 0, RESULT )

C
C     Check the convergence tolerance.
C
      IF ( TOL .LE. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be positive but was #.' )
         CALL ERRDP  ( '#',  TOL                               )
         CALL SIGERR ( 'SPICE(INVALIDTOLERANCE)'               )
         CALL CHKOUT ( 'GFFOVE'                                )
         RETURN

      END IF

C
C     Note to maintenance programmer: most input exception checks are
C     delegated to ZZGFFVIN. If the implementation of that routine
C     changes, or if this routine is modified to call a different
C     routine in place of ZZGFFVIN, then the error handling performed
C     by ZZGFFVIN will have to be performed here or in a routine called
C     by this routine.
C

C 
C     Initialize the visibility calculation.
C  
      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,   
     .                TARGET, TFRAME, ABCORR, OBSRVR )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'GFFOVE' )
         RETURN
      END IF
 
C
C     Prepare the progress reporter if appropriate.
C
      IF ( RPT ) THEN
         CALL UDREPI ( CNFINE, 'Target visibility search ', 'done.' )
      END IF
 
C 
C     Cycle over the intervals in the confinement window.
C
      COUNT = WNCARD( CNFINE )
 
      DO I = 1, COUNT
C
C        Retrieve the bounds for the Ith interval of the confinement
C        window. Search this interval for visibility events. Union the
C        result with the contents of the RESULT window.
C
         CALL WNFETD ( CNFINE, I, START, FINISH  )

         CALL ZZGFSOLV ( ZZGFFVST,   UDSTEP,   UDREFN,   BAIL,
     .                   UDBAIL,     CSTEP,    STEP,     START,
     .                   FINISH,     TOL,      RPT,      UDREPU,
     .                   RESULT                                  )
       
         IF (  FAILED()  ) THEN
            CALL CHKOUT ( 'GFFOVE'  )
            RETURN
         END IF

         IF ( BAIL ) THEN
C
C           Interrupt handling is enabled.
C
            IF ( UDBAIL() ) THEN
C
C              An interrupt has been issued. Return now regardless of
C              whether the search has been completed.
C
               CALL CHKOUT ( 'GFFOVE' )
               RETURN

            END IF

         END IF
 
      END DO   
 
C
C     End the progress report.
C
      IF ( RPT ) THEN
         CALL UDREPF
      END IF


      CALL CHKOUT ( 'GFFOVE' )
      RETURN
      END
