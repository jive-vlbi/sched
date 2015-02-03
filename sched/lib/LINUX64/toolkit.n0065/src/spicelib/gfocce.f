C$Procedure      GFOCCE ( GF, occultation event )
 
      SUBROUTINE GFOCCE ( OCCTYP,  FRONT,   FSHAPE,  FFRAME,
     .                    BACK,    BSHAPE,  BFRAME,  ABCORR, 
     .                    OBSRVR,  TOL,     UDSTEP,  UDREFN,  
     .                    RPT,     UDREPI,  UDREPU,  UDREPF,  
     .                    BAIL,    UDBAIL,  CNFINE,  RESULT )
      
C$ Abstract
C
C     Determine time intervals when an observer sees one target
C     occulted by another. Report progress and handle interrupts
C     if so commanded.
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
C     GF
C     KERNEL
C     NAIF_IDS
C     SPK
C     TIME
C     WINDOWS  
C
C$ Keywords
C
C     EVENT
C     GEOMETRY
C     SEARCH
C     WINDOW
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         OCCTYP
      CHARACTER*(*)         FRONT
      CHARACTER*(*)         FSHAPE
      CHARACTER*(*)         FFRAME
      CHARACTER*(*)         BACK
      CHARACTER*(*)         BSHAPE
      CHARACTER*(*)         BFRAME
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
C     OCCTYP     I   Type of occultation.
C     FRONT      I   Name of body occulting the other.
C     FSHAPE     I   Type of shape model used for front body.
C     FFRAME     I   Body-fixed, body-centered frame for front body.
C     BACK       I   Name of body occulted by the other.
C     BSHAPE     I   Type of shape model used for back body.
C     BFRAME     I   Body-fixed, body-centered frame for back body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     TOL        I   Convergence tolerance in seconds.
C     UDSTEP     I   Name of the routine that returns a time step.
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
C
C     OCCTYP     indicates the type of occultation that is to be found.
C                Supported values and corresponding definitions are:
C
C                   'FULL'               denotes the full occultation
C                                        of the body designated by 
C                                        BACK by the body designated
C                                        by FRONT, as seen from
C                                        the location of the observer.
C                                        In other words, the occulted
C                                        body is completely invisible
C                                        as seen from the observer's
C                                        location.
C
C                   'ANNULAR'            denotes an annular
C                                        occultation: the body
C                                        designated by FRONT blocks
C                                        part of, but not the limb of,
C                                        the body designated by BACK,
C                                        as seen from the location of
C                                        the observer.
C
C                   'PARTIAL'            denotes an partial,
C                                        non-annular occultation: the
C                                        body designated by FRONT
C                                        blocks part, but not all, of
C                                        the limb of the body
C                                        designated by BACK, as seen
C                                        from the location of the
C                                        observer.
C
C                   'ANY'                denotes any of the above three
C                                        types of occultations:
C                                        'PARTIAL', 'ANNULAR', or
C                                        'FULL'.
C
C                                        'ANY' should be used to search
C                                        for times when the body 
C                                        designated by FRONT blocks
C                                        any part of the body designated
C                                        by BACK.
C
C                                        The option 'ANY' must be used
C                                        if either the front or back
C                                        target body is modeled as
C                                        a point.
C
C                Case and leading or trailing blanks are not
C                significant in the string OCCTYP.
C
C
C     FRONT      is the name of the target body that occults---that is,
C                passes in front of---the other. Optionally, you may
C                supply the integer NAIF ID code for the body as a
C                string. For example both 'MOON' and '301' are
C                legitimate strings that designate the Moon.
C
C                Case and leading or trailing blanks are not
C                significant in the string FRONT.
C
C
C     FSHAPE     is a string indicating the geometric model used
C                to represent the shape of the front body. The
C                supported options are:
C
C                   'ELLIPSOID'     Use a triaxial ellipsoid model,
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
C                   'POINT'         Treat the body as a single point.
C                                   When a point target is specified,
C                                   the occultation type must be
C                                   set to 'ANY'.
C                                   
C                At least one of the target bodies FRONT and BACK must
C                be modeled as an ellipsoid.
C
C                Case and leading or trailing blanks are not
C                significant in the string FSHAPE.
C
C
C     FFRAME     is the name of the body-fixed, body-centered reference
C                frame associated with the front target body. Examples
C                of such names are 'IAU_SATURN' (for Saturn) and
C                'ITRF93' (for the Earth).
C
C                If the front target body is modeled as a point, FFRAME
C                should be left blank.

C                Case and leading or trailing blanks are not
C                significant in the string FFRAME.
C
C
C     BACK       is the name of the target body that is occulted
C                by---that is, passes in back of---the other.
C                Optionally, you may supply the integer NAIF ID code
C                for the body as a string. For example both 'MOON' and
C                '301' are legitimate strings that designate the Moon.
C
C                Case and leading or trailing blanks are not
C                significant in the string BACK.
C
C
C     BSHAPE     is the shape specification for the body designated
C                by BACK. The supported options are those for
C                FSHAPE. See the description of FSHAPE above for
C                details.
C                
C
C     BFRAME     is the name of the body-fixed, body-centered reference
C                frame associated with the ``back'' target body.  See
C                the description of FFRAME above for details.
C                Examples of such names are 'IAU_SATURN' (for Saturn)
C                and 'ITRF93' (for the Earth).
C
C                If the back target body is modeled as a point, BFRAME
C                should be left blank.
C
C                Case and leading or trailing blanks bracketing a
C                non-blank frame name are not significant in the string
C                BFRAME.
C
C
C     ABCORR     indicates the aberration corrections to be applied to
C                the state of the target body to account for one-way
C                light time.  Stellar aberration corrections are
C                ignored if specified, since these corrections don't
C                improve the accuracy of the occultation determination.
C
C                See the header of the SPICE routine SPKEZR for a
C                detailed description of the aberration correction
C                options. For convenience, the options supported by
C                this routine are listed below:
C
C                   'NONE'     Apply no correction.   
C
C                   'LT'       "Reception" case:  correct for
C                              one-way light time using a Newtonian
C                              formulation.
C
C                   'CN'       "Reception" case:  converged
C                              Newtonian light time correction.
C
C                   'XLT'      "Transmission" case:  correct for
C                              one-way light time using a Newtonian
C                              formulation.
C
C                   'XCN'      "Transmission" case:  converged
C                              Newtonian light time correction.
C
C                Case and blanks are not significant in the string
C                ABCORR.
C
C
C     OBSRVR     is the name of the body from which the occultation is
C                observed. Optionally, you may supply the integer NAIF
C                ID code for the body as a string.
C                
C                Case and leading or trailing blanks are not
C                significant in the string OBSRVR.
C
C
C     TOL        is a tolerance value used to determine convergence of
C                root-finding operations. TOL is measured in TDB seconds
C                and must be greater than zero.
C
C
C     UDSTEP     is an externally specified routine that computes a
C                time step used to find transitions of the state being
C                considered. A state transition occurs where the state
C                changes from being "in occultation" to being "not in
C                occultation" or vice versa.
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
C     RPT        is a logical variable which controls whether
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
C                example, when the CSPICE progress reporting functions
C                are used, if srcpre and srcsuf are, respectively,
C
C                   "Occultation search"
C                   "done."
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
C                ET is an epoch belonging to the interval 
C                [IVBEG, IVEND].
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
C                GFOCCE uses UDBAIL only when BAIL (see above) is set
C                to .TRUE., indicating that interrupt handling is
C                enabled. When interrupt handling is enabled, GFOCCE
C                and routines in its call tree will call UDBAIL to
C                determine whether to terminate processing and return
C                immediately.
C                
C                If interrupt handing is not enabled, a logical
C                function must still be passed to GFOCCE as
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
C                are interpreted as seconds past J2000 TDB..
C                
C                See the Examples section below for a code example 
C                that shows how to create a confinement window.
C                
C                CNFINE must be initialized by the caller via the
C                SPICELIB routine SSIZED.
C
C       
C$ Detailed_Output
C
C     RESULT     is a SPICE window representing the set of time
C                intervals, within the confinement period, when the
C                specified occultation occurs. 
C
C                The endpoints of the time intervals comprising RESULT
C                are interpreted as seconds past J2000 TDB.
C
C                If RESULT is non-empty on input, its contents
C                will be discarded before GFOCCE conducts its
C                search.
C
C$ Parameters
C
C     LBCELL     is the SPICELIB cell lower bound.
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
C         SPICE(INVALIDSTEP) will be signaled.
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
C     3)  If name of either target or the observer cannot be translated
C         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is
C         signaled.
C         
C     4)  If the radii of a target body modeled as an ellipsoid cannot
C         be determined by searching the kernel pool for a kernel
C         variable having a name of the form
C
C            'BODYnnn_RADII' 
C
C         where nnn represents the NAIF integer code associated with
C         the body, the error will be diagnosed by a routine in the
C         call tree of this routine.
C
C     5)  If either of the target bodies FRONT or BACK coincides with
C         the observer body OBSRVR, the error SPICE(BODIESNOTDISTINCT)
C         will be signaled.
C
C     6)  If the body designated by FRONT coincides with that
C         designated by BACK, the error SPICE(BODIESNOTDISTINCT) will
C         be signaled.
C         
C     7)  If either of the body model specifiers FSHAPE or BSHAPE
C         is not recognized, the error SPICE(INVALIDSHAPE) will be
C         signaled.
C
C     8)  If both of the body model specifiers FSHAPE and BSHAPE
C         specify point targets, the error SPICE(INVALIDSHAPECOMBO)
C         will be signaled.
C
C     9)  If a target body-fixed reference frame associated with a 
C         non-point target is not recognized, the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     10) If a target body-fixed reference frame is not centered at
C         the corresponding target body,  the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     11) If the loaded kernels provide insufficient data to 
C         compute the requested state vector, the deficiency will
C         be diagnosed by a routine in the call tree of this routine.
C
C     12) If an error occurs while reading an SPK or other kernel file,
C         the error will be diagnosed by a routine in the call tree 
C         of this routine.
C
C     13) If a point target is specified and the occultation
C         type is set to a valid value other than 'ANY', the
C         error SPICE(BADTYPESHAPECOMBO) will be signaled.
C
C     14) If the output SPICE window RESULT has insufficient capacity
C         to contain the number of intervals on which the specified
C         visibility condition is met, the error will be diagnosed
C         by a routine in the call tree of this routine. If the result
C         window has size less than 2, the error SPICE(WINDOWTOOSMALL)
C         will be signaled by this routine.
C 
C     15) Invalid occultation types will be diagnosed by a routine in
C         the call tree of this routine.
C
C     16) Invalid aberration correction specifications will be
C         diagnosed by a routine in the call tree of this routine.
C
C     17) If the convergence tolerance size is non-positive, the error
C         SPICE(INVALIDTOLERANCE) will be signaled.
C
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: the calling application must load ephemeris data
C          for the target, source and observer that cover the time
C          period specified by the window CNFINE. If aberration
C          corrections are used, the states of target and observer
C          relative to the solar system barycenter must be calculable
C          from the available ephemeris data. Typically ephemeris data
C          are made available by loading one or more SPK files via
C          FURNSH.
C
C        - PCK data: bodies modeled as triaxial ellipsoids must have
C          semi-axis lengths provided by variables in the kernel pool.
C          Typically these data are made available by loading a text
C          PCK file via FURNSH.
C
C        - FK data: if either of the reference frames designated by
C          BFRAME or FFRAME are not built in to the SPICE system,
C          one or more FKs specifying these frames must be loaded. 
C
C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C     This routine provides the SPICE GF system's most flexible
C     interface for searching for occultation events.
C   
C     Applications that require do not require support for progress
C     reporting, interrupt handling, non-default step or refinement
C     functions, or non-default convergence tolerance normally should
C     call GFOCLT rather than this routine.
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when a specified type of
C     occultation occurs. The resulting set of intervals is returned as
C     a SPICE window.
C
C     Below we discuss in greater detail aspects of this routine's
C     solution process that are relevant to correct and efficient
C     use of this routine in user applications.
C     
C
C     The Search Process
C     ==================
C
C     The search for occultations is treated as a search for state
C     transitions: times are sought when the state of the BACK body
C     changes from "not occulted" to "occulted" or vice versa.
C
C     Step Size
C     =========
C
C     Each interval of the confinement window is searched as follows:
C     first, the input step size is used to determine the time
C     separation at which the occultation state will be sampled.
C     Starting at the left endpoint of an interval, samples will be
C     taken at each step. If a state change is detected, a root has
C     been bracketed; at that point, the "root"--the time at which the
C     state change occurs---is found by a refinement process, for
C     example, via binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the occultation state is constant:
C     the step size should be shorter than the shortest occultation
C     duration and the shortest period between occultations, within
C     the confinement window.
C
C     Having some knowledge of the relative geometry of the targets and
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
C
C$ Examples
C
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     1) Conduct a search using the default GF progress reporting 
C        capability.
C
C        The program will use console I/O to display a simple
C        ASCII-based progress report.
C
C        The program will find occultations of the Sun by the Moon as
C        seen from the center of the Earth over the month December,
C        2001.
C
C        We use light time corrections to model apparent positions of
C        Sun and Moon. Stellar aberration corrections are not specified
C        because they don't affect occultation computations.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File name: standard.tm
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
C                                  'pck00008.tpc',
C                                  'naif0009.tls'  )
C
C           \begintext
C
C
C       Example code begins here.
C
C
C              PROGRAM EX1
C
C              IMPLICIT NONE
C
C              EXTERNAL              GFSTEP
C              EXTERNAL              GFREFN
C              EXTERNAL              GFREPI
C              EXTERNAL              GFREPU
C              EXTERNAL              GFREPF
C
C              INTEGER               WNCARD
C              LOGICAL               GFBAIL
C              EXTERNAL              GFBAIL
C
C
C              CHARACTER*(*)         TIMFMT
C              PARAMETER           ( TIMFMT =
C             .   'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)' )
C
C              DOUBLE PRECISION      CNVTOL
C              PARAMETER           ( CNVTOL = 1.D-6 )
C
C              INTEGER               MAXWIN
C              PARAMETER           ( MAXWIN = 2 * 100 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 40 )
C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              CHARACTER*(TIMLEN)    WIN0
C              CHARACTER*(TIMLEN)    WIN1
C              CHARACTER*(TIMLEN)    BEGSTR
C              CHARACTER*(TIMLEN)    ENDSTR
C
C              DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
C              DOUBLE PRECISION      ET0
C              DOUBLE PRECISION      ET1
C              DOUBLE PRECISION      LEFT
C              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C              DOUBLE PRECISION      RIGHT
C
C              INTEGER               I
C
C              LOGICAL               BAIL
C              LOGICAL               RPT
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ( 'standard.tm' )
C
C        C
C        C     Initialize the confinement and result windows.
C        C
C              CALL SSIZED ( MAXWIN, CNFINE )
C              CALL SSIZED ( MAXWIN, RESULT )
C
C        C
C        C     Obtain the TDB time bounds of the confinement
C        C     window, which is a single interval in this case.
C        C
C              WIN0 = '2001 DEC 01 00:00:00 TDB'
C              WIN1 = '2002 JAN 01 00:00:00 TDB'
C
C              CALL STR2ET ( WIN0, ET0 )
C              CALL STR2ET ( WIN1, ET1 )
C
C        C
C        C     Insert the time bounds into the confinement
C        C     window.
C        C
C              CALL WNINSD ( ET0, ET1, CNFINE )
C
C        C
C        C     Select a 20 second step. We'll ignore any occultations
C        C     lasting less than 20 seconds.
C        C
C              CALL GFSSTP ( 20.D0 )
C
C        C
C        C     Turn on progress reporting; turn off interrupt
C        C     handling.
C        C
C              RPT  = .TRUE.
C              BAIL = .FALSE.
C
C        C
C        C     Perform the search.
C        C
C              CALL GFOCCE ( 'ANY',
C             .              'MOON',   'ellipsoid',  'IAU_MOON',
C             .              'SUN',    'ellipsoid',  'IAU_SUN',
C             .              'LT',     'EARTH',      CNVTOL,
C             .              GFSTEP,   GFREFN,       RPT,
C             .              GFREPI,   GFREPU,       GFREPF,
C             .              BAIL,     GFBAIL,       CNFINE,  RESULT )
C
C
C              IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C                 WRITE (*,*) 'No occultation was found.'
C
C              ELSE
C
C                 DO I = 1, WNCARD(RESULT)
C        C
C        C           Fetch and display each occultation interval.
C        C
C                    CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C                    CALL TIMOUT ( LEFT,  TIMFMT, BEGSTR )
C                    CALL TIMOUT ( RIGHT, TIMFMT, ENDSTR )
C
C                    WRITE (*,*) 'Interval ', I
C                    WRITE (*,*) '   Start time: '//BEGSTR
C                    WRITE (*,*) '   Stop time:  '//ENDSTR
C
C                 END DO
C
C              END IF
C
C              END
C
C
C     When this program was executed on a PC/Linux/g77 platform, the
C     progress report had the format shown below:
C
C        Occultation/transit search   6.02% done.
C
C     The completion percentage was updated approximately once per
C     second.
C
C     When this program completed execution, the output was:
C
C        Occultation/transit search 100.00% done.
C         Interval  1
C            Start time: 2001 DEC 14 20:10:14.195952  (TDB)
C            Stop time:  2001 DEC 14 21:35:50.317994  (TDB)
C
C
C$ Restrictions
C
C     1) If the caller passes in the default, constant step 
C        size routine, GFSTEP, the caller must set the step
C        size by calling the entry point GFSSTP before
C        calling GFOCCE. The call syntax for GFSSTP is
C
C           CALL GFSSTP ( STEP )
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
C     I.M. Underwood (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0 15-APR-2009 (NJB) (LSE) (WLT) (IMU) (EDW) 
C
C-&


C$ Index_Entries
C
C      GF mid-level occultation search
C
C-& 

C
C     SPICELIB functions
C
      INTEGER               WNCARD
      INTEGER               SIZED

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     External routines
C
      EXTERNAL              ZZGFOCST

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
      CHARACTER*(SHPLEN)    LBSHAP
      CHARACTER*(SHPLEN)    LFSHAP
  
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

      CALL CHKIN  ( 'GFOCCE' ) 
     
C
C     Check the result window's size.
C
      IF ( SIZED(RESULT) .LT. 2 ) THEN

         CALL SETMSG ( 'Result window size must be at least 2 '
     .   //            'but was #.'                            )
         CALL ERRINT ( '#',  SIZED(RESULT)                     )
         CALL SIGERR ( 'SPICE(WINDOWTOOSMALL)'                 )
         CALL CHKOUT ( 'GFOCCE'                                )
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
         CALL CHKOUT ( 'GFOCCE'                                )
         RETURN

      END IF

C
C     Check the target shape specifications.
C
      CALL LJUST ( BSHAPE, LBSHAP )
      CALL UCASE ( LBSHAP, LBSHAP )

      CALL LJUST ( FSHAPE, LFSHAP )
      CALL UCASE ( LFSHAP, LFSHAP )

C
C     Note for maintenance programmer: these checks will
C     require modification to handle DSK-based shapes.
C


      IF (      ( LFSHAP .EQ. PTSHAP ) 
     .    .AND. ( LBSHAP .EQ. PTSHAP )  ) THEN 

         CALL SETMSG ( 'The front and back target shape '
     .   //            'specifications are both PTSHAP; '
     .   //            'at least one of these targets '
     .   //            'must be an extended object.'     )
         CALL SIGERR ( 'SPICE(INVALIDSHAPECOMBO)'        )
         CALL CHKOUT ( 'GFOCCE'                          )
         RETURN

      END IF


C 
C     Initialize the occultation calculation.
C  
      CALL ZZGFOCIN  ( OCCTYP, FRONT,  LFSHAP, FFRAME, 
     .                 BACK,   LBSHAP, BFRAME, OBSRVR, 
     .                 ABCORR                         )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'GFOCCE' )
         RETURN
      END IF
 
C
C     Prepare the progress reporter if appropriate.
C
      IF ( RPT ) THEN
         CALL UDREPI ( CNFINE, 'Occultation/transit search ', 'done.' )
      END IF
 
C 
C     Cycle over the intervals in the confining window.
C
      COUNT = WNCARD(CNFINE)
 
      DO I = 1, COUNT
C
C        Retrieve the bounds for the Ith interval of the confinement
C        window. Search this interval for occultation events. Union the
C        result with the contents of the RESULT window.
C
         CALL WNFETD ( CNFINE, I, START, FINISH  )
      
         CALL ZZGFSOLV ( ZZGFOCST,   UDSTEP,   UDREFN,   BAIL,
     .                   UDBAIL,     CSTEP,    STEP,     START,
     .                   FINISH,     TOL,      RPT,      UDREPU,
     .                   RESULT   )
       

         IF (  FAILED()  ) THEN
            CALL CHKOUT ( 'GFOCCE'  )
            RETURN
         END IF

         IF ( BAIL ) THEN
C
C           Interrupt handling is enabled.
C
            IF ( UDBAIL () ) THEN
C
C              An interrupt has been issued. Return now regardless of
C              whether the search has been completed.
C
               CALL CHKOUT ( 'GFOCCE' )
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

      CALL CHKOUT ( 'GFOCCE' )
      RETURN
      END
