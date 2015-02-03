C$Procedure GFSEP (GF, angular separation search)

      SUBROUTINE GFSEP ( TARG1,  SHAPE1, FRAME1,
     .                   TARG2,  SHAPE2, FRAME2,
     .                   ABCORR, OBSRVR, RELATE,
     .                   REFVAL, ADJUST, STEP,
     .                   CNFINE, MW,     NW,
     .                   WORK,   RESULT )

C$ Abstract
C
C     Determine time intervals when the angular separation between
C     the position vectors of two target bodies relative to an observer
C     satisfies a numerical relationship.
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
C     GF
C     NAIF_IDS
C     SPK
C     TIME
C     WINDOWS
C
C$ Keywords
C
C     ANGULAR SEPARATION
C     GEOMETRY
C     SEARCH
C     EVENT
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TARG1
      CHARACTER*(*)         SHAPE1
      CHARACTER*(*)         FRAME1
      CHARACTER*(*)         TARG2
      CHARACTER*(*)         SHAPE2
      CHARACTER*(*)         FRAME2
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         RELATE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK   ( LBCELL : MW, NW )
      DOUBLE PRECISION      RESULT ( LBCELL : * )


C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LBCELL     P   SPICE Cell lower bound.
C     CNVTOL     P   Convergence tolerance.
C     ZZGET      P   ZZHOLDD retrieves a stored DP value.
C     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance.
C     TARG1      I   Name of first body.
C     SHAPE1     I   Name of shape model describing the first body.
C     FRAME1     I   The body-fixed reference frame of the first body.
C     TARG2      I   Name of second body.
C     SHAPE2     I   Name of the shape model describing the second body.
C     FRAME2     I   The body-fixed reference frame of the second body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     RELATE     I   Operator that either looks for an extreme value
C                    (max, min, local, absolute) or compares the
C                    angular separation value and REFVAL.
C     REFVAL     I   Reference value.
C     ADJUST     I   Absolute extremum adjustment value.
C     STEP       I   Step size in seconds for finding angular separation
C                    events.
C     CNFINE     I   SPICE window to which the search is restricted.
C     MW         I   Size of workspace windows.
C     NW         I   The number of workspace windows needed for the
C                    search.
C     WORK      I/O  Array containing workspace windows.
C     RESULT    I/O  SPICE window containing results.
C
C$ Detailed_Input
C
C     TARG1    the string naming the first body of interest. You can
C              also supply the integer ID code for the object as an
C              integer string.  For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C     SHAPE1   the string naming the geometric model used to
C              represent the shape of the TARG1 body. Models
C              supported by this routine:
C
C                'SPHERE'        Treat the body as a sphere with
C                                radius equal to the maximum value of
C                                BODYnnn_RADII
C
C                'POINT'         Treat the body as a point;
C                                radius has value zero.
C
C              The SHAPE1 string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     FRAME1   the string naming the body-fixed reference frame
C              corresponding to TARG1. GFSEP does not currently use
C              this argument's value, its use is reserved for future
C              shape models. The value 'NULL' will suffice for
C              "POINT" and "SPHERE" shaped bodies.
C
C     TARG2    the string naming the second body of interest. You can
C              also supply the integer ID code for the object as an
C              integer string.  For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C     SHAPE2   the string naming the geometric model used to
C              represent the shape of the TARG2. Models supported by
C              this routine:
C
C                'SPHERE'        Treat the body as a sphere with
C                                radius equal to the maximum value of
C                                BODYnnn_RADII
C
C                'POINT'         Treat the body as a single point;
C                                radius has value zero.
C
C              The SHAPE2 string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     FRAME2   the string naming the body-fixed reference frame
C              corresponding to TARG2. GFSEP does not currently use
C              this argument's value, its use is reserved for future
C              shape models. The value 'NULL' will suffice for
C              "POINT" and "SPHERE" shaped bodies.
C
C     ABCORR   the string description of the aberration corrections
C              to apply to the state evaluations to account for
C              one-way light time and stellar aberration.
C
C              This routine accepts the same aberration corrections
C              as does the SPICE routine SPKEZR. See the header of
C              SPKEZR for a detailed description of the aberration
C              correction options. For convenience, the options are
C              listed below:
C
C                 'NONE'     Apply no correction.
C
C                 'LT'       "Reception" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case:  converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case:  converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C              The ABCORR string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     OBSRVR   the string naming the observing body. Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to supply to indicate the
C              observer is Earth.
C
C     RELATE   the string identifying the relational operator used to
C              define a constraint on the angular separation. The
C              result window found by this routine indicates the time
C              intervals where the constraint is satisfied. Supported
C              values of RELATE and corresponding meanings are shown
C              below:
C
C                 '>'      Separation is greater than the reference
C                          value REFVAL.
C
C                 '='      Separation is equal to the reference
C                          value REFVAL.
C
C                 '<'      Separation is less than the reference
C                          value REFVAL.
C
C                'ABSMAX'  Separation is at an absolute maximum.
C
C                'ABSMIN'  Separation is at an absolute  minimum.
C
C                'LOCMAX'  Separation is at a local maximum.
C
C                'LOCMIN'  Separation is at a local minimum.
C
C              The caller may indicate that the region of interest
C              is the set of time intervals where the quantity is
C              within a specified angular separation of an absolute
C              extremum. The argument ADJUST (described below) is used
C              to specify this angular separation.
C
C              Local extrema are considered to exist only in the
C              interiors of the intervals comprising the confinement
C              window:  a local extremum cannot exist at a boundary
C              point of the confinement window.
C
C              The RELATE string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     REFVAL   the double precision reference value used together with
C              RELATE argument to define an equality or inequality to
C              be satisfied by the angular separation between the
C              specified target and observer. See the discussion of
C              RELATE above for further information.
C
C              The units of REFVAL are radians.
C
C     ADJUST   a double precision value used to modify searches for
C              absolute extrema: when RELATE is set to ABSMAX or
C              ABSMIN and ADJUST is set to a positive value, GFSEP
C              finds times when the angular separation between the
C              bodies is within ADJUST radians of the specified
C              extreme value.
C
C              For RELATE set to ABSMAX, the RESULT window contains
C              time intervals when the angular separation has
C              values between ABSMAX - ADJUST and ABSMAX.
C
C              For RELATE set to ABSMIN, the RESULT window contains
C              time intervals when the angular separation has
C              values between ABSMIN and ABSMIN + ADJUST.
C
C              ADJUST is not used for searches for local extrema,
C              equality or inequality conditions.
C
C     CNFINE   a double precision SPICE window that confines the time
C              period over which the specified search is conducted.
C              CNFINE may consist of a single interval or a collection
C              of intervals.
C
C              In some cases the confinement window can be used to
C              greatly reduce the time period that must be searched
C              for the desired solution. See the Particulars section
C              below for further discussion.
C
C              See the Examples section below for a code example
C              that shows how to create a confinement window.
C
C              CNFINE must be initialized by the caller using the
C              SPICELIB routine SSIZED.
C
C     STEP     the double precision time step size to use in the
C              search.
C
C              STEP must be short enough to for a search using this
C              step size to locate the time intervals where the
C              specified angular separation function is monotone
C              increasing or decreasing. However, STEP must not be
C              *too* short, or the search will take an unreasonable
C              amount of time.
C
C              The choice of STEP affects the completeness but not
C              the precision of solutions found by this routine; the
C              precision is controlled by the convergence tolerance.
C              See the discussion of the parameter CNVTOL for
C              details.
C
C              STEP has units of TDB seconds.
C
C     MW       is a parameter specifying the length of the SPICE
C              windows in the workspace array WORK (see description
C              below) used by this routine.
C
C              MW should be set to a number at least twice as large
C              as the maximum number of intervals required by any
C              workspace window. In many cases, it's not necessary to
C              compute an accurate estimate of how many intervals are
C              needed; rather, the user can pick a size considerably
C              larger than what's really required.
C
C              However, since excessively large arrays can prevent
C              applications from compiling, linking, or running
C              properly, sometimes MW must be set according to
C              the actual workspace requirement. A rule of thumb
C              for the number of intervals NINTVLS needed is
C
C                  NINTVLS  =  2*N  +  ( M / STEP )
C
C              where
C
C                  N     is the number of intervals in the confinement
C                        window
C
C                  M     is the measure of the confinement window, in
C                        units of seconds
C
C                  STEP  is the search step size in seconds
C
C              MW should then be set to
C
C                  2 * NINTVLS
C
C     NW       is a parameter specifying the number of SPICE windows
C              in the workspace array WORK (see description below)
C              used by this routine. NW should be set to the
C              parameter NWSEP; this parameter is declared in the
C              include file gf.inc. (The reason this dimension is
C              an input argument is that this allows run-time
C              error checking to be performed.)
C
C     WORK     is an array used to store workspace windows. This
C              array should be declared by the caller as shown:
C
C                 INCLUDE 'gf.inc'
C                    ...
C
C                 DOUBLE PRECISION    WORK ( LBCELL : MW, NWSEP )
C
C              where MW is a constant declared by the caller and
C              NWSEP is a constant defined in the SPICELIB INCLUDE
C              file gf.inc. See the discussion of MW above.
C
C              WORK need not be initialized by the caller.
C
C     RESULT   a double precision SPICE window which will contain the
C              search results. RESULT must be initialized using
C              a call to SSIZED. RESULT must be declared and
C              initialized with sufficient size to capture the full
C              set of time intervals within the search region on which
C              the specified constraint is satisfied.
C
C              If RESULT is non-empty on input, its contents
C              will be discarded before GFSEP conducts its
C              search.
C
C$ Detailed_Output
C
C     WORK     the input workspace array, modified by this
C              routine. The caller should re-initialize this array
C              before attempting to use it for any other purpose.
C
C     RESULT   the SPICE window of intervals, contained within the
C              confinement window CNFINE, on which the specified
C              constraint is satisfied.
C
C              If the search is for local extrema, or for absolute
C              extrema with ADJUST set to zero, then normally each
C              interval of RESULT will be a singleton: the left and
C              right endpoints of each interval will be identical.
C
C              If no times within the confinement window satisfy the
C              constraint, RESULT will be returned with a
C              cardinality of zero.
C
C$ Parameters
C
C     LBCELL   the integer value defining the lower bound for
C              SPICE Cell arrays (a SPICE window is a kind of cell).
C
C     CNVTOL   is the convergence tolerance used for finding
C              endpoints of the intervals comprising the result
C              window. CNVTOL is also used for finding intermediate
C              results; in particular, CNVTOL is used for finding the
C              windows on which the specified distance is increasing
C              or decreasing. CNVTOL is used to determine when binary
C              searches for roots should terminate: when a root is
C              bracketed within an interval of length CNVTOL; the
C              root is considered to have been found.
C
C              The accuracy, as opposed to precision, of roots found
C              by this routine depends on the accuracy of the input
C              data. In most cases, the accuracy of solutions will be
C              inferior to their precision.
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
C         that if the step size is non-positive, an error is signaled
C         by a routine in the call tree of this routine.
C
C     2)  Due to numerical errors, in particular,
C
C            - truncation error in time values
C            - finite tolerance value
C            - errors in computed geometric quantities
C
C         it is *normal* for the condition of interest to not always be
C         satisfied near the endpoints of the intervals comprising the
C         RESULT window. One technique to handle such a situation,
C         slightly contract RESULT using the window routine WNCOND.
C
C     3)  SPICE(INVALIDDIMENSION) signals if workspace window size, MW,
C         is not at least 2 and an even value.
C
C     4)  SPICE(INVALIDDIMENSION) signals if workspace window count,
C         NW, is not at least NWSEP.
C
C     5)  SPICE(INVALIDDIMENSION) signals if result window, RESULT,
C         is not at least 2 and an even value.
C
C     6)  If RESULT has insufficient capacity to contain the
C         number of intervals on which the specified distance condition
C         is met, the error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     7)  If an error (typically cell overflow) occurs during
C         window arithmetic, the error will be diagnosed by a routine
C         in the call tree of this routine.
C
C     8)  If the relational operator RELATE is not recognized, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     9)  If ADJUST is negative, an error is signaled by a routine in
C         the call tree of this routine.
C
C     10) If either of the input body names, TARG1, TARG2 do not map
C         to NAIF ID codes, an error is signaled by a routine in the
C         call tree of this routine.
C
C     11) If either of the input body shape names, SHAPE1, SHAPE2,
C         are not recognized by the GF subsystem, an error is signaled
C         by a routine in the call tree of this routine.
C
C     12) If either of the input body frame names, FRAME1, FRAME2,
C         are not recognized by the frame subsystem, an error is
C         signaled by a routine in the call tree of this routine.
C
C     13) If either of the input body frames, FRAME1, FRAME2,
C         are not centered on the corresponding body (FRAME1 on TARG1,
C         FRAME2 on TARG2), an error is signaled by a routine in the
C         call tree of this routine.
C
C     14) If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     Appropriate SPK and PCK kernels must be loaded by the
C     calling program before this routine is called.
C
C     The following data are required:
C
C        - SPK data: the calling application must load ephemeris data
C          for the targets, observer, and any intermediate objects in
C          a chain connecting the targets and observer that cover the
C          time period specified by the window CNFINE. If aberration
C          corrections are used, the states of target and observer
C          relative to the solar system barycenter must be calculable
C          from the available ephemeris data. Typically ephemeris data
C          are made available by loading one or more SPK files using
C          FURNSH.
C
C        - PCK data: bodies modeled as triaxial ellipsoids must have
C          semi-axis lengths provided by variables in the kernel pool.
C          Typically these data are made available by loading a text
C          PCK file using FURNSH.
C
C        - If non-inertial reference frames are used, then PCK
C          files, frame kernels, C-kernels, and SCLK kernels may be
C          needed.
C
C     Such kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine provides a simpler, but less flexible interface
C     than does the routine GFEVNT for conducting searches for
C     angular separation events. Applications that require support for
C     progress reporting, interrupt handling, non-default step or
C     refinement functions, or non-default convergence tolerance should
C     call GFEVNT rather than this routine.
C
C     This routine determines a set of one or more time intervals
C     within the confinement window for which the angular separation
C     between the two bodies satisfies some defined relationship.
C     The resulting set of intervals is returned as a SPICE window.
C
C     Below we discuss in greater detail aspects of this routine's
C     solution process that are relevant to correct and efficient
C     use of this routine in user applications.
C
C     The Search Process
C     ==================
C
C     Regardless of the type of constraint selected by the caller, this
C     routine starts the search for solutions by determining the time
C     periods, within the confinement window, over which the specified
C     angular separation function is monotone increasing and monotone
C     decreasing. Each of these time periods is represented by a SPICE
C     window. Having found these windows, all of the angular separation
C     function's local extrema within the confinement window are known.
C     Absolute extrema then can be found very easily.
C
C     Within any interval of these "monotone" windows, there will be at
C     most one solution of any equality constraint. Since the boundary
C     of the solution set for any inequality constraint is contained in
C     the union of
C
C        - the set of points where an equality constraint is met
C        - the boundary points of the confinement window
C
C     the solutions of both equality and inequality constraints can be
C     found easily once the monotone windows have been found.
C
C
C     Step Size
C     =========
C
C     The monotone windows (described above) are found using a two-step
C     search process. Each interval of the confinement window is
C     searched as follows: first, the input step size is used to
C     determine the time separation at which the sign of the rate of
C     change of angular separation (angular separation rate) will be
C     sampled. Starting at the left endpoint of an interval, samples
C     will be taken at each step. If a change of sign is found, a
C     root has been bracketed; at that point, the time at which the
C     angular separation rate is zero can be found by a refinement
C     process, for example, using a binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the distance function is monotone:
C     the step size should be shorter than the shortest of these
C     intervals (within the confinement window).
C
C     The optimal step size is *not* necessarily related to the lengths
C     of the intervals comprising the result window. For example, if
C     the shortest monotone interval has length 10 days, and if the
C     shortest result window interval has length 5 minutes, a step size
C     of 9.9 days is still adequate to find all of the intervals in the
C     result window. In situations like this, the technique of using
C     monotone windows yields a dramatic efficiency improvement over a
C     state-based search that simply tests at each step whether the
C     specified constraint is satisfied. The latter type of search can
C     miss solution intervals if the step size is longer than the
C     shortest solution interval.
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
C     As described above, the root-finding process used by this routine
C     involves first bracketing roots and then using a search process
C     to locate them. "Roots" are both times when local extrema are
C     attained and times when the distance function is equal to a
C     reference value. All endpoints of the intervals comprising the
C     result window are either endpoints of intervals of the
C     confinement window or roots.
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
C     slow search of interest must be performed.
C
C
C     Negative Angular Separation
C     ===========================
C
C     For those searches using a SPHERE shape identifier for both
C     target bodies, the angular separation function returns a
C     negative value when the bodies overlap (occult), e.g.
C     a search for an ABSMIN of angular separation in a
C     confinement window covering an occultation event will
C     return the time when the apparent center of the
C     occulting body passes closest to the apparent center of
C     the occulted body.
C
C
C     Elongation
C     ===========================
C
C     The angular separation of two targets as seen from an observer
C     where one of those targets is the sun is known as elongation.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
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
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de421.bsp                     Planetary ephemeris
C              pck00009.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00009.tpc',
C                                  'naif0009.tls'  )
C
C           \begintext
C
C     Example(1):
C
C     Determine the times of local maxima of the angular separation
C     between the moon and earth as observed from the sun from
C     January 1, 2007 UTC to January 1 2008 UTC.
C
C           PROGRAM EX1
C           IMPLICIT              NONE
C
C     C
C     C     Include GF parameter declarations:
C     C
C           INCLUDE               'gf.inc'
C
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      SPD
C           DOUBLE PRECISION      RPD
C           INTEGER               WNCARD
C
C     C
C     C     Local variables and initial parameters.
C     C
C           INTEGER               LBCELL
C           PARAMETER           ( LBCELL = -5 )
C
C     C
C     C     Create 50 windows.
C     C
C           INTEGER               MAXWIN
C           PARAMETER           ( MAXWIN = 50 )
C
C     C
C     C     One window consists of two intervals.
C     C
C           INTEGER               NINTRVL
C           PARAMETER           ( NINTRVL = MAXWIN *2 )
C
C           INTEGER               STRLEN
C           PARAMETER           ( STRLEN = 64 )
C
C           CHARACTER*(STRLEN)    BEGSTR
C           CHARACTER*(STRLEN)    ENDSTR
C           CHARACTER*(STRLEN)    TARG1
C           CHARACTER*(STRLEN)    TARG2
C           CHARACTER*(STRLEN)    OBSRVR
C           CHARACTER*(STRLEN)    SHAPE1
C           CHARACTER*(STRLEN)    SHAPE2
C           CHARACTER*(STRLEN)    FRAME1
C           CHARACTER*(STRLEN)    FRAME2
C           CHARACTER*(STRLEN)    ABCORR
C
C           DOUBLE PRECISION      STEP
C           DOUBLE PRECISION      CNFINE ( LBCELL : NINTRVL )
C           DOUBLE PRECISION      RESULT ( LBCELL : NINTRVL )
C           DOUBLE PRECISION      WORK   ( LBCELL : NINTRVL, NWSEP )
C           DOUBLE PRECISION      BEGTIM
C           DOUBLE PRECISION      ENDTIM
C           DOUBLE PRECISION      BEG
C           DOUBLE PRECISION      END
C           DOUBLE PRECISION      REFVAL
C           DOUBLE PRECISION      ADJUST
C
C           INTEGER               COUNT
C           INTEGER               I
C
C
C     C
C     C     Load kernels.
C     C
C           CALL FURNSH ('standard.tm')
C
C     C
C     C     Initialize windows RESULT and CNFINE.
C     C
C           CALL SSIZED ( NINTRVL, RESULT )
C           CALL SSIZED ( 2,      CNFINE )
C
C     C
C     C     Store the time bounds of our search interval in
C     C     the CNFINE confinement window.
C     C
C           CALL STR2ET ( '2007 JAN 01', BEGTIM )
C           CALL STR2ET ( '2008 JAN 01', ENDTIM )
C
C           CALL WNINSD ( BEGTIM, ENDTIM, CNFINE )
C
C     C
C     C     Search using a step size of 6 days (in units of seconds).
C     C
C           STEP   = 6.D0 * SPD()
C           ADJUST = 0.D0
C           REFVAL = 0.D0
C
C           TARG1  = 'MOON'
C           SHAPE1 = 'SPHERE'
C           FRAME1 = 'NULL'
C
C           TARG2  = 'EARTH'
C           SHAPE2 = 'SPHERE'
C           FRAME2 = 'NULL'
C           ABCORR = 'NONE'
C
C           OBSRVR = 'SUN'
C
C           CALL GFSEP ( TARG1,  SHAPE1,  FRAME1,
C          .             TARG2,  SHAPE2,  FRAME2,
C          .             ABCORR, OBSRVR, 'LOCMAX',
C          .             REFVAL, ADJUST,  STEP,
C          .             CNFINE, NINTRVL, NWSEP, WORK,
C          .             RESULT )
C
C     C
C     C     Check the number of intervals in the result window.
C     C
C           COUNT = WNCARD(RESULT)
C
C     C
C     C     List the beginning and ending points in each interval
C     C     if RESULT contains data.
C     C
C           IF ( COUNT .EQ. 0 ) THEN
C              WRITE (*, '(A)') 'Result window is empty.'
C           ELSE
C
C              DO I = 1, COUNT
C
C     C
C     C           Fetch the endpoints of the Ith interval
C     C           of the result window.
C     C
C                 CALL WNFETD ( RESULT, I, BEG, END  )
C
C                 CALL TIMOUT ( BEG,
C          .                'YYYY-MON-DD HR:MN:SC.###### '
C          .  //            '(TDB) ::TDB ::RND',  BEGSTR )
C                 CALL TIMOUT ( END,
C          .                'YYYY-MON-DD HR:MN:SC.###### '
C          . //             '(TDB) ::TDB ::RND',  ENDSTR )
C
C                 WRITE (*,*) 'Interval ',  I
C                 WRITE (*,*) 'Beginning TDB ', BEGSTR
C                 WRITE (*,*) 'Ending TDB    ', ENDSTR
C
C              END DO
C
C           END IF
C
C           END
C
C     The program's partial output:
C
C        Interval  1
C        Beginning TDB 2007-JAN-11 11:21:20.213872 (TDB)
C        Ending TDB    2007-JAN-11 11:21:20.213872 (TDB)
C
C        Interval  2
C        Beginning TDB 2007-JAN-26 01:43:41.029955 (TDB)
C        Ending TDB    2007-JAN-26 01:43:41.029955 (TDB)
C
C              ...
C
C        Interval  24
C        Beginning TDB 2007-DEC-17 04:04:46.935442 (TDB)
C        Ending TDB    2007-DEC-17 04:04:46.935442 (TDB)
C
C        Interval  25
C        Beginning TDB 2007-DEC-31 13:43:52.558897 (TDB)
C        Ending TDB    2007-DEC-31 13:43:52.558897 (TDB)
C
C     Example(2):
C
C        Determine the time of local maxima elongation of the
C        Moon as seen from earth for the same time interval
C        as the previous example:
C
C        Edit the Example(1) program to use the assignments:
C
C           TARG1  = 'MOON'
C           TARG2  = 'SUN'
C           OBSRVR = 'EARTH'
C
C     The program's partial output:
C
C        Interval  1
C        Beginning TDB 2007-JAN-03 14:20:24.618884 (TDB)
C        Ending TDB    2007-JAN-03 14:20:24.618884 (TDB)
C
C        Interval  2
C        Beginning TDB 2007-FEB-02 06:16:24.101655 (TDB)
C        Ending TDB    2007-FEB-02 06:16:24.101655 (TDB)
C
C              ...
C
C        Interval  12
C        Beginning TDB 2007-NOV-24 14:31:04.334590 (TDB)
C        Ending TDB    2007-NOV-24 14:31:04.334590 (TDB)
C
C        Interval  13
C        Beginning TDB 2007-DEC-24 01:40:12.238389 (TDB)
C        Ending TDB    2007-DEC-24 01:40:12.238389 (TDB)
C
C$ Restrictions
C
C
C     1) The kernel files to be used by this routine must be loaded
C        (normally using the SPICELIB routine FURNSH) before this
C        routine is called.
C
C     2) This routine has the side effect of re-initializing the
C        angular separation quantity utility package.  Callers may
C        need to re-initialize the package after calling this routine.
C
C     3) Due to the current logic implemented in ZZGFSPU, a direct
C        search for zero angular separation of two point targets will
C        always fails, i.e.,
C
C           RELATE = '='
C           REFVAL = 0.D0
C
C        Use RELATE values of 'ABSMIN' or 'LOCMIN' to detect such an
C        event(s).
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 05-SEP-2012 (EDW)
C
C        Edit to comments to correct search description.
C
C        Implemented use of ZZHOLDD to allow user to alter convergence
C        tolerance.
C
C        Removed the STEP > 0 error check. The GFSSTP call includes
C        the check.
C
C        Small text edit for clarity on example code description; full
C        date strings replaced abbreviated versions.
C
C        Edits to Example section, proper description of "standard.tm"
C        meta kernel.
C
C        Edits to Exceptions section to improve description of
C        exceptions and error signals.
C
C-    SPICELIB Version 1.0.1, 29-DEC-2009 (EDW)
C
C        Edited argument descriptions. Removed mention of "ELLIPSOID"
C        shape from SHAPE1 and SHAPE2 as that option is not yet
C        implemented.
C
C-    SPICELIB Version 1.0.0, 19-FEB-2009 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     GF angular separation search
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               GFBAIL
      INTEGER               SIZED
      LOGICAL               EVEN

C
C     Routines to set step size, refine transition times
C     and report work.
C
      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFSTEP

C
C     Local parameters
C

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               QNPARS
      PARAMETER           ( QNPARS = 8 )

C
C     Local variables
C
      DOUBLE PRECISION      TOL
      LOGICAL               OK

C
C     Quantity definition parameter arrays:
C
      CHARACTER*(LNSIZE)    QCPARS ( QNPARS )
      CHARACTER*(LNSIZE)    QPNAMS ( QNPARS )

      DOUBLE PRECISION      QDPARS ( QNPARS )

      INTEGER               QIPARS ( QNPARS )

      LOGICAL               QLPARS ( QNPARS )

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'GFSEP' )


      IF ( MW .LT. 2 .OR. .NOT. EVEN(MW) ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2 and an even value.'            )
         CALL ERRINT ( '#',  MW                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                  )
         CALL CHKOUT ( 'GFSEP'                                    )
         RETURN

      END IF


      IF ( NW .LT. NWSEP ) THEN

         CALL SETMSG ( 'Workspace window count was #; count must be '
     .   //            'at least #.'                                 )
         CALL ERRINT ( '#',  NW                                      )
         CALL ERRINT ( '#',  NWSEP                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                     )
         CALL CHKOUT ( 'GFSEP'                                       )
         RETURN

      END IF


      IF ( SIZED(RESULT) .LT. 2 .OR. .NOT. EVEN( SIZED(RESULT) ) ) THEN

         CALL SETMSG ( 'Result window size was #; size must be '
     .   //            'at least 2 and an even value.'             )
         CALL ERRINT ( '#', SIZED(RESULT)                          )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFSEP'                                     )
         RETURN

      END IF

C
C    Set the TARGET1 body-fixed frame name and shape model identifier.
C
      QPNAMS(1) = 'TARGET1'
      QCPARS(1) =  TARG1

      QPNAMS(2) = 'FRAME1'
      QCPARS(2) =  FRAME1

      QPNAMS(3) = 'SHAPE1'
      QCPARS(3) =  SHAPE1

C
C    Set the TARGET2 body-fixed frame name and shape model identifier.
C
      QPNAMS(4) = 'TARGET2'
      QCPARS(4) =  TARG2

      QPNAMS(5) = 'FRAME2'
      QCPARS(5) =  FRAME2

      QPNAMS(6) = 'SHAPE2'
      QCPARS(6) =  SHAPE2

C
C     Observer, aberration and calculation reference frame settings.
C
      QPNAMS(7) = 'OBSERVER'
      QCPARS(7) =  OBSRVR

      QPNAMS(8) = 'ABCORR'
      QCPARS(8) =  ABCORR


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
C     Initialize the RESULT window to empty.
C
      CALL SCARDD ( 0, RESULT)

C
C     Look for solutions.
C
C     Progress report and bail-out options are set to .FALSE.
C
      CALL GFEVNT ( GFSTEP,   GFREFN,  'ANGULAR SEPARATION',
     .              QNPARS,   QPNAMS,   QCPARS,
     .              QDPARS,   QIPARS,   QLPARS,
     .              RELATE,   REFVAL,   TOL,
     .              ADJUST,   CNFINE,   .FALSE.,
     .              GFREPI,   GFREPU,   GFREPF,
     .              MW,       NWSEP,    WORK,
     .              .FALSE.,  GFBAIL,   RESULT )

      CALL CHKOUT ( 'GFSEP' )

      RETURN
      END

