C$Procedure GFRR ( GF, range rate search )

      SUBROUTINE GFRR ( TARGET, ABCORR, OBSRVR, RELATE,
     .                  REFVAL, ADJUST, STEP,   CNFINE,
     .                  MW,     NW,     WORK,   RESULT )

C$ Abstract
C
C     Determine time intervals for which a specified constraint
C     on the observer-target range rate is met.
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
C     EVENT
C     GEOMETRY
C     EPHEMERIS
C     SEARCH
C     WINDOW
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TARGET
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
C     TARGET     I   Name of the target body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     RELATE     I   Relational operator.
C     REFVAL     I   Reference value.
C     ADJUST     I   Adjustment value for absolute extrema searches.
C     STEP       I   Step size used for locating extrema and roots.
C     CNFINE     I   SPICE window to which the search is confined.
C     MW         I   Workspace window size.
C     NW         I   The number of workspace windows needed for
C                    the search.
C     WORK      I-O   Array of workspace windows.
C     RESULT    I-O   SPICE window containing results.
C
C$ Detailed_Input
C
C     TARGET   the string name of a target body.  Optionally, you may
C              supply the integer ID code for the object as an
C              integer string.  For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C              The target and observer define a position vector that
C              points from the observer to the target. The derivative
C              with respect to time of the length of this vector
C              is the "range rate" used by this routine as the geometric
C              quantity of interest.
C
C              Case and leading or trailing blanks are not significant
C              in the string TARGET.
C
C     ABCORR   the string description of the aberration corrections to
C              apply to the state evaluations to account for one-way
C              light time and stellar aberration.
C
C              Any aberration correction accepted by the SPICE
C              routine SPKEZR is accepted here. See the header
C              of SPKEZR for a detailed description of the
C              aberration correction options. For convenience,
C              the options are listed below:
C
C                 'NONE'     Apply no correction. Returns the "true"
C                            geometric state.
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
C                'CN+S'     "Reception" case:  converged
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
C              Case and leading or trailing blanks are not significant
C              in the string ABCORR.
C
C     OBSRVR   the string name of an observing body.  Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to indicate the observer as Earth.
C
C              Case and leading or trailing blanks are not significant
C              in the string OBSRVR.
C
C     RELATE   the string or character describing the relational
C              operator that defines the constraint on the
C              range rate of the observer-target vector. The result
C              window found by this routine indicates the time intervals
C              where the constraint is satisfied. Supported values of
C              RELATE and corresponding meanings are shown below:
C
C                 '>'       The range rate value is greater than the
C                           reference value REFVAL.
C
C                 '='       The range rate value is equal to the
C                           reference value REFVAL.
C
C                 '<'       The range rate value is less than the
C                           reference value REFVAL.
C
C                 'ABSMAX'  The range rate value is at an absolute
C                           maximum.
C
C                 'ABSMIN'  The range rate value is at an absolute
C                           minimum.
C
C                 'LOCMAX'  The range rate value is at a local
C                           maximum.
C
C                 'LOCMIN'  The range rate value is at a local
C                           minimum.
C
C              The caller may indicate that the region of interest
C              is the set of time intervals where the quantity is
C              within a specified measure of an absolute extremum.
C              The argument ADJUST (described below) is used to
C              specify this measure.
C
C              Local extrema are considered to exist only in the
C              interiors of the intervals comprising the confinement
C              window:  a local extremum cannot exist at a boundary
C              point of the confinement window.
C
C              Case and leading or trailing blanks are not
C              significant in the string RELATE.
C
C     REFVAL   the double precision reference value used together with
C              the argument RELATE to define an equality or inequality
C              to satisfy by the range rate of the observer-target
C              vector. See the discussion of RELATE above for
C              further information.
C
C              The units of REFVAL are km/s.
C
C     ADJUST   a double precision value used to modify searches for
C              absolute extrema: when RELATE is set to ABSMAX or ABSMIN
C              and ADJUST is set to a positive value, GFRR finds
C              times when the range rate is within
C              ADJUST kilometers/second of the specified extreme value.
C
C              For RELATE set to ABSMAX, the RESULT window contains
C              time intervals when the range rate has
C              values between ABSMAX - ADJUST and ABSMAX.
C
C              For RELATE set to ABSMIN, the RESULT window contains
C              time intervals when the range rate has
C              values between ABSMIN and ABSMIN + ADJUST.
C
C              ADJUST is not used for searches for local extrema,
C              equality or inequality conditions.
C
C     STEP     the double precision time step size to use in the search.
C
C              STEP must be short enough for a search using this step
C              size to locate the time intervals where the range rate
C              function is monotone increasing or decreasing. However,
C              STEP must not be *too* short, or the search will take an
C              unreasonable amount of time.
C
C              The choice of STEP affects the completeness but not
C              the precision of solutions found by this routine; the
C              precision is controlled by the convergence tolerance.
C              See the discussion of the parameter CNVTOL for
C              details.
C
C              STEP has units of TDB seconds.
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
C              parameter NWRR; this parameter is declared in the
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
C                 DOUBLE PRECISION    WORK ( LBCELL : MW, NWRR )
C
C              where MW is a constant declared by the caller and
C              NWRR is a constant defined in the SPICELIB INCLUDE
C              file gf.inc. See the discussion of MW above.
C
C              WORK need not be initialized by the caller.
C
C     RESULT   a double precision SPICE window that will contain the
C              search results. RESULT must be initialized using
C              a call to SSIZED. RESULT must be declared and initialized
C              with sufficient size to capture the full set of time
C              intervals within the search region on which the specified
C              constraint is satisfied.
C
C              If RESULT is non-empty on input, its contents
C              will be discarded before GFRR conducts its
C              search.
C
C$ Detailed_Output
C
C     WORK     the input workspace array, modified by this
C              routine.
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
C              windows on which the range rate is increasing
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
C         that if the step size is non-positive, the error
C         SPICE(INVALIDSTEP) is signaled.
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
C     3)  If the workspace window size MW is less than 2 or not an even
C         value, the error SPICE(INVALIDDIMENSION) will signal. If the
C         size of the workspace is too small, an error is signaled by a
C         routine in the call tree of this routine.
C
C     4)  If the size of the SPICE window RESULT is less than 2 or
C         not an even value, the error SPICE(INVALIDDIMENSION) will
C         signal. If RESULT has insufficient capacity to contain the
C         number of intervals on which the specified distance condition
C         is met, the error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     5)  If the window count NW is less than NWRR, the error
C         SPICE(INVALIDDIMENSION) will be signaled.
C
C     6)  If an error (typically cell overflow) occurs during
C         window arithmetic, the error will be diagnosed by a routine
C         in the call tree of this routine.
C
C     7)  If the relational operator RELATE is not recognized, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     8)  If ADJUST is negative, the error SPICE(VALUEOUTOFRANGE) will
C         signal from a routine in the call tree of this routine.
C
C         A non-zero value for ADJUST when RELATE has any value other
C         than "ABSMIN" or "ABSMAX" causes the error SPICE(INVALIDVALUE)
C         to signal from a routine in the call tree of this routine.
C
C     9)  If either of the input body names do not map to NAIF ID
C         codes, an error is signaled by a routine in the call tree of
C         this routine.
C
C     10) If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     Appropriate SPK and PCK kernels must be loaded by the calling
C     program before this routine is called.
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
C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C     This routine determines if the caller-specified constraint
C     condition on the geometric event (range rate) is satisfied for
C     any time intervals within the confinement window CNFINE. If one
C     or more such time intervals exist, those intervals are added
C     to the RESULT window.
C
C     This routine provides a simpler, but less flexible interface
C     than does the routine GFEVNT for conducting searches for
C     observer-target range rate value events. Applications that
C     require support for progress reporting, interrupt handling,
C     non-default step or refinement functions, or non-default
C     convergence tolerance should call GFEVNT rather than this routine.
C
C     Below we discuss in greater detail aspects of this routine's
C     solution process that are relevant to correct and efficient
C     use of this routine in user applications.
C
C
C     The Search Process
C     ==================
C
C     Regardless of the type of constraint selected by the caller, this
C     routine starts the search for solutions by determining the time
C     periods, within the confinement window, over which the
C     range rate function is monotone increasing and monotone
C     decreasing. Each of these time periods is represented by a SPICE
C     window. Having found these windows, all of the range rate
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
C     change of range rate will be sampled. Starting at
C     the left endpoint of an interval, samples will be taken at each
C     step. If a change of sign is found, a root has been bracketed; at
C     that point, the time at which the time derivative of the
C     range rate is zero can be found by a refinement process, for
C     example, using a binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the range rate function is monotone:
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
C     attained and times when the range rate function is equal to a
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
C     Example:
C
C     Determine the time windows from January 1, 2007 UTC to
C     April 1, 2007 UTC for which the sun-moon range rate satisfies the
C     relation conditions with respect to a reference value of
C     0.3365 km/s radians (this range rate known to occur within the
C     search interval). Also determine the time windows corresponding
C     to the local maximum and minimum range rate, and the absolute
C     maximum and minimum range rate during the search interval.
C
C           PROGRAM GFRR_T
C           IMPLICIT NONE
C
C     C
C     C     Include GF parameter declarations:
C     C
C           INCLUDE 'gf.inc'
C
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      SPD
C           DOUBLE PRECISION      DVNORM
C           INTEGER               WNCARD
C
C     C
C     C     Local parameters
C     C
C           INTEGER               LBCELL
C           PARAMETER           ( LBCELL = -5 )
C
C     C
C     C     Use the parameter MAXWIN for both the result window size and
C     C     the workspace size.
C     C
C           INTEGER               MAXWIN
C           PARAMETER           ( MAXWIN = 20000 )
C
C     C
C     C     Length of strings:
C     C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 26 )
C
C           INTEGER               NLOOPS
C           PARAMETER           ( NLOOPS = 7 )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(TIMLEN)    TIMSTR
C           CHARACTER*(TIMLEN)    RELATE (NLOOPS)
C
C           DOUBLE PRECISION      ADJUST
C           DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C           DOUBLE PRECISION      DRDT
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      ET1
C           DOUBLE PRECISION      FINISH
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      POS    ( 6 )
C           DOUBLE PRECISION      REFVAL
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      START
C           DOUBLE PRECISION      STEP
C           DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWRR )
C
C           INTEGER               I
C           INTEGER               J
C
C
C           DATA                  RELATE / '=',
C          .                               '<',
C          .                               '>',
C          .                               'LOCMIN',
C          .                               'ABSMIN',
C          .                               'LOCMAX',
C          .                               'ABSMAX'  /
C
C     C
C     C     Load kernels.
C     C
C           CALL FURNSH ( 'standard.tm' )
C
C     C
C     C     Initialize windows.
C     C
C           CALL SSIZED ( MAXWIN, RESULT )
C           CALL SSIZED ( 2,      CNFINE )
C
C     C
C     C     Store the time bounds of our search interval in
C     C     the confinement window.
C     C
C           CALL STR2ET ( '2007 JAN 1', ET0 )
C           CALL STR2ET ( '2007 APR 1', ET1 )
C
C           CALL WNINSD ( ET0, ET1, CNFINE )
C
C     C
C     C     Search using a step size of 1 day (in units of seconds).
C     C     The reference value is .3365 km/s. We're not using the
C     C     adjustment feature, so we set ADJUST to zero.
C     C
C           STEP   = SPD()
C           REFVAL = .3365D0
C           ADJUST = 0.D0
C
C           DO J=1, NLOOPS
C
C              WRITE(*,*) 'Relation condition: ', RELATE(J)
C
C     C
C     C        Perform the search. The SPICE window RESULT contains
C     C        the set of times when the condition is met.
C     C
C              CALL GFRR (  'MOON', 'NONE', 'SUN', RELATE(J),
C          .                 REFVAL, ADJUST, STEP,    CNFINE,
C          .                 MAXWIN, NWRR,   WORK,    RESULT )
C     C
C     C        Display the results.
C     C
C              IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C                 WRITE (*, '(A)') 'Result window is empty.'
C
C              ELSE
C
C                 DO I = 1, WNCARD(RESULT)
C     C
C     C              Fetch the endpoints of the Ith interval
C     C              of the result window.
C     C
C                    CALL WNFETD ( RESULT, I, START, FINISH )
C
C                    CALL SPKEZR ( 'MOON',  START, 'J2000', 'NONE',
C          .                       'SUN', POS,   LT              )
C                    DRDT = DVNORM(POS)
C
C                    CALL TIMOUT ( START, 'YYYY-MON-DD HR:MN:SC.###',
C          .                       TIMSTR                            )
C
C                    WRITE (*, '(A,F16.9)' ) 'Start time, drdt = '//
C          .                                 TIMSTR, DRDT
C
C                    CALL SPKEZR ( 'MOON',  FINISH, 'J2000', 'NONE',
C          .                       'SUN', POS,     LT              )
C                    DRDT = DVNORM(POS)
C
C                    CALL TIMOUT ( FINISH, 'YYYY-MON-DD HR:MN:SC.###',
C          .                       TIMSTR                            )
C
C                    WRITE (*, '(A,F16.9)' ) 'Stop time,  drdt = '//
C          .                              TIMSTR, DRDT
C                 END DO
C
C              END IF
C
C              WRITE(*,*) ' '
C
C           END DO
C
C           END
C
C     The program outputs:
C
C        Relation condition: =
C        Start time, drdt = 2007-JAN-02 00:35:19.574       0.336500000
C        Stop time,  drdt = 2007-JAN-02 00:35:19.574       0.336500000
C        Start time, drdt = 2007-JAN-19 22:04:54.899       0.336500000
C        Stop time,  drdt = 2007-JAN-19 22:04:54.899       0.336500000
C        Start time, drdt = 2007-FEB-01 23:30:13.428       0.336500000
C        Stop time,  drdt = 2007-FEB-01 23:30:13.428       0.336500000
C        Start time, drdt = 2007-FEB-17 11:10:46.540       0.336500000
C        Stop time,  drdt = 2007-FEB-17 11:10:46.540       0.336500000
C        Start time, drdt = 2007-MAR-04 15:50:19.929       0.336500000
C        Stop time,  drdt = 2007-MAR-04 15:50:19.929       0.336500000
C        Start time, drdt = 2007-MAR-18 09:59:05.959       0.336500000
C        Stop time,  drdt = 2007-MAR-18 09:59:05.959       0.336500000
C
C        Relation condition: <
C        Start time, drdt = 2007-JAN-02 00:35:19.574       0.336500000
C        Stop time,  drdt = 2007-JAN-19 22:04:54.899       0.336500000
C        Start time, drdt = 2007-FEB-01 23:30:13.428       0.336500000
C        Stop time,  drdt = 2007-FEB-17 11:10:46.540       0.336500000
C        Start time, drdt = 2007-MAR-04 15:50:19.929       0.336500000
C        Stop time,  drdt = 2007-MAR-18 09:59:05.959       0.336500000
C
C        Relation condition: >
C        Start time, drdt = 2007-JAN-01 00:00:00.000       0.515522367
C        Stop time,  drdt = 2007-JAN-02 00:35:19.574       0.336500000
C        Start time, drdt = 2007-JAN-19 22:04:54.899       0.336500000
C        Stop time,  drdt = 2007-FEB-01 23:30:13.428       0.336500000
C        Start time, drdt = 2007-FEB-17 11:10:46.540       0.336500000
C        Stop time,  drdt = 2007-MAR-04 15:50:19.929       0.336500000
C        Start time, drdt = 2007-MAR-18 09:59:05.959       0.336500000
C        Stop time,  drdt = 2007-APR-01 00:00:00.000       0.793546222
C
C        Relation condition: LOCMIN
C        Start time, drdt = 2007-JAN-11 07:03:58.988      -0.803382743
C        Stop time,  drdt = 2007-JAN-11 07:03:58.988      -0.803382743
C        Start time, drdt = 2007-FEB-10 06:26:15.439      -0.575837623
C        Stop time,  drdt = 2007-FEB-10 06:26:15.439      -0.575837623
C        Start time, drdt = 2007-MAR-12 03:28:36.404      -0.441800446
C        Stop time,  drdt = 2007-MAR-12 03:28:36.404      -0.441800446
C
C        Relation condition: ABSMIN
C        Start time, drdt = 2007-JAN-11 07:03:58.988      -0.803382743
C        Stop time,  drdt = 2007-JAN-11 07:03:58.988      -0.803382743
C
C        Relation condition: LOCMAX
C        Start time, drdt = 2007-JAN-26 02:27:33.766       1.154648992
C        Stop time,  drdt = 2007-JAN-26 02:27:33.766       1.154648992
C        Start time, drdt = 2007-FEB-24 09:35:07.816       1.347132236
C        Stop time,  drdt = 2007-FEB-24 09:35:07.816       1.347132236
C        Start time, drdt = 2007-MAR-25 17:26:56.150       1.428141707
C        Stop time,  drdt = 2007-MAR-25 17:26:56.150       1.428141707
C
C        Relation condition: ABSMAX
C        Start time, drdt = 2007-MAR-25 17:26:56.150       1.428141707
C        Stop time,  drdt = 2007-MAR-25 17:26:56.150       1.428141707
C
C$ Restrictions
C
C     1) The kernel files to be used by this routine must be loaded
C        (normally using the SPICELIB routine FURNSH) before this
C        routine is called.
C
C     2) This routine has the side effect of re-initializing the
C        range rate quantity utility package. Callers may themselves
C        need to re-initialize the range rate quantity utility
C        package after calling this routine.
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
C        Edits to Example section, proper description of "standard.tm"
C        meta kernel.
C
C-    SPICELIB Version 1.0.0, 24-JUN-2009 (EDW)
C
C-&

C$ Index_Entries
C
C   GF range rate search
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               GFBAIL
      INTEGER               SIZED
      LOGICAL               ODD

      EXTERNAL              GFBAIL

C
C     Routines to set step size, refine transition times
C     and report work.
C
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
      PARAMETER           ( QNPARS = 3 )

      LOGICAL               NOBAIL
      PARAMETER           ( NOBAIL = .FALSE. )

      LOGICAL               NORPT
      PARAMETER           ( NORPT  = .FALSE. )

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

C
C     Check into the error subsystem.
C
      CALL CHKIN( 'GFRR' )

C
C     Confirm minimum window sizes.
C
      IF ( MW .LT. 2 .OR. ODD(MW) ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2 and an even value.'            )
         CALL ERRINT ( '#',  MW                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                  )
         CALL CHKOUT ( 'GFRR'                                     )
         RETURN

      END IF


      IF ( NW .LT. NWRR ) THEN

         CALL SETMSG ( 'Workspace window count was #; count must be '
     .   //            'at least #.'                                )
         CALL ERRINT ( '#',  NW                                     )
         CALL ERRINT ( '#',  NWRR                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                    )
         CALL CHKOUT ( 'GFRR'                                       )
         RETURN

      END IF


C
C     Check the result window size.
C
      IF ( (SIZED(RESULT) .LT. 2) .OR. ODD( SIZED(RESULT) ) ) THEN

         CALL SETMSG ( 'Result window size was #; size must be '
     .   //            'at least 2 and an even value.'             )
         CALL ERRINT ( '#', SIZED(RESULT)                          )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFRR'                                      )
         RETURN

      END IF


C
C     Set up a call to GFEVNT specific to the range rate search.
C
      QPNAMS(1) = 'TARGET'
      QCPARS(1) =  TARGET

      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) =  OBSRVR

      QPNAMS(3) = 'ABCORR'
      QCPARS(3) =  ABCORR

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
C     Progress report and interrupt options are set to .FALSE.
C
      CALL GFEVNT ( GFSTEP,   GFREFN,  'RANGE RATE', QNPARS,
     .              QPNAMS,   QCPARS,   QDPARS,      QIPARS,
     .              QLPARS,   RELATE,   REFVAL,      TOL,
     .              ADJUST,   CNFINE,   NORPT,       GFREPI,
     .              GFREPU,   GFREPF,   MW,          NWRR,
     .              WORK,     NOBAIL,   GFBAIL,      RESULT  )


      CALL CHKOUT( 'GFRR' )
      RETURN

      END

