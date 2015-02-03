C$Procedure GFDIST ( GF, distance search )

      SUBROUTINE GFDIST ( TARGET, ABCORR, OBSRVR, RELATE,
     .                    REFVAL, ADJUST, STEP,   CNFINE,
     .                    MW,     NW,     WORK,   RESULT  )
      IMPLICIT NONE

C$ Abstract
C
C     Determine time intervals over which a specified constraint on
C     observer-target distance is met.
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

      INCLUDE               'gf.inc'
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
C     CNVTOL     P   Convergence tolerance
C     NWDIST     P   Number of workspace windows for distance search.
C     TARGET     I   Name of the target body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     RELATE     I   Relational operator.
C     REFVAL     I   Reference value.
C     ADJUST     I   Adjustment value for absolute extrema searches.
C     STEP       I   Step size used for locating extrema and roots.
C     CNFINE     I   SPICE window to which the search is confined.
C     MW         I   Workspace window size.
C     NW         I   Workspace window count.
C     WORK      I-O   Array of workspace windows.
C     RESULT    I-O   SPICE window containing results.
C
C$ Detailed_Input
C
C     TARGET      is the name of a target body. Optionally, you may
C                 supply the integer ID code for the object as an
C                 integer string.  For example both 'MOON' and '301'
C                 are legitimate strings that indicate the moon is the
C                 target body.
C
C                 The target and observer define a position vector which
C                 points from the observer to the target; the length of
C                 this vector is the "distance" that serves as the
C                 subject of the search performed by this routine.
C
C                 Case and leading or trailing blanks are not
C                 significant in the string TARGET.
C
C
C     ABCORR      indicates the aberration corrections to be applied to
C                 the observer-target position vector to account for
C                 one-way light time and stellar aberration.
C
C                 Any aberration correction accepted by the SPICE
C                 routine SPKEZR is accepted here. See the header
C                 of SPKEZR for a detailed description of the
C                 aberration correction options. For convenience,
C                 the options are listed below:
C
C                    'NONE'     Apply no correction.
C
C                    'LT'       "Reception" case:  correct for
C                               one-way light time using a Newtonian
C                               formulation.
C
C                    'LT+S'     "Reception" case:  correct for
C                               one-way light time and stellar
C                               aberration using a Newtonian
C                               formulation.
C
C                    'CN'       "Reception" case:  converged
C                               Newtonian light time correction.
C
C                    'CN+S'     "Reception" case:  converged
C                               Newtonian light time and stellar
C                               aberration corrections.
C
C                    'XLT'      "Transmission" case:  correct for
C                               one-way light time using a Newtonian
C                               formulation.
C
C                    'XLT+S'    "Transmission" case:  correct for
C                               one-way light time and stellar
C                               aberration using a Newtonian
C                               formulation.
C
C                    'XCN'      "Transmission" case:  converged
C                               Newtonian light time correction.
C
C                    'XCN+S'    "Transmission" case:  converged
C                               Newtonian light time and stellar
C                               aberration corrections.
C
C                 Case and leading or trailing blanks are not
C                 significant in the string ABCORR.
C
C
C     OBSRVR      is the name of an observing body.  Optionally, you
C                 may supply the ID code of the object as an integer
C                 string. For example, both 'EARTH' and '399' are
C                 legitimate strings to supply to indicate the
C                 observer is Earth.
C
C                 Case and leading or trailing blanks are not
C                 significant in the string OBSRVR.
C
C
C     RELATE      is a relational operator used to define a constraint
C                 on the observer-target distance. The result window
C                 found by this routine indicates the time intervals
C                 where the constraint is satisfied. Supported values
C                 of RELATE and corresponding meanings are shown below:
C
C                    '>'      Distance is greater than the reference
C                             value REFVAL.
C
C                    '='      Distance is equal to the reference
C                             value REFVAL.
C
C                    '<'      Distance is less than the reference
C                             value REFVAL.
C
C
C                   'ABSMAX'  Distance is at an absolute maximum.
C
C                   'ABSMIN'  Distance is at an absolute  minimum.
C
C                   'LOCMAX'  Distance is at a local maximum.
C
C                   'LOCMIN'  Distance is at a local minimum.
C
C                The caller may indicate that the region of interest is
C                the set of time intervals where the distance is within
C                a specified offset relative to an absolute extremum.
C                The argument ADJUST (described below) is used to
C                specify this offset.
C
C                Local extrema are considered to exist only in the
C                interiors of the intervals comprising the confinement
C                window:  a local extremum cannot exist at a boundary
C                point of the confinement window.
C
C                Case and leading or trailing blanks are not
C                significant in the string RELATE.
C
C
C     REFVAL     is the reference value used together with the argument
C                RELATE to define an equality or inequality to be
C                satisfied by the distance between the specified target
C                and observer. See the discussion of RELATE above for
C                further information.
C
C                The units of REFVAL are km.
C
C
C     ADJUST     is a parameter used to modify searches for absolute
C                extrema: when RELATE is set to ABSMAX or ABSMIN and
C                ADJUST is set to a positive value, GFDIST
C                will find times when the observer-target distance is
C                within ADJUST km of the specified extreme value.
C
C                If ADJUST is non-zero and a search for an absolute
C                minimum AMIN is performed, the result window contains
C                time intervals when the observer-target distance has
C                values between AMIN and AMIN + ADJUST.
C
C                If the search is for an absolute maximum AMAX, the
C                corresponding range is  between AMAX - ADJUST and
C                AMAX.
C
C                ADJUST is not used for searches for local extrema,
C                equality or inequality conditions.
C
C
C     STEP       is the step size to be used in the search. STEP must
C                be shorter than any maximal time interval on which the
C                specified distance function is monotone increasing or
C                decreasing. That is, if the confinement window is
C                partitioned into alternating intervals on which the
C                distance function is either monotone increasing or
C                decreasing, STEP must be shorter than any of these
C                intervals.
C
C                However, STEP must not be *too* short, or the search
C                will take an unreasonable amount of time.
C
C                The choice of STEP affects the completeness but not
C                the precision of solutions found by this routine; the
C                precision is controlled by the convergence tolerance.
C                See the discussion of the parameter CNVTOL for
C                details.
C
C                STEP has units of TDB seconds.
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
C
C     MW         is a parameter specifying the length of the SPICE
C                windows in the workspace array WORK (see description
C                below) used by this routine.
C
C                MW should be set to a number at least twice as large
C                as the maximum number of intervals required by any
C                workspace window. In many cases, it's not necessary to
C                compute an accurate estimate of how many intervals are
C                needed; rather, the user can pick a size considerably
C                larger than what's really required.
C
C                However, since excessively large arrays can prevent
C                applications from compiling, linking, or running
C                properly, sometimes MW must be set according to
C                the actual workspace requirement. A rule of thumb
C                for the number of intervals NINTVLS needed is
C
C                    NINTVLS  =  2*N  +  ( M / STEP )
C
C                where
C
C                    N     is the number of intervals in the confinement
C                          window
C
C                    M     is the measure of the confinement window, in
C                          units of seconds
C
C                    STEP  is the search step size in seconds
C
C                MW should then be set to
C
C                    2 * NINTVLS
C
C
C     NW         is a parameter specifying the number of SPICE windows
C                in the workspace array WORK (see description below)
C                used by this routine. NW should be set to the
C                parameter NWDIST; this parameter is declared in the
C                include file gf.inc. (The reason this dimension is
C                an input argument is that this allows run-time
C                error checking to be performed.)
C
C
C     WORK       is an array used to store workspace windows. This
C                array should be declared by the caller as shown:
C
C                   INCLUDE 'gf.inc'
C                      ...
C
C                   DOUBLE PRECISION    WORK ( LBCELL : MW, NWDIST )
C
C                where MW is a constant declared by the caller and
C                NWDIST is a constant defined in the SPICELIB INCLUDE
C                file gf.inc. See the discussion of MW above.
C
C                WORK need not be initialized by the caller.
C
C$ Detailed_Output
C
C     WORK       is the input workspace array, modified by this
C                routine. The caller should re-initialize this array
C                before attempting to use it for any other purpose.
C
C
C     RESULT     is the window of intervals, contained within the
C                confinement window CNFINE, on which the specified
C                constraint is satisfied.
C
C                The endpoints of the time intervals comprising RESULT
C                are interpreted as seconds past J2000 TDB.
C
C                If RESULT is non-empty on input, its contents
C                will be discarded before GFDIST conducts its
C                search.
C
C$ Parameters
C
C     LBCELL     is the lower bound for SPICE Cell arrays.
C
C     CNVTOL     is the convergence tolerance used for finding
C                endpoints of the intervals comprising the result
C                window. CNVTOL is also used for finding intermediate
C                results; in particular, CNVTOL is used for finding the
C                windows on which the specified distance is increasing
C                or decreasing. CNVTOL is used to determine when binary
C                searches for roots should terminate: when a root is
C                bracketed within an interval of length CNVTOL; the
C                root is considered to have been found.
C
C                The accuracy, as opposed to precision, of roots found
C                by this routine depends on the accuracy of the input
C                data. In most cases, the accuracy of solutions will be
C                inferior to their precision.
C
C     NWDIST     is the number of workspace windows required by
C                this routine.
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
C     3)  If an error (typically cell overflow) occurs while performing
C         window arithmetic, the error will be diagnosed by a routine
C         in the call tree of this routine.
C
C     4)  If the relational operator RELATE is not recognized, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     5)  If the aberration correction specifier contains an
C         unrecognized value, an error is signaled by a routine in the
C         call tree of this routine
C
C     6)  If ADJUST is negative, an error is signaled by a routine in
C         the call tree of this routine.
C
C     7)  If either of the input body names do not map to NAIF ID
C         codes, an error is signaled by a routine in the call tree of
C         this routine.
C
C     8)  If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C     9)  If the window size MW is less than 2, the error
C         SPICE(INVALIDDIMENSION) will be signaled.
C
C     10) If the window count NW is less than NWDIST, the error
C         SPICE(INVALIDDIMENSION) will be signaled.
C
C     11) If the output SPICE window RESULT has insufficient capacity
C         to contain the number of intervals on which the specified
C         distance condition is met, the error will be diagnosed
C         by a routine in the call tree of this routine. If the result
C         window has size less than 2, the error SPICE(INVALIDDIMENSION)
C         will be signaled by this routine.
C
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target and observer for the
C          time period defined by the confinement window must be
C          loaded. If aberration corrections are used, the states of
C          target and observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C          Typically ephemeris data are made available by loading one
C          or more SPK files via FURNSH.
C
C        - If non-inertial reference frames are used, then PCK
C          files, frame kernels, C-kernels, and SCLK kernels may be
C          needed.
C
C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when the distance between the
C     specified target and observer satisfies a caller-specified
C     constraint. The resulting set of intervals is returned as a SPICE
C     window.
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
C     distance function is monotone increasing and monotone
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
C     The monotone windows (described above) are found via a two-step
C     search process. Each interval of the confinement window is
C     searched as follows: first, the input step size is the time
C     separation at which the sign of the rate of change of distance
C     ("range rate") is sampled. Starting at the left endpoint of the
C     interval, samples will be taken at each step. If a change of sign
C     is found, a root has been bracketed; at that point, the time at
C     which the range rate is zero can be found by a refinement
C     process, for example, via binary search.
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
C     to locate them. "Roots" include times when extrema are attained
C     and times when the distance function is equal to a reference
C     value or adjusted extremum. All endpoints of the intervals
C     comprising the result window are either endpoints of intervals of
C     the confinement window or roots.
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
C     slow search of interest must be performed. See the "CASCADE"
C     example program in gf.req for a demonstration.
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
C
C     1) Find times during the first three months of the year 2007 when
C        the geometric Earth-Moon distance is greater than 400000 km.
C        Display the start and stop times of the time intervals over
C        which this constraint is met, along with the Earth-Moon
C        distance at each interval endpoint.
C
C        We expect the Earth-Moon distance to be an oscillatory
C        function with extrema roughly two weeks apart. Using
C        a step size of one day guarantees that the GF system
C        won't fail to find any distance extrema. (Recall that a
C        search for distance extrema is an intermediate step
C        in the GF search process.)
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
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
C              pck00008.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
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
C           End of meta-kernel
C
C
C
C        Example code begins here.
C
C
C           PROGRAM EX1
C           IMPLICIT NONE
C     C
C     C     Include GF parameter declarations:
C     C
C           INCLUDE 'gf.inc'
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      SPD
C           DOUBLE PRECISION      VNORM
C           INTEGER               WNCARD
C     C
C     C     Local parameters
C     C
C           INTEGER               LBCELL
C           PARAMETER           ( LBCELL = -5 )
C     C
C     C     Use the parameter MAXWIN for both
C     C     the result window size and the workspace
C     C     size.
C     C
C           INTEGER               MAXWIN
C           PARAMETER           ( MAXWIN = 20000 )
C     C
C     C     Length of output time string:
C     C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 26 )
C     C
C     C     Local variables
C     C
C           CHARACTER*(TIMLEN)    TIMSTR
C
C           DOUBLE PRECISION      ADJUST
C           DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C           DOUBLE PRECISION      DIST
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      ET1
C           DOUBLE PRECISION      FINISH
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      POS    ( 3 )
C           DOUBLE PRECISION      REFVAL
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      START
C           DOUBLE PRECISION      STEP
C           DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWDIST )
C
C           INTEGER               I
C
C     C
C     C     Load kernels.
C     C
C           CALL FURNSH ( 'standard.tm' )
C     C
C     C     Initialize windows.
C     C
C           CALL SSIZED ( MAXWIN, RESULT )
C           CALL SSIZED ( 2,      CNFINE )
C     C
C     C     Store the time bounds of our search interval in
C     C     the confinement window.
C     C
C           CALL STR2ET ( '2007 JAN 1', ET0 )
C           CALL STR2ET ( '2007 APR 1', ET1 )
C
C           CALL WNINSD ( ET0, ET1, CNFINE )
C     C
C     C     Search using a step size of 1 day (in units of
C     C     seconds). The reference value is 450000 km.
C     C     We're not using the adjustment feature, so
C     C     we set ADJUST to zero.
C     C
C           STEP   = SPD()
C           REFVAL = 4.D5
C           ADJUST = 0.D0
C
C     C
C     C     Perform the search. The set of times when the
C     C     constraint is met will be stored in the SPICE
C     C     window RESULT.
C     C
C           CALL GFDIST ( 'MOON', 'NONE', 'EARTH', '>',
C          .              REFVAL, ADJUST, STEP,    CNFINE,
C          .              MAXWIN, NWDIST, WORK,    RESULT )
C     C
C     C     Display the results.
C     C
C           IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C              WRITE (*, '(A)') 'Result window is empty.'
C           ELSE
C              DO I = 1, WNCARD(RESULT)
C     C
C     C           Fetch the endpoints of the Ith interval
C     C           of the result window.
C     C
C                 CALL WNFETD ( RESULT, I, START, FINISH )
C     C
C     C           Check the distance at the start and stop times.
C     C
C                 CALL SPKPOS ( 'MOON',  START, 'J2000', 'NONE',
C          .                    'EARTH', POS,   LT              )
C                 DIST = VNORM(POS)
C
C                 CALL TIMOUT ( START, 'YYYY-MON-DD HR:MN:SC.###',
C          .                    TIMSTR                            )
C
C                 WRITE (*, '(A,F16.9)' ) 'Start time, distance = '//
C          .                              TIMSTR, DIST
C
C                 CALL SPKPOS ( 'MOON',  FINISH, 'J2000', 'NONE',
C          .                    'EARTH', POS,     LT              )
C                 DIST = VNORM(POS)
C
C                 CALL TIMOUT ( FINISH, 'YYYY-MON-DD HR:MN:SC.###',
C          .                    TIMSTR                            )
C
C                 WRITE (*, '(A,F16.9)' ) 'Stop time,  distance = '//
C          .                              TIMSTR, DIST
C              END DO
C
C           END IF
C           END
C
C
C
C        When this program was executed on a PC/Linux/g77 platform, the
C        output was:
C
C
C    Start time, distance = 2007-JAN-08 00:10:02.439  399999.999999989
C    Stop time,  distance = 2007-JAN-13 06:36:42.770  400000.000000010
C    Start time, distance = 2007-FEB-04 07:01:30.094  399999.999999990
C    Stop time,  distance = 2007-FEB-10 09:29:56.659  399999.999999998
C    Start time, distance = 2007-MAR-03 00:19:19.998  400000.000000006
C    Stop time,  distance = 2007-MAR-10 14:03:33.312  400000.000000007
C    Start time, distance = 2007-MAR-29 22:52:52.961  399999.999999995
C    Stop time,  distance = 2007-APR-01 00:00:00.000  404531.955232216
C
C        Note that at the final solutions interval's stop time is not
C        close to the reference value of 400000 km. This is because the
C        interval's stop time was determined by the stop time of the
C        confinement window.
C
C
C
C     2) Extend the first example to demonstrate use of all supported
C        relational operators. Find times when
C
C           Earth-Moon distance is = 400000 km
C           Earth-Moon distance is < 400000 km
C           Earth-Moon distance is > 400000 km
C           Earth-Moon distance is at a local minimum
C           Earth-Moon distance is at a absolute minimum
C           Earth-Moon distance is > the absolute minimum + 100 km
C           Earth-Moon distance is at a local maximum
C           Earth-Moon distance is at a absolute maximum
C           Earth-Moon distance is > the absolute maximum - 100 km
C
C        To shorten the search time and output, use the
C        shorter search interval
C
C           2007 JAN 15 00:00:00 UTC  to
C           2007 MAR 15 00:00:00 UTC
C
C        As before, use geometric (uncorrected) positions, so
C        set the aberration correction flag to 'NONE'.
C
C        Use the meta-kernel from the first example.
C
C
C        Example code begins here.
C
C
C           PROGRAM EX2
C           IMPLICIT NONE
C     C
C     C     Include GF parameter declarations:
C     C
C           INCLUDE 'gf.inc'
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      SPD
C           DOUBLE PRECISION      VNORM
C           INTEGER               WNCARD
C     C
C     C     Local parameters
C     C
C           INTEGER               LBCELL
C           PARAMETER           ( LBCELL = -5 )
C     C
C     C     Use the parameter MAXWIN for both
C     C     the result window size and the workspace
C     C     size.
C     C
C           INTEGER               MAXWIN
C           PARAMETER           ( MAXWIN = 20000 )
C     C
C     C     Length of output time string:
C     C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 26 )
C
C     C
C     C     Number of relational operators:
C     C
C           INTEGER               NRELOP
C           PARAMETER           ( NRELOP = 9 )
C
C     C
C     C     Operator name length:
C     C
C           INTEGER               OPNMLN
C           PARAMETER           ( OPNMLN = 6 )
C
C     C
C     C     Output line length:
C     C
C           INTEGER               LNSIZE
C           PARAMETER           ( LNSIZE = 80 )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(OPNMLN)    RELATE ( NRELOP )
C           CHARACTER*(LNSIZE)    TEMPLT ( NRELOP )
C           CHARACTER*(TIMLEN)    TIMSTR
C           CHARACTER*(LNSIZE)    TITLE
C
C           DOUBLE PRECISION      ADJUST ( NRELOP )
C           DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C           DOUBLE PRECISION      DIST
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      ET1
C           DOUBLE PRECISION      FINISH
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      POS    ( 3 )
C           DOUBLE PRECISION      REFVAL
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      START
C           DOUBLE PRECISION      STEP
C           DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWDIST )
C
C           INTEGER               I
C           INTEGER               J
C
C     C
C     C     Saved variables
C     C
C           SAVE                  ADJUST
C           SAVE                  RELATE
C           SAVE                  TEMPLT
C
C     C
C     C     Initial values
C     C
C           DATA                  ADJUST / 0.D0,
C          .                               0.D0,
C          .                               0.D0,
C          .                               0.D0,
C          .                               0.D0,
C          .                               100.D0,
C          .                               0.D0,
C          .                               0.D0,
C          .                               100.D0 /
C
C           DATA                  RELATE / '=',
C          .                               '<',
C          .                               '>',
C          .                               'LOCMIN',
C          .                               'ABSMIN',
C          .                               'ABSMIN',
C          .                               'LOCMAX',
C          .                               'ABSMAX',
C          .                               'ABSMAX'  /
C
C           DATA                  TEMPLT /
C          .      'Condition: distance = # km',
C          .      'Condition: distance < # km',
C          .      'Condition: distance > # km',
C          .      'Condition: distance is a local minimum',
C          .      'Condition: distance is the absolute minimum',
C          .      'Condition: distance < the absolute minimum + * km',
C          .      'Condition: distance is a local maximum',
C          .      'Condition: distance is the absolute maximum',
C          .      'Condition: distance > the absolute maximum - * km' /
C
C     C
C     C     Load kernels.
C     C
C           CALL FURNSH ( 'standard.tm' )
C     C
C     C     Initialize windows.
C     C
C           CALL SSIZED ( MAXWIN, RESULT )
C           CALL SSIZED ( 2,      CNFINE )
C     C
C     C     Store the time bounds of our search interval in
C     C     the confinement window.
C     C
C           CALL STR2ET ( '2007 JAN 15', ET0 )
C           CALL STR2ET ( '2007 MAR 15', ET1 )
C
C           CALL WNINSD ( ET0, ET1, CNFINE )
C
C     C
C     C     Search using a step size of 1 day (in units of
C     C     seconds). Use a reference value of 400000 km.
C     C
C           STEP   = SPD()
C           REFVAL = 4.D5
C
C           DO I = 1, NRELOP
C
C              CALL GFDIST ( 'MOON', 'NONE',    'EARTH', RELATE(I),
C          .                 REFVAL, ADJUST(I), STEP,    CNFINE,
C          .                 MAXWIN, NWDIST,    WORK,    RESULT    )
C     C
C     C        Display the results.
C     C
C              WRITE (*,*) ' '
C
C     C
C     C        Substitute the reference and adjustment values,
C     C        where applicable, into the title string:
C     C
C              CALL REPMD ( TEMPLT(I), '#', REFVAL,    6, TITLE )
C              CALL REPMD ( TITLE,     '*', ADJUST(I), 6, TITLE )
C
C              WRITE (*, '(A)' ) TITLE
C
C              IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C                 WRITE (*, '(A)' ) ' Result window is empty.'
C              ELSE
C                 WRITE (*, '(A)' ) ' Result window:'
C
C                 DO J = 1, WNCARD(RESULT)
C     C
C     C              Fetch the endpoints of the Jth interval
C     C              of the result window.
C     C
C                    CALL WNFETD ( RESULT, J, START, FINISH )
C     C
C     C              Check the distance at the start and stop times.
C     C
C                    CALL SPKPOS ( 'MOON',  START, 'J2000', 'NONE',
C          .                       'EARTH', POS,   LT              )
C                    DIST = VNORM(POS)
C
C                    CALL TIMOUT ( START, 'YYYY-MON-DD HR:MN:SC.###',
C          .                       TIMSTR                            )
C
C                    WRITE (*, '(A,F16.9)' ) '  Start time, distance = '
C          .         //                      TIMSTR, DIST
C
C                    CALL SPKPOS ( 'MOON',  FINISH, 'J2000', 'NONE',
C          .                       'EARTH', POS,     LT              )
C                    DIST = VNORM(POS)
C
C                    CALL TIMOUT ( FINISH, 'YYYY-MON-DD HR:MN:SC.###',
C          .                       TIMSTR                            )
C
C                    WRITE (*, '(A,F16.9)' ) '  Stop time,  distance = '
C          .         //                      TIMSTR, DIST
C                 END DO
C
C              END IF
C
C           END DO
C
C           WRITE (*,*) ' '
C
C           END
C
C
C        When this program was executed on a PC/Linux/g77 platform, the
C        output was:
C
C
C    Condition: distance = 4.00000E+05 km
C     Result window:
C      Start time, distance = 2007-FEB-04 07:01:30.094  399999.999999998
C      Stop time,  distance = 2007-FEB-04 07:01:30.094  399999.999999998
C      Start time, distance = 2007-FEB-10 09:29:56.659  399999.999999989
C      Stop time,  distance = 2007-FEB-10 09:29:56.659  399999.999999989
C      Start time, distance = 2007-MAR-03 00:19:19.998  399999.999999994
C      Stop time,  distance = 2007-MAR-03 00:19:19.998  399999.999999994
C      Start time, distance = 2007-MAR-10 14:03:33.312  400000.000000000
C      Stop time,  distance = 2007-MAR-10 14:03:33.312  400000.000000000
C
C    Condition: distance < 4.00000E+05 km
C     Result window:
C      Start time, distance = 2007-JAN-15 00:00:00.000  393018.609906208
C      Stop time,  distance = 2007-FEB-04 07:01:30.094  399999.999999990
C      Start time, distance = 2007-FEB-10 09:29:56.659  399999.999999998
C      Stop time,  distance = 2007-MAR-03 00:19:19.998  400000.000000006
C      Start time, distance = 2007-MAR-10 14:03:33.312  400000.000000010
C      Stop time,  distance = 2007-MAR-15 00:00:00.000  376255.453934464
C
C    Condition: distance > 4.00000E+05 km
C     Result window:
C      Start time, distance = 2007-FEB-04 07:01:30.094  399999.999999990
C      Stop time,  distance = 2007-FEB-10 09:29:56.659  399999.999999998
C      Start time, distance = 2007-MAR-03 00:19:19.998  400000.000000006
C      Stop time,  distance = 2007-MAR-10 14:03:33.312  400000.000000010
C
C    Condition: distance is a local minimum
C     Result window:
C      Start time, distance = 2007-JAN-22 12:30:49.458  366925.804109350
C      Stop time,  distance = 2007-JAN-22 12:30:49.458  366925.804109350
C      Start time, distance = 2007-FEB-19 09:36:29.968  361435.646812061
C      Stop time,  distance = 2007-FEB-19 09:36:29.968  361435.646812061
C
C    Condition: distance is the absolute minimum
C     Result window:
C      Start time, distance = 2007-FEB-19 09:36:29.968  361435.646812061
C      Stop time,  distance = 2007-FEB-19 09:36:29.968  361435.646812061
C
C    Condition: distance < the absolute minimum + 1.00000E+02 km
C     Result window:
C      Start time, distance = 2007-FEB-19 01:09:52.706  361535.646812062
C      Stop time,  distance = 2007-FEB-19 18:07:45.136  361535.646812061
C
C    Condition: distance is a local maximum
C     Result window:
C      Start time, distance = 2007-FEB-07 12:38:29.870  404992.424288620
C      Stop time,  distance = 2007-FEB-07 12:38:29.870  404992.424288620
C      Start time, distance = 2007-MAR-07 03:37:02.122  405853.452130754
C      Stop time,  distance = 2007-MAR-07 03:37:02.122  405853.452130754
C
C    Condition: distance is the absolute maximum
C     Result window:
C      Start time, distance = 2007-MAR-07 03:37:02.122  405853.452130754
C      Stop time,  distance = 2007-MAR-07 03:37:02.122  405853.452130754
C
C    Condition: distance > the absolute maximum - 1.00000E+02 km
C     Result window:
C      Start time, distance = 2007-MAR-06 15:56:00.957  405753.452130753
C      Stop time,  distance = 2007-MAR-07 15:00:38.674  405753.452130753
C
C
C$ Restrictions
C
C     1) The kernel files to be used by this routine must be loaded
C        (normally via the SPICELIB routine FURNSH) before this routine
C        is called.
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
C-    SPICELIB Version 1.0.0, 15-APR-2009 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     GF distance search
C
C-&


C
C     SPICELIB functions
C
      INTEGER               SIZED

      LOGICAL               RETURN
      LOGICAL               GFBAIL

C
C     External functions
C

C
C     Interrupt indicator function:
C
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

      CALL CHKIN ( 'GFDIST' )

C
C     Check the workspace window dimensions.
C
      IF ( MW .LT. 2 ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2.'                               )
         CALL ERRINT ( '#',  MW                                    )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFDIST'                                    )
         RETURN

      END IF

      IF ( NW .LT. NWDIST ) THEN

         CALL SETMSG ( 'Workspace window count was #; count must be '
     .   //            'at least #.'                                 )
         CALL ERRINT ( '#',  NW                                      )
         CALL ERRINT ( '#',  NWDIST                                  )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                     )
         CALL CHKOUT ( 'GFDIST'                                      )
         RETURN

      END IF

C
C     Check the result window size.
C
      IF ( SIZED(RESULT) .LT. 2 ) THEN

         CALL SETMSG ( 'Result window size was #; size must be '
     .   //            'at least 2.'                             )
         CALL ERRINT ( '#',  SIZED(RESULT)                       )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                 )
         CALL CHKOUT ( 'GFDIST'                                  )
         RETURN

      END IF

C
C     Set up a call to GFEVNT, which will handle the search.
C
      QPNAMS(1) = 'TARGET'
      QCPARS(1) =  TARGET

      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) =  OBSRVR

      QPNAMS(3) = 'ABCORR'
      QCPARS(3) =  ABCORR

C
C     Check and set the step size.
C
      CALL GFSSTP ( STEP )

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
C     Initialize the RESULT window.
C
      CALL SCARDD ( 0, RESULT )

C
C     Look for solutions.
C
C     Progress report and bail-out options are set to .FALSE.
C
      CALL GFEVNT ( GFSTEP,   GFREFN,   'DISTANCE',  QNPARS,
     .              QPNAMS,   QCPARS,   QDPARS,      QIPARS,
     .              QLPARS,   RELATE,   REFVAL,      TOL,
     .              ADJUST,   CNFINE,   NORPT,       GFREPI,
     .              GFREPU,   GFREPF,   MW,          NWDIST,
     .              WORK,     NOBAIL,   GFBAIL,      RESULT  )

      CALL CHKOUT ( 'GFDIST' )
      RETURN
      END




