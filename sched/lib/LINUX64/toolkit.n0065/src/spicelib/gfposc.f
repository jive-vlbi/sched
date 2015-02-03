C$Procedure GFPOSC (GF, observer-target vector coordinate search )

      SUBROUTINE GFPOSC ( TARGET, FRAME,  ABCORR, OBSRVR,
     .                    CRDSYS, COORD,  RELATE, REFVAL,
     .                    ADJUST, STEP,   CNFINE, MW,
     .                    NW,     WORK,   RESULT )

C$ Abstract
C
C     Determine time intervals for which a coordinate of an
C     observer-target position vector satisfies a numerical constraint.
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
C     SPK
C     CK
C     TIME
C     WINDOWS
C
C$ Keywords
C
C     COORDINATE
C     GEOMETRY
C     SEARCH
C     EVENT
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         FRAME
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         CRDSYS
      CHARACTER*(*)         COORD
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
C     FRAME      I   Name of the reference frame for coordinate
C                    calculations.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     CRDSYS     I   Name of the coordinate system containing COORD.
C     COORD      I   Name of the coordinate of interest.
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
C              The target and observer define a position vector
C              that points from the observer to the target.
C
C     FRAME    the string name of the reference frame in which to
C              perform state look-ups and coordinate calculations.
C
C              The SPICE frame subsystem must recognize the FRAME
C              name.
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
C              The ABCORR string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     OBSRVR   the string name of an observing body.  Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to indicate the observer as Earth.
C
C     CRDSYS   the string name of the coordinate system for which the
C              coordinate of interest is a member
C
C     COORD    the string name of the coordinate of interest in CRDSYS
C
C              The supported coordinate systems and coordinate names:
C
C          Coordinate System (CRDSYS)   Coordinates (COORD)  Range
C
C          'RECTANGULAR'              'X'
C                                     'Y'
C                                     'Z'
C
C          'LATITUDINAL'              'RADIUS'
C                                     'LONGITUDE'        (-Pi,Pi]
C                                     'LATITUDE'         [-Pi/2,Pi/2]
C
C          'RA/DEC'                   'RANGE'
C                                     'RIGHT ASCENSION'  [0,2Pi)
C                                     'DECLINATION'      [-Pi/2,Pi/2]
C
C          'SPHERICAL'                'RADIUS'
C                                     'COLATITUDE'       [0,Pi]
C                                     'LONGITUDE'        (-Pi,Pi]
C
C          'CYLINDRICAL'              'RADIUS'
C                                     'LONGITUDE'        [0,2Pi)
C                                     'Z'
C
C          'GEODETIC'                 'LONGITUDE'        (-Pi,Pi]
C                                     'LATITUDE'         [-Pi/2,Pi/2]
C                                     'ALTITUDE'
C
C          'PLANETOGRAPHIC'           'LONGITUDE'        [0,2Pi)
C                                     'LATITUDE'         [-Pi/2,Pi/2]
C                                     'ALTITUDE'
C
C                                      The ALTITUDE coordinates have a
C                                      constant value of zero +/-
C                                      roundoff for ellipsoid targets.
C
C          Limit those searches for coordinate events in the GEODETIC
C          and PLANETOGRAPHIC coordinate systems to TARGET bodies with
C          axial symmetry in the equatorial plane, i.e. equality of
C          the body X and Y radii (oblate or prolate spheroids).
C
C          Searches on GEODETIC or PLANETOGRAPHIC coordinates requires
C          body shape data, and in the case of PLANETOGRAPHIC
C          coordinates, body rotation data.
C
C          The body associated to GEODETIC or PLANETOGRAPHIC
C          coordinates is the body center of FRAME.
C
C     RELATE   the string or character describing the relational
C              operator used to define a constraint on the selected
C              coordinate of the observer-target vector. The result
C              window found by this routine indicates the time intervals
C              where the constraint is satisfied. Supported values of
C              RELATE and corresponding meanings are shown below:
C
C                 '>'       The coordinate value is greater than the
C                           reference value REFVAL.
C
C                 '='       The coordinate value is equal to the
C                           reference value REFVAL.
C
C                 '<'       The coordinate value is less than the
C                           reference value REFVAL.
C
C                 'ABSMAX'  The coordinate value is at an absolute
C                           maximum.
C
C                 'ABSMIN'  The coordinate value is at an absolute
C                           minimum.
C
C                 'LOCMAX'  The coordinate value is at a local
C                           maximum.
C
C                 'LOCMIN'  The coordinate value is at a local
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
C              The RELATE string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     REFVAL   the double precision reference value used together with
C              the argument RELATE to define an equality or inequality
C              to satisfy by the selected coordinate of the observer-
C              target vector. See the discussion of RELATE above for
C              further information.
C
C              The units of REFVAL correspond to the type as defined
C              by COORD, radians for angular measures, kilometers for
C              distance measures.
C
C     ADJUST   a double precision value used to modify searches for
C              absolute extrema: when RELATE is set to ABSMAX or ABSMIN
C              and ADJUST is set to a positive value, GFPOSC finds
C              times when the position vector coordinate is within
C              ADJUST radians/kilometers of the specified extreme value.
C
C              For RELATE set to ABSMAX, the RESULT window contains
C              time intervals when the position vector coordinate has
C              values between ABSMAX - ADJUST and ABSMAX.
C
C              For RELATE set to ABSMIN, the RESULT window contains
C              time intervals when the position vector coordinate has
C              values between ABSMIN and ABSMIN + ADJUST.
C
C              ADJUST is not used for searches for local extrema,
C              equality or inequality conditions.
C
C     STEP     the double precision time step size to use in the search.
C
C              STEP must be short enough to for a search using this step
C              size to locate the time intervals where coordinate
C              function of the position vector is monotone increasing or
C              decreasing. However, STEP must not be *too* short, or
C              the search will take an unreasonable amount of time.
C
C              For coordinates other than LONGITUDE and RIGHT ASCENSION,
C              the step size must be shorter than the shortest interval,
C              within the confinement window, over which the coordinate
C              is monotone increasing or decreasing.
C
C              For LONGITUDE and RIGHT ASCENSION, the step size must
C              be shorter than the shortest interval, within the
C              confinement window, over which either the sin or cos
C              of the coordinate is monotone increasing or decreasing.
C
C              The choice of STEP affects the completeness but not
C              the precision of solutions found by this routine; the
C              precision is controlled by the convergence tolerance.
C              See the discussion of the parameter CNVTOL for
C              details.
C
C              STEP has units of seconds.
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
C              parameter NWMAX; this parameter is declared in the
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
C                 DOUBLE PRECISION    WORK ( LBCELL : MW, NWMAX )
C
C              where MW is a constant declared by the caller and
C              NWMAX is a constant defined in the SPICELIB INCLUDE
C              file gf.inc. See the discussion of MW above.
C
C              WORK need not be initialized by the caller.
C
C     RESULT   a double precision SPICE window which will contain the
C              search results. RESULT must be initialized using
C              a call to SSIZED. RESULT must be declared and initialized
C              with sufficient size to capture the full set of time
C              intervals within the search region on which the specified
C              constraint is satisfied.
C
C              If RESULT is non-empty on input, its contents
C              will be discarded before GFPOSC conducts its
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
C              windows on which the specified coordinate is increasing
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
C     3)  If the window size MW is less than 2 or not an even value,
C         the error SPICE(INVALIDDIMENSION) will signal.
C
C     4)  If the window size of RESULT is less than 2, the error
C         SPICE(INVALIDDIMENSION) will signal.
C
C     5)  If an error (typically cell overflow) occurs during
C         window arithmetic, the error will be diagnosed by a routine
C         in the call tree of this routine.
C
C     6)  If the relational operator RELATE is not recognized, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     7)  If the size of the workspace is too small, an error is
C         signaled by a routine in the call tree of this routine.
C
C     8)  If ADJUST is negative, an error is signaled by a routine in
C         the call tree of this routine.
C
C     9)  If either of the input body names do not map to NAIF ID
C         codes, an error is signaled by a routine in the call tree of
C         this routine.
C
C     10) If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C     11) If the search uses GEODETIC or PLANETOGRAPHIC coordinates,
C         a routine in the call tree of this routine signals the
C         SPICE(NOTSUPPORTED) error if the center body of the reference
C         frame has unequal equatorial radii.
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
C     observer-target position vector coordinate value events.
C     Applications that require support for progress reporting,
C     interrupt handling, non-default step or refinement functions,
C     or non-default convergence tolerance should call GFEVNT rather
C     than this routine.
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when the selected coordinate of
C     the observer-target position vector satisfies a caller-specified
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
C     periods, within the confinement window, over which the specified
C     coordinate function is monotone increasing and monotone
C     decreasing. Each of these time periods is represented by a SPICE
C     window. Having found these windows, all of the coordinate
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
C     change of coordinate will be sampled. Starting at
C     the left endpoint of an interval, samples will be taken at each
C     step. If a change of sign is found, a root has been bracketed; at
C     that point, the time at which the time derivative of the
C     coordinate is zero can be found by a refinement process, for
C     example, using a binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the coordinate function is monotone:
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
C     attained and times when the coordinate function is equal to a
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
C     Practical use of the coordinate search capability would likely
C     consist of searches over multiple coordinate constraints to find
C     time intervals that satisfies the constraints. An
C     effective technique to accomplish such a search is
C     to use the result window from one search as the confinement window
C     of the next.
C
C     Longitude and Right Ascension
C     =============================
C
C     The cyclic nature of the longitude and right ascension coordinates
C     produces branch cuts at +/- 180 degrees longitude and 0-360
C     right ascension. Round-off error may cause solutions near these
C     branches to cross the branch. Use of the SPICE routine WNCOND
C     will contract solution windows by some epsilon, reducing the
C     measure of the windows and eliminating the branch crossing. A
C      one millisecond contraction will in most cases eliminate
C      numerical round-off caused branch crossings.
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
C     Find the time during 2007 for which the latitude of the
C     Earth-Sun vector in IAU_EARTH frame has the maximum value,
C     i.e. the latitude of the Tropic of Cancer.
C
C           PROGRAM  GFPOSC_T
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
C           DOUBLE PRECISION      RPD
C           INTEGER               WNCARD
C
C     C
C     C     Local parameters
C     C
C           INTEGER               LBCELL
C           PARAMETER           ( LBCELL = -5 )
C
C     C
C     C     Use the parameter MAXWIN for both
C     C     the result window size and the workspace
C     C     size.
C     C
C           INTEGER               MAXWIN
C           PARAMETER           ( MAXWIN = 750 )
C
C     C
C     C     String length.
C     C
C           INTEGER               STRLEN
C           PARAMETER           ( STRLEN = 64 )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(STRLEN)    TIMSTR
C           CHARACTER*(STRLEN)    TIMFIN
C           CHARACTER*(STRLEN)    RELATE
C           CHARACTER*(STRLEN)    CRDSYS
C           CHARACTER*(STRLEN)    COORD
C           CHARACTER*(STRLEN)    ABCORR
C           CHARACTER*(STRLEN)    TARG
C           CHARACTER*(STRLEN)    OBSRVR
C           CHARACTER*(STRLEN)    FRAME
C           CHARACTER*(STRLEN)   TIMFMT
C
C
C           DOUBLE PRECISION      ADJUST
C           DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      ET1
C           DOUBLE PRECISION      FINISH
C           DOUBLE PRECISION      REFVAL
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      START
C           DOUBLE PRECISION      STEP
C           DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )
C
C           INTEGER               I
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
C           TIMFMT = 'YYYY-MON-DD HR:MN:SC.###### (TDB) ::TDB ::RND'
C
C     C
C     C     Store the time bounds of our search interval in
C     C     the confinement window.
C     C
C
C           CALL STR2ET ( '2007 JAN 1', ET0 )
C           CALL STR2ET ( '2008 JAN 1', ET1 )
C
C           CALL WNINSD ( ET0, ET1, CNFINE )
C
C     C
C     C     The latitude varies relatively slowly, ~46 degrees during
C     C     the year. The extrema occur approximately every six months.
C     C     Search using a step size less than half that value
C     C     (180 days). For this example use ninety days
C     C     (in units of seconds).
C     C
C           STEP   = SPD() * 90.D0
C           ADJUST = 0.D0
C           REFVAL = 0.D0
C
C     C
C     C     Search for the date on which the CRDSYS system
C     C     coordinate COORD satisfies the RELATE constraint.
C     C
C           RELATE = 'ABSMAX'
C           CRDSYS = 'LATITUDINAL'
C           COORD  = 'LATITUDE'
C           TARG   = 'SUN'
C           OBSRVR = 'EARTH'
C           FRAME  = 'IAU_EARTH'
C           ABCORR = 'NONE'
C
C
C     C
C     C     Perform this search using the geometric position
C     C     of the bodies; set the aberration correction to 'NONE'.
C     C
C           CALL GFPOSC ( TARG,   FRAME, ABCORR,
C          .              OBSRVR, CRDSYS, COORD,
C          .              RELATE, REFVAL, ADJUST,
C          .              STEP,   CNFINE,  MAXWIN,
C          .              NWMAX,  WORK,   RESULT )
C
C     C
C     C     Display the results.
C     C
C           IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C              WRITE (*,*) 'Result window is empty.'
C
C           ELSE
C
C              DO I = 1, WNCARD(RESULT)
C
C     C
C     C           Fetch the endpoints of the Ith interval
C     C           of the result window.
C     C
C                 CALL WNFETD ( RESULT, I, START, FINISH )
C
C                 IF( START .EQ. FINISH ) THEN
C
C     C
C     C              The result window contains singleton
C     C              intervals, so we need display only the
C     C              start times.
C     C
C                    CALL TIMOUT ( START, TIMFMT, TIMSTR )
C                    WRITE (*, *) 'Event time: ', TIMSTR
C
C                 ELSE
C
C                    CALL TIMOUT ( START,  TIMFMT, TIMSTR )
C                    CALL TIMOUT ( FINISH, TIMFMT, TIMFIN )
C
C                    WRITE(*, *) 'From : ', TIMSTR
C                    WRITE(*, *) 'To   : ', TIMFIN
C                    WRITE(*, *) ' '
C
C                 END IF
C
C              END DO
C
C           END IF
C
C           END
C
C      The program outputs:
C
C           Event time: 2007-JUN-21 17:54:13.166910 (TDB)
C
C     Example(2):
C
C        A minor modification of the program listed in Example 1; find
C        the time during 2007 for which the latitude of the Earth-Sun
C        vector in IAU_EARTH frame has the minimum value, i.e. the
C        latitude of the Tropic of Capricorn.
C
C        Edit the GFPOSC_T program, assign
C
C           RELATE = 'ABSMIN'
C
C        The program outputs:
C
C           Event time: 2007-DEC-22 06:04:32.630160 (TDB)
C
C     Example(3):
C
C        Find the time during 2007 for which the Z component of the
C        Earth-Sun vector in IAU_EARTH frame has value 0, i.e. crosses
C        the equatorial plane (this also defines a zero latitude).
C        The search should return two times, one for an ascending
C        passage and one for descending.
C
C        Edit the GFPOSC_T program above, assign:
C
C           RELATE = '='
C           CRDSYS = 'RECTANGULAR'
C           COORD  = 'Z'
C
C           Note, this RELATE operator refers to the REFVAL value,
C           assigned to 0.D0 for this example.
C
C        The program outputs:
C
C           Event time: 2007-MAR-21 00:01:25.495120 (TDB)
C           Event time: 2007-SEP-23 09:46:39.574123 (TDB)
C
C     Example(4):
C
C        Find the times between Jan 1, 2007 and Jan 1, 2008
C        corresponding to the apoapsis on the Moon's orbit around the
C        Earth (note, the GFDIST routine can also perform this search).
C
C        Edit the GFPOSC_T program above, assign:
C
C           This search requires a change in the step size since the
C           Moon's orbit about the earth (earth-moon barycenter) has a
C           twenty-eight day period. Use a step size something less
C           than half that value. In this case, we use twelve days.
C
C           STEP   = SPD() * 12.D0
C           RELATE = 'LOCMAX'
C           CRDSYS = 'SPHERICAL'
C           COORD  = 'RADIUS'
C           TARG   = 'MOON'
C           OBSRVR = 'EARTH'
C           FRAME  = 'J2000'
C
C        The program outputs:
C
C           Event time: 2007-JAN-10 16:26:18.805837 (TDB)
C           Event time: 2007-FEB-07 12:39:35.078525 (TDB)
C           Event time: 2007-MAR-07 03:38:07.334769 (TDB)
C           Event time: 2007-APR-03 08:38:55.222606 (TDB)
C           Event time: 2007-APR-30 10:56:49.847028 (TDB)
C           Event time: 2007-MAY-27 22:03:28.857783 (TDB)
C           Event time: 2007-JUN-24 14:26:23.639351 (TDB)
C           Event time: 2007-JUL-22 08:43:50.135565 (TDB)
C           Event time: 2007-AUG-19 03:28:33.538170 (TDB)
C           Event time: 2007-SEP-15 21:07:13.964698 (TDB)
C           Event time: 2007-OCT-13 09:52:30.819371 (TDB)
C           Event time: 2007-NOV-09 12:32:50.070555 (TDB)
C           Event time: 2007-DEC-06 16:54:31.225504 (TDB)
C
C     Example(5):
C
C        Find times between Jan 1, 2007 and Jan 1, 2008 when the
C        latitude (elevation) of the observer-target vector between
C        DSS 17 and the Moon, as observed in the DSS 17 topocentric
C        (station) frame, exceeds 83 degrees.
C
C        This search uses a step size of four hours since the time
C        for all declination zero-to-max-to-zero passes within
C        the search window exceeds eight hours.
C
C        This search requires kernels not included in the standard.tm
C        meta kernel.
C
C            Kernel name                      Contents
C            -----------                      --------
C            earthstns_itrf93_050714.bsp      SPK for DSN Station
C                                             Locations
C            earth_topo_050714.tf             Topocentric DSN stations
C                                             frame definitions
C            earth_000101_080120_071029.bpc   High precision earth PCK
C
C        Edit the GFPOSC_T program above, assign:
C
C            CALL FURNSH ('earthstns_itrf93_050714.bsp')
C            CALL FURNSH ('earth_topo_050714.tf')
C            CALL FURNSH ('earth_000101_080120_071029.bpc')
C
C            STEP   = SPD() * (4.D0/24.D0)
C            REFVAL = 83.D0 * RPD()
C            RELATE = '>'
C            CRDSYS = 'LATITUDINAL'
C            COORD  = 'LATITUDE'
C            TARG   = 'MOON'
C            OBSRVR = 'DSS-17'
C            FRAME  = 'DSS-17_TOPO'
C
C        The example uses an 83 degree elevation because of its rare
C        occurrence and short duration.
C
C     The program outputs:
C
C           From : 2007-FEB-26 03:18:48.229806 (TDB)
C           To   : 2007-FEB-26 03:31:29.734169 (TDB)
C
C           From : 2007-MAR-25 01:12:38.551183 (TDB)
C           To   : 2007-MAR-25 01:23:53.908601 (TDB)
C
C$ Restrictions
C
C     1) The kernel files to be used by this routine must be loaded
C        (normally using the SPICELIB routine FURNSH) before this
C        routine is called.
C
C     2) This routine has the side effect of re-initializing the
C        coordinate quantity utility package.  Callers may
C        need to re-initialize the package after calling this routine.
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
C        Header edits. COORD description to clarify the body with which
C        GEODETIC and PLANETOGRAPHIC coordinates are associated.
C        Clarified exception SPICE(NOTSUPPORTED) description.
C
C        Edits to Example section, proper description of "standard.tm"
C        meta kernel.
C
C-    SPICELIB Version 1.0.1, 10-JUN-2009 (NJB) (EDW)
C
C        Edited argument descriptions.
C
C-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     GF position coordinate search
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               GFBAIL
      INTEGER               SIZED
      LOGICAL               EVEN

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
      PARAMETER           ( QNPARS = 10 )

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
      CHARACTER*(LNSIZE)    DREF

      DOUBLE PRECISION      QDPARS ( QNPARS )
      DOUBLE PRECISION      DVEC   ( 3 )

      INTEGER               QIPARS ( QNPARS )

      LOGICAL               QLPARS ( QNPARS )

C
C     Define no-use values for DVEC and DREF
C
      DATA                  DVEC  / 0.D0, 0.D0, 0.D0 /
      DATA                  DREF  / ' '  /


      SAVE                  DVEC
      SAVE                  DREF

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Check into the error subsystem.
C
      CALL CHKIN ( 'GFPOSC' )

C
C     Confirm minimum window sizes.
C
      IF ( MW .LT. 2 .OR. .NOT. EVEN(MW) ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2 and an even value.'            )
         CALL ERRINT ( '#',  MW                                    )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFPOSC'                                    )
         RETURN

      END IF


      IF ( SIZED(RESULT) .LT. 2 ) THEN

         CALL SETMSG ( 'Result window size was #; size must be '
     .   //            'at least 2.'                               )
         CALL ERRINT ( '#', SIZED(RESULT)                          )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFPOSC'                                    )
         RETURN

      END IF


C
C     Set up a call to GFEVNT specific to the observer-target
C     coordinate search.
C
      QPNAMS(1) = 'TARGET'
      QCPARS(1) =  TARGET

      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) =  OBSRVR

      QPNAMS(3) = 'ABCORR'
      QCPARS(3) =  ABCORR

      QPNAMS(4) = 'COORDINATE SYSTEM'
      QCPARS(4) =  CRDSYS

      QPNAMS(5) = 'COORDINATE'
      QCPARS(5) =  COORD

      QPNAMS(6) = 'REFERENCE FRAME'
      QCPARS(6) =  FRAME

      QPNAMS(7) = 'VECTOR DEFINITION'
      QCPARS(7) =  POSDEF

      QPNAMS(8) = 'METHOD'
      QCPARS(8) = ' '

      QPNAMS(9) = 'DREF'
      QCPARS(9) =  DREF

      QPNAMS(10) = 'DVEC'
      QDPARS(1)  = DVEC(1)
      QDPARS(2)  = DVEC(2)
      QDPARS(3)  = DVEC(3)

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
      CALL GFEVNT ( GFSTEP,   GFREFN,  'COORDINATE',
     .              QNPARS,   QPNAMS,   QCPARS,
     .              QDPARS,   QIPARS,   QLPARS,
     .              RELATE,   REFVAL,   TOL,
     .              ADJUST,   CNFINE,   NORPT,
     .              GFREPI,   GFREPU,   GFREPF,
     .              MW,       NW,       WORK,
     .              NOBAIL,   GFBAIL,   RESULT )


      CALL CHKOUT ( 'GFPOSC' )
      RETURN

      END

