C$Procedure GFUDS ( GF, user defined scalar )

      SUBROUTINE GFUDS ( UDFUNS, UDQDEC, RELATE, REFVAL,
     .                   ADJUST, STEP,   CNFINE,
     .                   MW,     NW,     WORK,   RESULT  )

C$ Abstract
C
C     Perform a GF search on a user defined scalar quantity.
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
C     TIME
C     WINDOWS
C
C$ Keywords
C
C     EVENT
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

      EXTERNAL              UDQDEC
      EXTERNAL              UDFUNS

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
C     UDFUNS     I   Name of the routine that computes a scalar
C                    quantity corresponding to an ET.
C     UDQDEC     I   Name of the routine that computes whether the
C                    scalar quantity is decreasing.
C     RELATE     I   Operator that either looks for an extreme value
C                    (max, min, local, absolute) or compares the
C                    geometric quantity value and a number.
C     REFVAL     I   Value used as reference for scalar quantity
C                    condition.
C     ADJUST     I   Allowed variation for absolute extremal
C                    geometric conditions.
C     STEP       I   Step size used for locating extrema and roots.
C     CNFINE     I   SPICE window to which the search is confined.
C     MW         I   Size of workspace windows.
C     NW         I   Number of workspace windows.
C     WORK       I   Array containing workspace windows.
C     RESULT    I-O  SPICE window containing results.
C
C$ Detailed_Input
C
C     UDFUNS     the routine that returns the value of the scalar
C                quantity of interest at time ET. The calling sequence
C                for UDFUNS is:
C
C                   CALL UDFUNS ( ET, VALUE )
C
C                where:
C
C                   ET      a double precision value representing
C                           ephemeris time, expressed as seconds past
C                           J2000 TDB, at which to determine the scalar
C                           value.
C
C                   VALUE   is the value of the scalar quantity
C                           at ET.
C
C     UDQDEC     the name of the routine that determines if the scalar
C                quantity calculated by UDFUNS is decreasing.
C                The calling sequence of UDQDEC is:
C
C                   CALL UDQDEC ( UDFUNS, ET, ISDECR )
C
C                where:
C
C                   UDFUNS   the name of the scalar function as
C                            defined above.
C
C                   ET       a double precision value representing
C                            ephemeris time, expressed as seconds past
C                            J2000 TDB, at which to determine the time
C                            derivative of UDFUNS.
C
C                   ISDECR   a logical return indicating whether
C                            or not the scalar value returned by UDFUNS
C                            is decreasing. ISDECR returns true if the
C                            time derivative of UDFUNS at ET is
C                            negative.
C
C     RELATE     the scalar string comparison operator indicating
C                the numeric constraint of interest. Values are:
C
C                   '>'       value of scalar quantity greater than some
C                             reference (REFVAL).
C
C                   '='       value of scalar quantity equal to some
C                             reference (REFVAL).
C
C                   '<'       value of scalar quantity less than some
C                             reference (REFVAL).
C
C                   'ABSMAX'  The scalar quantity is at an absolute
C                             maximum.
C
C                   'ABSMIN'  The scalar quantity is at an absolute
C                              minimum.
C
C                   'LOCMAX'  The scalar quantity is at a local
C                             maximum.
C
C                   'LOCMIN'  The scalar quantity is at a local
C                             minimum.
C
C                The caller may indicate that the region of interest
C                is the set of time intervals where the quantity is
C                within a specified distance of an absolute extremum.
C                The argument ADJUST (described below) is used to
C                specified this distance.
C
C                Local extrema are considered to exist only in the
C                interiors of the intervals comprising the confinement
C                window:  a local extremum cannot exist at a boundary
C                point of the confinement window.
C
C                RELATE is insensitive to case, leading and
C                trailing blanks.
C
C     REFVAL     is the reference value used to define an equality or
C                inequality to  satisfied by the scalar quantity.
C                The units of REFVAL are those of the scalar quantity.
C
C     ADJUST     the amount by which the quantity is allowed to vary
C                from an absolute extremum.
C
C                If the search is for an absolute minimum is performed,
C                the resulting window contains time intervals when the
C                geometric quantity value has values between
C                ABSMIN and ABSMIN + ADJUST.
C
C                If the search is for an absolute maximum, the
C                corresponding range is  between ABSMAX - ADJUST and
C                ABSMAX.
C
C                ADJUST is not used for searches for local extrema,
C                equality or inequality conditions and must have value
C                zero for such searches.
C
C     STEP       the double precision time step size to use in
C                the search.
C
C                STEP must be short enough to for a search using this
C                step size to locate the time intervals where the
C                scalar quantity function is monotone increasing or
C                decreasing. However, STEP must not be *too* short,
C                or the search will take an unreasonable amount of time.
C
C                The choice of STEP affects the completeness but not
C                the precision of solutions found by this routine; the
C                precision is controlled by the convergence tolerance.
C                See the discussion of the parameter CNVTOL for
C                details.
C
C                STEP has units of TDB seconds.
C
C     CNFINE     is a SPICE window that confines the time period over
C                which the specified search is conducted. CNFINE may
C                consist of a single interval or a collection of
C                intervals.
C
C                In some cases the confinement window can be used to
C                greatly reduce the time period that must be searched
C                for the desired solution. See the Particulars section
C                below for further discussion.
C
C                See the Examples section below for a code example
C                that shows how to create a confinement window.
C
C                CNFINE must be initialized by the caller via the
C                SPICELIB routine SSIZED.
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
C                   NINTVLS  =  2*N  +  ( M / STEP )
C
C                where
C
C                   N     is the number of intervals in the confinement
C                         window
C
C                   M     is the measure of the confinement window, in
C                         units of seconds
C
C                   STEP  is the search step size in seconds
C
C                MW should then be set to
C
C                   2 * NINTVLS
C
C     NW         is a parameter specifying the number of SPICE windows
C                in the workspace array WORK (see description below)
C                used by this routine.  (The reason this dimension is
C                an input argument is that this allows run-time
C                error checking to be performed.)
C
C                NW must be at least as large as the parameter NWUDS.
C
C     WORK       is an array used to store workspace windows. This
C                array should be declared by the caller as shown:
C
C                    DOUBLE PRECISION     WORK ( LBCELL : MW,  NW )
C
C                WORK need not be initialized by the caller.
C
C     RESULT     a double precision SPICE window which will contain the
C                search results. RESULT must be declared and initialized
C                with sufficient size to capture the full set of time
C                intervals within the search region on which the
C                specified constraint is satisfied.
C
C                RESULT must be initialized by the caller via the
C                SPICELIB routine SSIZED.
C
C                If RESULT is non-empty on input, its contents
C                will be discarded before GFUDS conducts its search.
C
C$ Detailed_Output
C
C     WORK       the input workspace array, modified by this
C                routine.
C
C     RESULT     is a SPICE window containing the time intervals within
C                the confinement window, during which the specified
C                condition on the scalar quantity is met.
C
C                If the search is for local extrema, or for absolute
C                extrema with ADJUST set to zero, then normally each
C                interval of RESULT will be a singleton: the left and
C                right endpoints of each interval will be identical.
C
C                If no times within the confinement window satisfy the
C                search, RESULT will be returned with a cardinality
C                of zero.
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
C         number of intervals on which the specified condition
C         is met, the error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     5)  If the window count NW is less than NWUDS, the error
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
C     9)  If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     If the scalar function requires access to ephemeris data:
C
C        - SPK data: ephemeris data for any body over the
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
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when the scalar function
C     satisfies a caller-specified constraint. The resulting set of
C     intervals is returned as a SPICE window.
C
C     UDQDEC Default Template
C     =======================
C
C     The user must supply a routine to determine whether sign of the
C     time derivative of UDFUNS is positive or negative at ET. For
C     cases where UDFUNS is numerically well behaved, the user
C     may find it convenient to use a routine based on the below
C     template. UDDC determines the truth of the expression
C
C        d (UDFUNS)
C        --         < 0
C        dt
C
C     using the library routine UDDF to numerically calculate the
C     derivative of UDFUNS using a three-point estimation.
C     Please see the Examples section for an example of GFDECR use.
C
C           SUBROUTINE GFDECR ( UDFUNS, ET, ISDECR )
C           IMPLICIT NONE
C
C           EXTERNAL              UDFUNS
C           EXTERNAL              UDDF
C
C           DOUBLE PRECISION      ET
C           LOGICAL               ISDECR
C
C           DOUBLE PRECISION      DT
C
C           DT =  h, double precision interval size
C
C           CALL UDDC ( UDFUNS, ET, DT, ISDECR )
C
C           END
C
C     The Search Process
C     ==================
C
C     Regardless of the type of constraint selected by the caller, this
C     routine starts the search for solutions by determining the time
C     periods, within the confinement window, over which the specified
C     scalar function is monotone increasing and monotone
C     decreasing. Each of these time periods is represented by a SPICE
C     window. Having found these windows, all of the quantity
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
C     change of quantity function will be sampled. Starting at
C     the left endpoint of an interval, samples will be taken at each
C     step. If a change of sign is found, a root has been bracketed; at
C     that point, the time at which the time derivative of the quantity
C     function is zero can be found by a refinement process, for
C     example, using a binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the quantity function is monotone:
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
C     Use the meta-kernel shown below to load the required SPICE
C     kernels.
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
C              KERNELS_TO_LOAD = ( 'de414.bsp',
C                                  'pck00008.tpc',
C                                  'naif0009.tls'  )
C
C           \begintext
C
C     Conduct a search on the range-rate of the vector from the Sun
C     to the Moon. Define a function to calculate the value.
C
C        Code:
C
C           PROGRAM GFUDS_T
C           IMPLICIT NONE
C
C     C
C     C     Include GF parameter declarations:
C     C
C           INCLUDE 'gf.inc'
C
C           EXTERNAL     GFQ
C           EXTERNAL     GFDECR
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
C     C     Use the parameter MAXWIN for both the result window size
C     C     and the workspace size.
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
C           DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWUDS )
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
C           CALL SCARDD ( 0,      CNFINE )
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
C     C     The reference value is .3365 km/s - a range rate value known
C     C     to exist during the confinement window. We're not using the
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
C              CALL GFUDS ( GFQ,       GFDECR,
C          .                RELATE(J), REFVAL,  ADJUST, STEP, CNFINE,
C          .                MAXWIN,    NWUDS,   WORK,   RESULT )
C
C
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
C
C
C     C-Procedure GFQ
C
C           SUBROUTINE GFQ ( ET, VALUE )
C           IMPLICIT NONE
C
C     C- Abstract
C     C
C     C     User defined geometric quantity function. In this case,
C     C     the range from the sun to the Moon at TDB time ET.
C     C
C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      VALUE
C
C     C
C     C     Local variables.
C     C
C           INTEGER               TARG
C           INTEGER               OBS
C
C           CHARACTER*(12)        REF
C           CHARACTER*(12)        ABCORR
C
C           DOUBLE PRECISION      STATE ( 6 )
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      DVNORM
C
C     C
C     C     Initialization. Retrieve the vector from the Sun to
C     C     the Moon in the J2000 frame, without aberration
C     C     correction.
C     C
C           TARG   = 301
C           REF    = 'J2000'
C           ABCORR = 'NONE'
C           OBS    = 10
C
C           CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT )
C
C     C
C     C     Calculate the scalar range rate corresponding the
C     C     STATE vector.
C     C
C           VALUE = DVNORM( STATE )
C
C           END
C
C
C
C
C     C-Procedure GFDECR
C
C           SUBROUTINE GFDECR ( UDFUNS, ET, ISDECR )
C           IMPLICIT NONE
C
C     C- Abstract
C     C
C     C     User defined function to detect if the function derivative
C     C     is negative (the function is decreasing) at TDB time ET.
C     C
C
C           EXTERNAL              UDFUNS
C           EXTERNAL              UDDF
C
C           DOUBLE PRECISION      ET
C           LOGICAL               ISDECR
C
C           DOUBLE PRECISION      DT
C
C           DT = 1.D0
C
C     C
C     C     Determine if GFQ is decreasing at ET.
C     C
C     C     UDDC - the default GF function to determine if
C     C                the derivative of the user defined
C     C                function is negative at ET.
C     C
C     C     UDFUNS - the user defined scalar quantity function.
C     C
C           CALL UDDC ( UDFUNS, ET, DT, ISDECR )
C
C           END
C
C     The program outputs:
C
C      Relation condition: =
C     Start time, drdt = 2007-JAN-02 00:35:19.574       0.336500000
C     Stop time,  drdt = 2007-JAN-02 00:35:19.574       0.336500000
C     Start time, drdt = 2007-JAN-19 22:04:54.899       0.336500000
C     Stop time,  drdt = 2007-JAN-19 22:04:54.899       0.336500000
C     Start time, drdt = 2007-FEB-01 23:30:13.428       0.336500000
C     Stop time,  drdt = 2007-FEB-01 23:30:13.428       0.336500000
C     Start time, drdt = 2007-FEB-17 11:10:46.540       0.336500000
C     Stop time,  drdt = 2007-FEB-17 11:10:46.540       0.336500000
C     Start time, drdt = 2007-MAR-04 15:50:19.929       0.336500000
C     Stop time,  drdt = 2007-MAR-04 15:50:19.929       0.336500000
C     Start time, drdt = 2007-MAR-18 09:59:05.959       0.336500000
C     Stop time,  drdt = 2007-MAR-18 09:59:05.959       0.336500000
C
C      Relation condition: <
C     Start time, drdt = 2007-JAN-02 00:35:19.574       0.336500000
C     Stop time,  drdt = 2007-JAN-19 22:04:54.899       0.336500000
C     Start time, drdt = 2007-FEB-01 23:30:13.428       0.336500000
C     Stop time,  drdt = 2007-FEB-17 11:10:46.540       0.336500000
C     Start time, drdt = 2007-MAR-04 15:50:19.929       0.336500000
C     Stop time,  drdt = 2007-MAR-18 09:59:05.959       0.336500000
C
C      Relation condition: >
C     Start time, drdt = 2007-JAN-01 00:00:00.000       0.515522367
C     Stop time,  drdt = 2007-JAN-02 00:35:19.574       0.336500000
C     Start time, drdt = 2007-JAN-19 22:04:54.899       0.336500000
C     Stop time,  drdt = 2007-FEB-01 23:30:13.428       0.336500000
C     Start time, drdt = 2007-FEB-17 11:10:46.540       0.336500000
C     Stop time,  drdt = 2007-MAR-04 15:50:19.929       0.336500000
C     Start time, drdt = 2007-MAR-18 09:59:05.959       0.336500000
C     Stop time,  drdt = 2007-APR-01 00:00:00.000       0.793546222
C
C      Relation condition: LOCMIN
C     Start time, drdt = 2007-JAN-11 07:03:58.988      -0.803382743
C     Stop time,  drdt = 2007-JAN-11 07:03:58.988      -0.803382743
C     Start time, drdt = 2007-FEB-10 06:26:15.439      -0.575837623
C     Stop time,  drdt = 2007-FEB-10 06:26:15.439      -0.575837623
C     Start time, drdt = 2007-MAR-12 03:28:36.404      -0.441800446
C     Stop time,  drdt = 2007-MAR-12 03:28:36.404      -0.441800446
C
C      Relation condition: ABSMIN
C     Start time, drdt = 2007-JAN-11 07:03:58.988      -0.803382743
C     Stop time,  drdt = 2007-JAN-11 07:03:58.988      -0.803382743
C
C      Relation condition: LOCMAX
C     Start time, drdt = 2007-JAN-26 02:27:33.766       1.154648992
C     Stop time,  drdt = 2007-JAN-26 02:27:33.766       1.154648992
C     Start time, drdt = 2007-FEB-24 09:35:07.816       1.347132236
C     Stop time,  drdt = 2007-FEB-24 09:35:07.816       1.347132236
C     Start time, drdt = 2007-MAR-25 17:26:56.150       1.428141707
C     Stop time,  drdt = 2007-MAR-25 17:26:56.150       1.428141707
C
C      Relation condition: ABSMAX
C     Start time, drdt = 2007-MAR-25 17:26:56.150       1.428141707
C     Stop time,  drdt = 2007-MAR-25 17:26:56.150       1.428141707
C
C$ Restrictions
C
C     1) Any kernel files required by this routine must be loaded
C        (normally via the SPICELIB routine FURNSH) before this routine
C        is called.
C
C$ Literature_References
C
C    None.
C
C$ Author_and_Institution
C
C    N.J. Bachman   (JPL)
C    E.D. Wright    (JPL)
C
C$ Version
C
C-   SPICELIB Version 1.1.0, 15-JUL-2014 (EDW)
C
C       Correction to description of UDQDEC to show UDFUNS as
C       an argument.
C
C       Edit to comments to correct search description.
C
C       Implemented use of ZZHOLDD to allow user to alter convergence
C       tolerance.
C
C       Removed the STEP > 0 error check. The GFSSTP call includes
C       the check.
C
C       Removed ZZGFREF call. That call now occurs in ZZGFRELX. Update
C       to ZZGFRELX argument list to reflect this change in
C       functionality.
C
C       Added RETURN() check.
C
C-   SPICELIB Version 1.0.0  16-FEB-2010 (EDW)
C
C-&

C$ Index_Entries
C
C   GF user defined scalar function search
C
C-&

C
C     SPICELIB functions.
C

      LOGICAL               ODD
      LOGICAL               RETURN
      INTEGER               SIZED


C
C     Local variables.
C
      EXTERNAL              GFREFN
      EXTERNAL              GFREPF
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFSTEP
      EXTERNAL              ZZGFUDLT

      DOUBLE PRECISION      TOL
      LOGICAL               OK

      LOGICAL               GFBAIL
      EXTERNAL              GFBAIL

      LOGICAL               NOBAIL
      PARAMETER           ( NOBAIL = .FALSE. )

      LOGICAL               NORPT
      PARAMETER           ( NORPT  = .FALSE. )

C
C     Dummy variables.
C
      CHARACTER*(1)         RPTPRE ( 2 )
      CHARACTER*(1)         RPTSUF ( 2 )

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN( 'GFUDS' )


C
C     Confirm minimum number of windows.
C
      IF ( NW .LT. NWUDS ) THEN

         CALL SETMSG ( 'Workspace window count was #; count must be '
     .   //            'at least #.'                                 )
         CALL ERRINT ( '#',  NW                                      )
         CALL ERRINT ( '#',  NWUDS                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                     )
         CALL CHKOUT ( 'GFUDS'                                       )
         RETURN

      END IF


C
C     Confirm minimum window sizes.
C
      IF ( (MW .LT. 2) .OR. ODD(MW) ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2 and an even value.'            )
         CALL ERRINT ( '#',  MW                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                  )
         CALL CHKOUT ( 'GFUDS'                                    )
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
         CALL CHKOUT ( 'GFUDS'                                     )
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
C     Initialize the RESULT window to empty.
C
      CALL SCARDD ( 0, RESULT)


C
C     Call ZZGFRELX to do the event detection work.
C
      CALL ZZGFRELX ( GFSTEP, GFREFN,    UDQDEC,  ZZGFUDLT,
     .                UDFUNS, RELATE,    REFVAL,
     .                TOL,    ADJUST,    CNFINE,  MW,
     .                NW,     WORK,      NORPT,   GFREPI,
     .                GFREPU, GFREPF,    RPTPRE,  RPTSUF,
     .                NOBAIL, GFBAIL,    RESULT   )


      CALL CHKOUT( 'GFUDS' )

      END

