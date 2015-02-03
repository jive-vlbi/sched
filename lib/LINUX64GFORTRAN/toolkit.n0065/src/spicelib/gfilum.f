C$Procedure GFILUM ( GF, illumination angle search )
  
      SUBROUTINE GFILUM ( METHOD, ANGTYP, TARGET, ILLMN,  
     .                    FIXREF, ABCORR, OBSRVR, SPOINT, 
     .                    RELATE, REFVAL, ADJUST, STEP,       
     .                    CNFINE, MW,     NW,     WORK,  RESULT )

C$ Abstract
C
C     Determine time intervals over which a specified constraint on
C     the observed phase, solar incidence, or emission angle at
C     a specified target body surface point is met.
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
C     FRAMES
C     NAIF_IDS
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     ANGLE
C     EPHEMERIS
C     ILLUMINATION
C     LIGHTING
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         ANGTYP
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLMN
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
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
C     NWILUM     P   Number of workspace windows for angle search.
C     METHOD     I   Computation method.
C     ANGTYP     I   Type of illumination angle.
C     TARGET     I   Name of the target body.
C     ILLMN      I   Name of the illumination source.
C     FIXREF     I   Body-fixed, body-centered target body frame.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C     SPOINT     I   Body-fixed coordinates of a target surface point.
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
C     METHOD      is a short string providing parameters defining
C                 the computation method to be used. Parameters
C                 include, but are not limited to, the shape model
C                 used to represent the surface of the target body.
C
C                 The only choice currently supported is
C
C                    'Ellipsoid'        The illumination angle
C                                       computation uses a triaxial
C                                       ellipsoid to model the surface
C                                       of the target body. The
C                                       ellipsoid's radii must be
C                                       available in the kernel pool.
C
C                 Neither case nor blanks are significant in METHOD.
C                 For example, the string ' eLLipsoid ' is valid.
C
C
C     ANGTYP      is a string specifying the type of illumination
C                 angle for which a search is to be performed. The
C                 possible values of ANGTYP are
C
C                    'PHASE'
C                    'INCIDENCE'
C                    'EMISSION'
C
C                 When the illumination source is the sun, the 
C                 incidence angle is commonly called the "solar 
C                 incidence angle."
C
C                 See the Particulars section below for a detailed
C                 description of these angles.
C
C                 Neither case nor white space are significant in 
C                 ANGTYP. For example, the string ' Incidence ' is
C                 valid.
C
C
C     TARGET      is the name of a target body. The point at which the
C                 illumination angles are defined is located on the
C                 surface of this body.
C
C                 Optionally, you may supply the integer ID code for
C                 the object as an integer string. For example both
C                 'MOON' and '301' are legitimate strings that indicate
C                 the moon is the target body.
C
C                 Neither case nor leading and trailing blanks are
C                 significant in TARGET. For example, the string
C                 ' Incidence ' is valid. Sequences of embedded blanks
C                 are treated as a single blank. 
C
C
C     ILLMN       is the name of the illumination source. This source
C                 may be any ephemeris object. Case, blanks, and
C                 numeric values are treated in the same way as for the
C                 input TARGET.
C
C
C     FIXREF      is the name of the body-fixed, body-centered
C                 reference frame associated with the target body. The
C                 input surface point SPOINT is expressed relative to
C                 this reference frame, and this frame is used to
C                 define the orientation of the target body as a
C                 function of time.
C
C                 The string FIXREF is case-insensitive, and leading
C                 and trailing blanks in FIXREF are not significant.
C
C
C     ABCORR      indicates the aberration corrections to be applied to
C                 the observer-surface point vector, the surface point-
C                 illumination source vector, and the target body
C                 orientation to account for one-way light time and
C                 stellar aberration.
C
C                 Any "reception" correction accepted by SPKEZR can be
C                 used here. See the header of SPKEZR for a detailed
C                 description of the aberration correction options. For
C                 convenience, the options are listed below:
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
C                 Case and blanks are not significant in the string
C                 ABCORR.
C
C
C     OBSRVR      is the name of an observing body. Case, blanks, and
C                 numeric values are treated in the same way as for the
C                 input TARGET.
C
C
C     SPOINT      is a surface point on the target body, expressed in
C                 Cartesian coordinates, relative to the body-fixed
C                 target frame designated by FIXREF.
C
C                 SPOINT need not be visible from the observer's
C                 location in order for the constraint specified by
C                 RELATE and REFVAL (see descriptions below) to be
C                 satisfied.
C
C                 The components of SPOINT have units of km.
C
C
C     RELATE      is a relational operator used to define a constraint
C                 on a specified illumination angle. The result window
C                 found by this routine indicates the time intervals
C                 where the constraint is satisfied. Supported values
C                 of RELATE and corresponding meanings are shown below:
C
C                    '>'      The angle is greater than the reference
C                             value REFVAL.
C
C                    '='      The angle is equal to the reference
C                             value REFVAL.
C
C                    '<'      The angle is less than the reference
C                             value REFVAL.
C
C
C                   'ABSMAX'  The angle is at an absolute maximum.
C
C                   'ABSMIN'  The angle is at an absolute minimum.
C
C                   'LOCMAX'  The angle is at a local maximum.
C
C                   'LOCMIN'  The angle is at a local minimum.
C
C                The caller may indicate that the window of interest is
C                the set of time intervals where the angle is within a
C                specified separation from an absolute extremum. The
C                argument ADJUST (described below) is used to specify
C                this separation.
C
C                Local extrema are considered to exist only in the
C                interiors of the intervals comprising the confinement
C                window: a local extremum cannot exist at a boundary
C                point of the confinement window.
C
C                Case is not significant in the string RELATE.
C
C
C     REFVAL     is the reference value used together with the argument
C                RELATE to define an equality or inequality to be
C                satisfied by the specified illumination angle. See the
C                discussion of RELATE above for further information.
C
C                The units of REFVAL are radians.
C
C
C     ADJUST     is a parameter used to modify searches for absolute
C                extrema: when RELATE is set to ABSMAX or ABSMIN and
C                ADJUST is set to a positive value, GFILUM will find
C                times when the specified illumination angle is within
C                ADJUST radians of the specified extreme value.
C
C                If ADJUST is non-zero and a search for an absolute
C                minimum is performed, the result window contains
C                time intervals when the specified illumination angle
C                has values between the absolute minimum MIN
C                MIN + ADJUST radians.
C
C                If ADJUST is non-zero and the search is for an
C                absolute maximum, the corresponding angle is between
C                the absolute maximum MAX and MAX - ADJUST radians.
C               
C                ADJUST is not used for searches for local extrema,
C                equality or inequality conditions.
C
C
C     STEP       is the step size to be used in the search. STEP must
C                be short enough for a search using this step size to
C                locate the time intervals where the specified
C                illumination angle is monotone increasing or
C                decreasing. However, STEP must not be *too* short, or
C                the search will take an unreasonable amount of time.
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
C                In some cases the confinement window can be used to
C                greatly reduce the time window that must be searched
C                for the desired solution. See the Particulars section
C                below for further discussion.
C                
C                See the Examples section below for a code example 
C                that shows how to create a confinement window.
C                
C                CNFINE must be initialized by the caller via the
C                SPICELIB routine SSIZED.
C
C
C     MW         is a parameter specifying the length of the workspace
C                array WORK (see description below) used by this
C                routine. MW should be at least as large as TWICE the
C                number of intervals within the search window on which
C                the specified illumination angle is monotone increasing
C                or decreasing. It does no harm to pick a value of
C                MW larger than the minimum required to execute the
C                specified search, but if MW is too small, the
C                search will fail.
C            
C
C     WORK       is an array used to store workspace windows. This
C                array should be declared by the caller as shown:
C
C                   INCLUDE 'gf.inc'
C                      ...
C
C                   DOUBLE PRECISION    WORK ( LBCELL : MW, NWILUM )
C
C                where MW is a constant declared by the caller and
C                NWILUM is a constant defined in the SPICELIB INCLUDE
C                file gf.inc.
C
C                WORK need not be initialized by the caller.
C
C
C     RESULT     is the result window. RESULT must be initialized via a
C                call to SSIZED. RESULT must be declared and initialized
C                with sufficient size to capture the full set of time
C                intervals within the search window on which the 
C                specified constraint is satisfied.
C
C
C$ Detailed_Output
C
C     WORK       is the input workspace array, modified by this
C                routine. The caller should re-initialize this array
C                before attempting to use it for any other purpose.
C
C     RESULT     is the window of intervals, contained within the
C                confinement window CNFINE, on which the specified
C                constraint is satisfied.
C
C                The endpoints of the time intervals comprising RESULT
C                are interpreted as seconds past J2000 TDB.
C 
C                If RESULT is non-empty on input, its contents will be
C                discarded before gfilum_c conducts its search.
C
C                If the search is for local extrema, or for absolute
C                extrema with ADJUST set to zero, then normally each
C                interval of RESULT will be a singleton: the left and
C                right endpoints of each interval will be identical.
C
C                If no times within the confinement window satisfy the
C                constraint, RESULT will be returned with a
C                cardinality of zero.
C
C$ Parameters
C
C     LBCELL     is the lower bound for SPICE Cell arrays.
C
C     CNVTOL     is the default convergence tolerance used for finding
C                endpoints of the intervals comprising the result
C                window. CNVTOL is also used for finding intermediate
C                results; in particular, CNVTOL is used for finding the
C                windows on which the specified illumination angle is
C                increasing or decreasing. CNVTOL is used to determine
C                when binary searches for roots should terminate: when
C                a root is bracketed within an interval of length
C                CNVTOL, the root is considered to have been found.
C
C                The accuracy, as opposed to precision, of roots found
C                by this routine depends on the accuracy of the input
C                data. In most cases, the accuracy of solutions will be
C                inferior to their precision.
C
C                The calling program can reset the convergence
C                tolerance; see the Particulars section below for
C                further information.
C
C
C     NWILUM     is the number of workspace windows required by
C                this routine. 
C
C     See INCLUDE file gf.inc for declarations and descriptions of 
C     parameters used throughout the GF subsystem.
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
C     3)  If the window size MW is less than 2, the error 
C         SPICE(INVALIDDIMENSION) will be signaled.
C
C     4)  If the window count NW is less than NWILUM, the error
C         SPICE(INVALIDDIMENSION) will be signaled.
C
C     5)  If an error (typically cell overflow) occurs while performing 
C         window arithmetic, the error will be diagnosed by a routine
C         in the call tree of this routine.
C
C     6)  If the output SPICE window RESULT has insufficient capacity
C         to hold the set of intervals on which the specified
C         illumination angle condition is met, the error will be
C         diagnosed by a routine in the call tree of this routine. If
C         the result window has size less than 2, the error
C         SPICE(INVALIDDIMENSION) will be signaled by this routine.
C
C     7)  If the input target body-fixed frame FIXREF is not
C         recognized, an error is signaled by a routine in the call
C         tree of this routine. A frame name may fail to be recognized
C         because a required frame specification kernel has not been
C         loaded; another cause is a misspelling of the frame name.
C
C     8)  If the input frame FIXREF is not centered at the target body,
C         an error is signaled by a routine in the call tree of this
C         routine.
C
C     9)  If the input argument METHOD is not recognized, the error
C         SPICE(INVALIDMETHOD) is signaled.
C
C    10)  If the illumination angle type ANGTYP is not recognized,
C         an error is signaled by a routine in the call tree
C         of this routine.
C
C    11)  If the relational operator RELATE is not recognized, an 
C         error is signaled by a routine in the call tree of this
C         routine.
C
C    12)  If the aberration correction specifier contains an
C         unrecognized value, an error is signaled by a routine in the
C         call tree of this routine.
C
C    13)  If ADJUST is negative, an error is signaled by a routine in
C         the call tree of this routine.
C
C    14)  If any of the input body names do not map to NAIF ID
C         codes, an error is signaled by a routine in the call tree of
C         this routine.
C
C    15)  If the target coincides with the observer or the illumination
C         source, an error is signaled by a routine in the call tree
C         of this routine.
C
C    16)  If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target, observer, and the
C          illumination source must be loaded. If aberration
C          corrections are used, the states of target, observer, and
C          the illumination source relative to the solar system
C          barycenter must be calculable from the available ephemeris
C          data. Typically ephemeris data are made available by loading
C          one or more SPK files via FURNSH.
C
C        - PCK data: if the target body shape is modeled as an
C          ellipsoid (currently no other shapes are supported), 
C          triaxial radii for the target body must be loaded
C          into the kernel pool. Typically this is done by loading a
C          text PCK file via FURNSH.
C
C        - Further PCK data: rotation data for the target body must be
C          loaded. These may be provided in a text or binary PCK file.
C
C        - Frame data: if a frame definition not built into SPICE
C          is required to convert the observer and target states to the
C          body-fixed frame of the target, that definition must be
C          available in the kernel pool. Typically the definition is
C          supplied by loading a frame kernel via FURNSH.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C
C$ Particulars
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when the specified illumination
C     angle satisfies a caller-specified constraint. The resulting set
C     of intervals is returned as a SPICE window.
C
C     The term "illumination angles" refers to the following set of
C     angles:
C
C
C        phase angle              Angle between the vectors from the
C                                 surface point to the observer and
C                                 from the surface point to the 
C                                 illumination source.
C
C        incidence angle          Angle between the surface normal at
C                                 the specified surface point and the
C                                 vector from the surface point to the
C                                 illumination source. When the sun is
C                                 the illumination source, this angle is
C                                 commonly called the "solar incidence
C                                 angle."
C
C        emission angle           Angle between the surface normal at
C                                 the specified surface point and the
C                                 vector from the surface point to the
C                                 observer.
C 
C     The diagram below illustrates the geometric relationships
C     defining these angles. The labels for the incidence, emission,
C     and phase angles are "inc.", "e.", and "phase".
C
C
C                                                      *
C                                              illumination source
C
C                    surface normal vector
C                              ._                 _.
C                              |\                 /|  illumination 
C                                \    phase      /    source vector
C                                 \   .    .    /
C                                 .            .
C                                   \   ___   /
C                              .     \/     \/
C                                    _\ inc./
C                             .    /   \   /
C                             .   |  e. \ /
C         *             <--------------- *  surface point on
C      viewing            vector            target body
C      location           to viewing
C      (observer)         location
C
C
C     Note that if the target-observer vector, the target normal vector
C     at the surface point, and the target-illumination source vector
C     are coplanar, then phase is the sum of the incidence and emission
C     angles. This rarely occurs; usually
C
C        phase angle  <  incidence angle + emission angle
C
C     All of the above angles can be computed using light time
C     corrections, light time and stellar aberration corrections, or no
C     aberration corrections. In order to describe apparent geometry as
C     observed by a remote sensing instrument, both light time and
C     stellar aberration corrections should be used.
C     
C     The way aberration corrections are applied by this routine
C     is described below.
C
C        Light time corrections
C        ======================
C
C           Observer-target surface point vector
C           ------------------------------------
C
C           Let ET be the epoch at which an observation or remote
C           sensing measurement is made, and let ET - LT ("LT" stands
C           for "light time") be the epoch at which the photons
C           received at ET were emitted from the surface point SPOINT.
C           Note that the light time between the surface point and
C           observer will generally differ from the light time between
C           the target body's center and the observer.
C
C
C           Target body's orientation
C           -------------------------
C
C           Using the definitions of ET and LT above, the target body's
C           orientation at ET - LT is used. The surface normal is
C           dependent on the target body's orientation, so the body's
C           orientation model must be evaluated for the correct epoch.
C
C
C           Target body -- illumination source vector
C           -----------------------------------------
C
C           The surface features on the target body near SPOINT will
C           appear in a measurement made at ET as they were at ET-LT.
C           In particular, lighting on the target body is dependent on
C           the apparent location of the illumination source as seen
C           from the target body at ET-LT. So, a second light time
C           correction is used to compute the position of the
C           illumination source relative to the surface point.
C
C
C        Stellar aberration corrections
C        ==============================
C
C        Stellar aberration corrections are applied only if
C        light time corrections are applied as well.
C
C           Observer-target surface point body vector
C           -----------------------------------------
C
C           When stellar aberration correction is performed, the
C           observer-to-surface point direction vector, which we'll
C           call SRFVEC, is adjusted so as to point to the apparent
C           position of SPOINT: considering SPOINT to be an ephemeris
C           object, SRFVEC points from the observer's position at ET to
C           the light time and stellar aberration
C           corrected position of SPOINT.
C
C           Target body-illumination source vector
C           --------------------------------------
C
C           The target body-illumination source vector is the apparent
C           position of the illumination source, corrected for light
C           time and stellar aberration, as seen from the surface point
C           SPOINT at time ET-LT.
C
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
C     illumination angle is monotone increasing and monotone decreasing.
C     Each of these time periods is represented by a SPICE window.
C     Having found these windows, all of the illumination angle's local
C     extrema within the confinement window are known. Absolute extrema
C     then can be found very easily.
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
C     searched as follows: first, the input step size is used to
C     determine the time separation at which the sign of the rate of
C     change of the illumination angle will be sampled. Starting at the
C     left endpoint of an interval, samples will be taken at each step.
C     If a change of sign is found, a root has been bracketed; at that
C     point, the time at which the rate of change of the selected
C     illumination angle is zero can be found by a refinement process,
C     for example, via binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the illumination angle is monotone:
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
C     Having some knowledge of the relative geometry of the target,
C     observer, and illumination source can be a valuable aid in
C     picking a reasonable step size. In general, the user can
C     compensate for lack of such knowledge by picking a very short
C     step size; the cost is increased computation time.

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
C     attained and times when the illumination angle is equal to a
C     reference value. All endpoints of the intervals comprising the
C     result window are either endpoints of intervals of the
C     confinement window or roots.
C
C     Once a root has been bracketed, a refinement process is used to
C     narrow down the time interval within which the root must lie.
C     This refinement process terminates when the location of the root
C     has been determined to within an error margin called the
C     "convergence tolerance." The convergence tolerance used by this
C     routine is set via the parameter CNVTOL.
C 
C     The value of CNVTOL is set to a "tight" value so that the
C     tolerance doesn't become the limiting factor in the accuracy of
C     solutions found by this routine. In general the accuracy of input
C     data will be the limiting factor.
C
C     The user may change the convergence tolerance from the default
C     CNVTOL value by calling the routine GFSTOL, e.g.
C
C        CALL GFSTOL( tolerance value in seconds )
C
C     Call GFSTOL prior to calling this routine. All subsequent
C     searches will use the updated tolerance value.
C
C     Searches over time windows of long duration may require use of
C     larger tolerance values than the default: the tolerance must be
C     large enough so that it, when added to or subtracted from the
C     confinement window's lower and upper bounds, yields distinct time
C     values.
C
C     Setting the tolerance tighter than CNVTOL is unlikely to be
C     useful, since the results are unlikely to be more accurate.
C     Making the tolerance looser will speed up searches somewhat,
C     since a few convergence steps will be omitted.
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
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C
C     1) Determine time intervals over which the MER-1 ("Opportunity")
C        rover's location satisfies certain constraints on its
C        illumination and visibility as seen from the Mars
C        Reconaissance Orbiter (MRO) spacecraft.
C
C        In this case we require the emission angle to be less than
C        20 degrees and the solar incidence angle to be less than
C        60 degrees.
C
C        The reader can verify that the observation start times of the
C        MRO HIRISE images
C
C           Product ID              Image start time
C           ----------              ----------------
C           TRA_000873_1780_RED     2006-10-03T12:44:13.425
C           PSP_001414_1780_RED     2006-11-14T15:39:55.373
C           PSP_001612_1780_RED     2006-11-30T01:38:34.390 
C
C        are contained within the result window found by the 
C        example program shown below.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File: mer1_ex.tm
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
C              pck00010.tpc                  Planet orientation
C                                            and radii
C              naif0010.tls                  Leapseconds
C              mro_psp1.bsp                  MRO ephemeris
C              mer1_surf_rover_ext10_v1.bsp  MER-1 ephemeris
C              mer1_surf_rover_ext11_v1.bsp  MER-1 ephemeris
C              mer1_ls_040128_iau2000_v1.bsp MER-1 landing site
C                                            ephemeris
C              mer1_v10.tf                   MER-1 frame kernel
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00010.tpc',
C                                  'naif0010.tls',
C                                  'mro_psp1.bsp',
C                                  'mer1_surf_rover_ext10_v1.bsp',
C                                  'mer1_surf_rover_ext11_v1.bsp',
C                                  'mer1_ls_040128_iau2000_v1.bsp',
C                                  'mro_psp1.bsp',
C                                  'mer1_v10.tf'                    )
C           \begintext
C
C
C
C        Example code begins here.
C 
C
C              PROGRAM MER1_EX
C              IMPLICIT NONE
C        C
C        C     Global parameters
C        C
C              INCLUDE 'gf.inc'
C              INCLUDE 'zzabcorr.inc'
C
C        C
C        C     SPICE cell lower bound:
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      RPD
C
C              INTEGER               WNCARD
C
C        C
C        C     Local parameters
C        C
C        C
C        C     Output time format:
C        C
C              CHARACTER*(*)         FMT
C              PARAMETER           ( FMT =
C             .             'YYYY MON DD HR:MN:SC.###### UTC' )
C
C        C
C        C     Meta-kernel name:
C        C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'mer1_ex.tm' )
C
C        C
C        C        Maximum number of intervals in the windows
C        C        used in this program:
C        C
C              INTEGER               MAXIVL
C              PARAMETER           ( MAXIVL = 1000 )
C
C              INTEGER               MAXWIN
C              PARAMETER           ( MAXWIN = 2 * MAXIVL )
C
C        C
C        C     Maximum length of reference frame name:
C        C
C              INTEGER               FRNMLN
C              PARAMETER           ( FRNMLN = 32 )
C
C        C
C        C     Maximum length of body name:
C        C
C              INTEGER               BDNMLN
C              PARAMETER           ( BDNMLN = 36 )
C
C        C
C        C     Maximum length of time string:
C        C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 40 )
C
C        C
C        C     Length of computation method string:
C        C
C              INTEGER               METLEN
C              PARAMETER           ( METLEN = 80 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(FRNMLN)    FIXREF
C              CHARACTER*(BDNMLN)    ILLMN
C              CHARACTER*(METLEN)    METHOD
C              CHARACTER*(BDNMLN)    OBSRVR
C              CHARACTER*(BDNMLN)    TARGET
C              CHARACTER*(TIMLEN)    TIMSTR
C              CHARACTER*(TIMLEN)    UTCBEG
C              CHARACTER*(TIMLEN)    UTCEND
C
C              DOUBLE PRECISION      ADJUST
C              DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
C              DOUBLE PRECISION      EMISSN
C              DOUBLE PRECISION      ET0
C              DOUBLE PRECISION      ET1
C              DOUBLE PRECISION      FINISH
C              DOUBLE PRECISION      PHASE
C              DOUBLE PRECISION      REFVAL
C              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C              DOUBLE PRECISION      ROVLT
C              DOUBLE PRECISION      ROVPOS ( 3 )
C              DOUBLE PRECISION      SOLAR
C              DOUBLE PRECISION      SRFVEC ( 3 )
C              DOUBLE PRECISION      START
C              DOUBLE PRECISION      STEP
C              DOUBLE PRECISION      TRGEPC
C              DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWILUM )
C              DOUBLE PRECISION      WNSOLR ( LBCELL : MAXWIN )
C
C              INTEGER               I
C
C        C
C        C     Load kernels:
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Set window sizes:
C        C
C              CALL SSIZED ( 2,      CNFINE )
C              CALL SSIZED ( MAXWIN, RESULT )
C              CALL SSIZED ( MAXWIN, WNSOLR )
C
C        C
C        C     Set the search interval:
C        C
C              UTCBEG = '2006 OCT 02 00:00:00 UTC'
C              CALL STR2ET ( UTCBEG, ET0 )
C
C              UTCEND = '2006 NOV 30 12:00:00 UTC'
C              CALL STR2ET ( UTCEND, ET1 )
C
C              CALL WNINSD ( ET0, ET1, CNFINE )
C
C        C
C        C     Set observer, target, aberration correction, and the
C        C     Mars body-fixed, body-centered reference frame. The
C        C     lighting source is the sun.
C        C
C        C     Aberration corrections are set for remote observations.
C        C
C              ILLMN  = 'SUN'
C              OBSRVR = 'MRO'
C              TARGET = 'MARS'
C              ABCORR = 'CN+S'
C              FIXREF = 'IAU_MARS'
C
C        C
C        C     Use the rover position at the start of
C        C     the search interval as the surface point.
C        C
C              CALL SPKPOS ( 'MER-1', ET0,    FIXREF,
C             .              'NONE',  TARGET, ROVPOS, ROVLT )
C
C        C
C        C     Initialize the adjustment value for absolute
C        C     extremum searches. We're not performing
C        C     such searches in this example, but this input
C        C     to GFILUM must still be set.
C        C
C              ADJUST = 0.D0
C
C        C
C        C     The computation uses an ellipsoidal model for the
C        C     target body shape.
C        C
C              METHOD = 'Ellipsoid'
C
C        C
C        C     Set the reference value to use for the solar
C        C     incidence angle search.
C        C
C              REFVAL = 60.D0 * RPD()
C
C        C
C        C     Since the period of the solar incidence angle
C        C     is about one Martian day, we can safely use 6 hours
C        C     as the search step.
C        C
C              STEP   = 21600.D0
C
C        C
C        C     Search over the confinement window for times
C        C     when the solar incidence angle is less than
C        C     the reference value.
C        C
C              CALL GFILUM ( METHOD, 'INCIDENCE',    TARGET, ILLMN,
C             .              FIXREF, ABCORR,         OBSRVR, ROVPOS,
C             .              '<',    REFVAL, ADJUST, STEP,   CNFINE,
C             .              MAXWIN, NWILUM, WORK,   WNSOLR          )
C
C        C
C        C     With the search on the incidence angle complete, perform
C        C     a search on the emission angle.
C        C  
C        C     Set the reference value for the emission angle search.
C        C
C              REFVAL = 20D0 * RPD()
C
C        C
C        C     We'll use 15 minutes as the search step. This step
C        C     is small enough to be suitable for Mars orbiters.
C        C     Units are seconds.
C        C
C              STEP   = 900.D0
C
C        C
C        C     Search over the previous result window for times when the
C        C     emission angle is less than the reference value.
C        C
C              CALL GFILUM ( METHOD, 'EMISSION', TARGET, ILLMN,
C             .              FIXREF, ABCORR,     OBSRVR, ROVPOS,
C             .              '<',    REFVAL,     ADJUST, STEP,
C             .              WNSOLR, MAXWIN,     NWILUM, WORK,  RESULT )
C
C        C
C        C     Display the result window. Show the solar incidence
C        C     and emission angles at the window's interval
C        C     boundaries.
C        C
C              WRITE (*,*) ' '
C
C              IF ( WNCARD( RESULT ) .EQ. 0 ) THEN
C
C                 WRITE (*,*) '     Window is empty: condition '
C             .   //          'is not met.'
C
C              ELSE
C
C                 WRITE (*,*) '                                     '
C             .   //          '      Solar Incidence   Emission'
C                 WRITE (*,*) '                                     '
C             .   //          '            (deg)         (deg)'
C                 WRITE (*,*) ' '
C
C                 DO I = 1, WNCARD( RESULT )
C
C                    CALL WNFETD ( RESULT, I, START, FINISH )
C
C                    CALL TIMOUT ( START,  FMT,   TIMSTR )
C        C
C        C           Compute the angles of interest at the boundary
C        C           epochs.
C        C
C                    CALL ILUMIN ( METHOD, TARGET, START,  FIXREF,
C             .                    ABCORR, OBSRVR, ROVPOS, TRGEPC,
C             .                    SRFVEC, PHASE,  SOLAR,  EMISSN )
C
C                    WRITE (*, '(A11, A31, 2F15.9)' )
C             .            'Start: ', TIMSTR, SOLAR*DPR(), EMISSN*DPR()
C
C
C                    CALL TIMOUT ( FINISH, FMT,   TIMSTR )
C
C                    CALL ILUMIN ( METHOD, TARGET, FINISH, FIXREF,
C             .                    ABCORR, OBSRVR, ROVPOS, TRGEPC,
C             .                    SRFVEC, PHASE,  SOLAR,  EMISSN )
C
C                    WRITE (*, '(A11, A31, 2F15.9)' )
C             .            'Stop:  ', TIMSTR, SOLAR*DPR(), EMISSN*DPR()
C
C                    WRITE (*,*) ' '
C
C                 END DO
C
C              END IF
C
C              END
C
C
C        When this program was executed on a PC/Linux/gfortran
C        platform, the output was:
C
C
C                                           Solar Incidence   Emission
C                                                 (deg)         (deg)
C
C   Start: 2006 OCT 03 12:43:46.949483 UTC   56.104150191   20.000000187
C   Stop:  2006 OCT 03 12:44:42.288747 UTC   56.299961806   20.000000155
C
C   Start: 2006 OCT 08 16:03:33.956839 UTC   56.489554846   20.000000207
C   Stop:  2006 OCT 08 16:04:29.495919 UTC   56.687545101   19.999999969
C
C   Start: 2006 OCT 13 19:23:24.634854 UTC   56.887410591   19.999999879
C   Stop:  2006 OCT 13 19:24:12.492952 UTC   57.059318573   20.000000174
C
C   Start: 2006 OCT 18 22:43:21.631086 UTC   57.309244667   20.000000118
C   Stop:  2006 OCT 18 22:43:47.966990 UTC   57.404572725   20.000000043
C
C   Start: 2006 NOV 14 15:39:44.153177 UTC   54.328758385   19.999999935
C   Stop:  2006 NOV 14 15:40:10.446479 UTC   54.426680766   19.999999896
C
C   Start: 2006 NOV 19 18:59:10.190551 UTC   54.630961112   20.000000067
C   Stop:  2006 NOV 19 18:59:54.776369 UTC   54.798407529   19.999999848
C
C   Start: 2006 NOV 24 22:18:38.342454 UTC   54.949599996   19.999999822
C   Stop:  2006 NOV 24 22:19:30.964843 UTC   55.148838833   20.000000029
C
C   Start: 2006 NOV 30 01:38:07.309245 UTC   55.280547838   19.999999832
C   Stop:  2006 NOV 30 01:39:03.296253 UTC   55.494189248   19.999999989
C
C
C
C$ Restrictions
C
C     1) The kernel files to be used by this routine must be loaded
C        (normally using the SPICELIB routine FURNSH) before this
C        routine is called.
C
C     2) This routine has the side effect of re-initializing the
C        illumination angle utility package. Callers may 
C        need to re-initialize the package after calling this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 20-NOV-2012 (NJB) (BVS) (EDW)
C
C-&
 
C$ Index_Entries
C
C     solve for illumination_angle constraints
C     solve for phase_angle constraints
C     solve for solar_incidence_angle constraints
C     solve for incidence_angle constraints
C     solve for emission_angle constraints
C     search using illumination_angle constraints
C     search using lighting_angle constraints
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
      PARAMETER           ( QNPARS = 8 )

      LOGICAL               NOBAIL
      PARAMETER           ( NOBAIL = .FALSE. )
      
      LOGICAL               NORPT
      PARAMETER           ( NORPT  = .FALSE. )

C
C     Local variables
C


C
C     Quantity definition parameter arrays:
C
      CHARACTER*(LNSIZE)    QCPARS ( QNPARS )
      CHARACTER*(LNSIZE)    QPNAMS ( QNPARS )

      DOUBLE PRECISION      QDPARS ( QNPARS )

      INTEGER               QIPARS ( QNPARS )

      LOGICAL               QLPARS ( QNPARS )

C
C     Other local variables
C
      DOUBLE PRECISION      TOL

      LOGICAL               OK


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'GFILUM' )

C
C     Check the result window's size.
C
      IF ( SIZED(RESULT) .LT. 2 ) THEN

         CALL SETMSG ( 'Result window size must be at least 2 '
     .   //            'but was #.'                            )
         CALL ERRINT ( '#',  SIZED(RESULT)                     )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'               )
         CALL CHKOUT ( 'GFILUM'                                )
         RETURN

      END IF
      
C
C     Check the workspace window dimensions.
C
      IF ( MW .LT. 2 ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2.'                               )
         CALL ERRINT ( '#',  MW                                    )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFILUM'                                    )
         RETURN

      END IF

      IF ( NW .LT. NWILUM ) THEN

         CALL SETMSG ( 'Workspace window count was #; count must be '
     .   //            'at least #.'                                 )
         CALL ERRINT ( '#',  NW                                      )
         CALL ERRINT ( '#',  NWILUM                                  )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                     )
         CALL CHKOUT ( 'GFILUM'                                      )
         RETURN

      END IF

C
C     Set up a call to GFEVNT, which will handle the search.
C
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = TARGET

      QPNAMS(2) = 'ILLUM'
      QCPARS(2) = ILLMN

      QPNAMS(3) = 'OBSERVER'
      QCPARS(3) =  OBSRVR

      QPNAMS(4) = 'ABCORR'
      QCPARS(4) =  ABCORR

      QPNAMS(5) = 'REFERENCE FRAME'
      QCPARS(5) =  FIXREF

      QPNAMS(6) = 'ANGTYP'
      QCPARS(6) =  ANGTYP

      QPNAMS(7) = 'METHOD'
      QCPARS(7) =  METHOD

C
C     Copy SPOINT to elements 1-3 of the QDPARS array.
C
      QPNAMS(8) = 'SPOINT'
      CALL MOVED ( SPOINT, 3, QDPARS )

C
C     Set the step size.
C
      IF ( STEP .LE. 0.D0 ) THEN

         CALL SETMSG ( 'Step size was #; step size must be positive.' )
         CALL ERRDP  ( '#',  STEP                                     )
         CALL SIGERR ( 'SPICE(INVALIDSTEP)'                           )
         CALL CHKOUT ( 'GFILUM'                                       )
         RETURN

      END IF

      CALL GFSSTP ( STEP ) 

C
C     Retrieve the convergence tolerance, if set.
C
      CALL ZZHOLDD ( ZZGET, GF_TOL, OK, TOL )

C
C     Use the default value CNVTOL if there's no stored tolerance
C     value.
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
      CALL GFEVNT ( GFSTEP,  GFREFN,  'ILLUMINATION ANGLE',  QNPARS,   
     .              QPNAMS,  QCPARS,  QDPARS,                QIPARS,   
     .              QLPARS,  RELATE,  REFVAL,                TOL, 
     .              ADJUST,  CNFINE,  NORPT,                 GFREPI,   
     .              GFREPU,  GFREPF,  MW,                    NWILUM,
     .              WORK,    NOBAIL,  GFBAIL,                RESULT )

      CALL CHKOUT ( 'GFILUM' )
      RETURN
      END




