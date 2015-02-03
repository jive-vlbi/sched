C$Procedure ZZGFUDB ( Private --- GF, general use boolean search )

      SUBROUTINE ZZGFUDB ( UDFUNS, UDFUNB, TOL,     UDSTEP,  UDREFN,
     .                     RPT,    UDREPI, UDREPU,  UDREPF,
     .                     BAIL,   UDBAIL, CNFINE,  RESULT )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Routine to determine time intervals when a user defined boolean
C     function returns true. Report progress and handle interrupts
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
C     None.
C
C$ Keywords
C
C     GF
C     TIME
C     WINDOWS
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      EXTERNAL              UDFUNS
      EXTERNAL              UDFUNB
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
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UDFUNS     I   Name of the routine that computes a scalar
C                    quantity of interest.
C     UDFUNB     I   Name of the routine that computes the boolean
C                    value of interest.
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
C     UDFUNS     the routine that returns the value of the scalar
C                quantity of interest at time ET. The calling sequence
C                for UDFUNC is:
C
C                   CALL UDFUNS ( ET, VALUE )
C
C                where:
C
C                   ET      a double precision value representing
C                           ephemeris time, expressed as seconds past
C                           J2000 TDB at which to determine the scalar
C                           value.
C
C                   VALUE   is the value of the scalar quantity
C                           at ET.
C
C
C     UDFUNB     the user defined routine returning a boolean value
C                for an epoch ET.
C
C                The calling sequence:
C
C                   CALL UDFUNB ( UDFUNS, ET, BOOL )
C
C                where:
C
C                   UDFUNS   a scalar function as previously defined.
C
C                   ET       a double precision value representing
C                            ephemeris time, expressed as seconds past
C                            J2000 TDB, at which to evaluate UDFUNB.
C
C                   BOOL     the boolean value at ET.
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
C                changes from being "in state" to being "not in
C                state" or vice versa.
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
C                are used, if SRCPRE and SRCSUF are, respectively,
C
C                   "User defined boolean event search"
C                   "done."
C
C                the progress report display at the end of the
C                search will be:
C
C                   User defined boolean event search 100.00% done.
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
C                GFUDB uses UDBAIL only when BAIL (see above) is set
C                to .TRUE., indicating that interrupt handling is
C                enabled. When interrupt handling is enabled, GFUDB
C                and routines in its call tree will call UDBAIL to
C                determine whether to terminate processing and return
C                immediately.
C
C                If interrupt handing is not enabled, a logical
C                function must still be passed to GFUDB as
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
C
C$ Detailed_Output
C
C     RESULT     is a SPICE window representing the set of time
C                intervals, within the confinement period, when the
C                specified boolean function has a value true.
C
C                The endpoints of the time intervals comprising RESULT
C                are interpreted as seconds past J2000 TDB.
C
C                If RESULT is non-empty on input, its contents
C                will be discarded before GFUDB conducts its
C                search.
C
C$ Parameters
C
C     LBCELL     is the SPICELIB cell lower bound.
C
C$ Exceptions
C
C     1)  SPICE(INVALIDTOLERANCE) will signal if the convergence
C         tolerance value is non-positive.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     If the boolean function requires access to ephemeris data:
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
C     This routine provides the SPICE GF system's most flexible
C     interface for searching for boolean events based on a user
C     defined boolean function.
C
C     Applications that do not require support for progress
C     reporting, interrupt handling, non-default step refinement
C     functions, or non-default convergence tolerance normally should
C     call GFUDB rather than this routine.
C
C     This routine determines a set of one or more time intervals
C     within the confinement window when the boolean function
C     satisfies a caller-specified constraint. The resulting set of
C     intervals is returned as a SPICE window.
C
C     Below we discuss in greater detail aspects of this routine's
C     solution process that are relevant to correct and efficient
C     use of this routine in user applications.
C
C     The Search Process
C     ==================
C
C     The search for boolean events is treated as a search for state
C     transitions: times are sought when the boolean function value
C     changes from true to false or vice versa.
C
C     Step Size
C     =========
C
C     Each interval of the confinement window is searched as follows:
C     first, the input step size is used to determine the time
C     separation at which the boolean function will be sampled.
C     Starting at the left endpoint of the interval, samples of the
C     boolean function will be taken at each step. If a state change
C     is detected, a root has been bracketed; at that point, the
C     "root"--the time at which the state change occurs---is found by a
C     refinement process, for example, via binary search.
C
C     Note that the optimal choice of step size depends on the lengths
C     of the intervals over which the boolean function is constant:
C     the step size should be shorter than the shortest such interval
C     and the shortest separation between the intervals, within
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
C     interval within which a solution is sought.
C
C     The confinement window also can be used to restrict a search to
C     a time window over which required data are known to be
C     available.
C
C     In some cases, the confinement window be used to make searches
C     more efficient. Sometimes it's possible to do an efficient search
C     to reduce the size of the time period over which a relatively
C     slow search of interest must be performed. See the "CASCADE"
C     example program in gf.req for a demonstration.
C
C$ Examples
C
C    Refer to GFUDB.
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
C-   SPICELIB Version 1.0.0  17-OCT-2013 (EDW)
C
C       Logic and implementation based on GFOCCE by Nat Bachman.
C
C-&

C$ Index_Entries
C
C   GF user defined boolean function search
C
C-&

C
C     SPICELIB functions
C
      INTEGER               WNCARD
      LOGICAL               FAILED
      LOGICAL               RETURN


C
C     Local parameters
C

C
C     STEP is a step size initializer for the unused, dummy step size
C     argument to ZZGFSOLVX. The routine UDSTEP, which is passed to
C     ZZGFSOLVX, will be used by that routine to obtain the step size.
C
      DOUBLE PRECISION      STEP
      PARAMETER           ( STEP   = 1.D0 )

C
C     CSTEP indicates whether a constant step size, provided
C     via the input argument STEP, is to be used by ZZGFSOLVX.
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

      CALL CHKIN  ( 'ZZGFUDB' )

C
C     Check the convergence tolerance.
C
      IF ( TOL .LE. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be positive but was #.' )
         CALL ERRDP  ( '#',  TOL                               )
         CALL SIGERR ( 'SPICE(INVALIDTOLERANCE)'               )
         CALL CHKOUT ( 'ZZGFUDB'                               )
         RETURN

      END IF


C
C     Prepare the progress reporter if appropriate.
C
      IF ( RPT ) THEN
         CALL UDREPI ( CNFINE, 'User defined boolean event search ',
     .                         'done.' )
      END IF

C
C     Cycle over the intervals in the confining window.
C
      COUNT = WNCARD(CNFINE)

      DO I = 1, COUNT

C
C        Retrieve the bounds for the Ith interval of the confinement
C        window. Search this interval for boolean events. Union the
C        result with the contents of the RESULT window.
C
         CALL WNFETD ( CNFINE, I, START, FINISH )

C
C        Call ZZGFSOLVX to do the event detection work. The boolean
C        function passes as UDFUNB, the scalar as UDFUNS.
C

         CALL ZZGFSOLVX ( UDFUNS, UDFUNB,
     .                    UDSTEP, UDREFN,
     .                    BAIL,   UDBAIL,
     .                    CSTEP,  STEP,
     .                    START,  FINISH,
     .                    TOL,
     .                    RPT,    UDREPU,
     .                    RESULT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFUDB'  )
            RETURN
         END IF

         IF ( BAIL ) THEN
C
C           Interrupt handling is enabled.
C
            IF ( UDBAIL() ) THEN
C
C              An interrupt has been issued. Return now regardless of
C              whether the search completed.
C
               CALL CHKOUT ( 'ZZGFUDB' )
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

      CALL CHKOUT ( 'ZZGFUDB' )
      RETURN
      END

