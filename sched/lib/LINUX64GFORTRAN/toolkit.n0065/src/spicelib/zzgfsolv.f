C$Procedure ZZGFSOLV ( Private --- GF, event finding routine )

      SUBROUTINE ZZGFSOLV ( UDCOND,   UDSTEP,  UDREFN,  BAIL,
     .                      UDBAIL,   CSTEP,   STEP,    START,
     .                      FINISH,   TOL,     RPT,     UDREPU,
     .                      RESULT )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine is a root finding general purpose event location
C     routine. Most of the HARD work has been delegated to other
C     routines (In particular, how the dynamic step size is chosen).
C
C     Sister routine to ZZGFSOLVX. Copy any edits to ZZGFSOLV or
C     ZZGFSOLVX to the sister routine.
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
C     ROOT
C     SEARCH
C     WINDOWS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      EXTERNAL              UDCOND
      EXTERNAL              UDSTEP
      EXTERNAL              UDREFN
      LOGICAL               UDBAIL
      LOGICAL               BAIL
      EXTERNAL              UDBAIL
      LOGICAL               CSTEP
      DOUBLE PRECISION      STEP
      LOGICAL               RPT
      EXTERNAL              UDREPU
      DOUBLE PRECISION      START
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      RESULT  ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UDCOND     I   Name of the routine that compares the current state
C                    condition with-respect-to a constraint.
C     UDSTEP     I   Name of the routine that computes a time step
C     UDREFN     I   Name of the routine that computes a refined time.
C     BAIL       I   Logical indicating program interrupt monitoring.
C     UDBAIL     I   Name of a routine that signals a program interrupt.
C     CSTEP      I   Logical indicating constant step size.
C     STEP       I   Constant step size in seconds for finding geometric
C                    events.
C     START      I   Beginning of the search interval.
C     FINISH     I   End of the search interval.
C     TOL        I   Maximum error in detection of state transitions.
C     RPT        I   Progress reporter on ( .TRUE.) or off ( .FALSE. )
C     UDREPU     I   Function that updates the progress report.
C     RESULT    I-O   SPICE window containing results.
C
C$ Detailed_Input
C
C     The first three inputs to this routine are names of
C     subroutines that this routine will call.  These routines
C     should meet the following specifications.
C
C     UDCOND     the routine that determines if the system state
C                satisfies some constraint condition at epoch ET.
C
C                The calling sequence:
C
C                   CALL UDCOND ( ET, IN_CON )
C
C                where:
C
C                   ET       a double precision value representing
C                            ephemeris time, expressed as seconds past
C                            J2000 TDB, at which to evaluate the state.
C
C                   IN_CON   a logical value indicating whether
C                            or not the quantity satisfies the 
C                            constraint at ET (TRUE) or not (FALSE).
C
C     UDSTEP     the routine that computes a time step in an attempt to
C                find a transition of the state of the specified 
C                coordinate. In the context of this routine's algorithm,
C                a "state transition" occurs where the geometric state 
C                changes from being in the desired geometric condition 
C                event to not, or vice versa.
C
C                This routine relies on UDSTEP returning step sizes
C                small enough so that state transitions within the
C                confinement window are not overlooked.  There must
C                never be two roots A and B separated by less than
C                STEP, where STEP is the minimum step size returned by
C                UDSTEP for any value of ET in the interval [A, B].
C
C                The calling sequence for UDSTEP is:
C
C                   CALL UDSTEP ( ET, STEP )
C
C                where:
C
C                   ET      a double precision value representing
C                           ephemeris time, expressed as seconds past
C                           J2000 TDB, from which the algorithm is to 
C                           search forward for a state transition. 
C
C                   STEP    is the output step size. STEP indicates
C                           how far to advance ET so that ET and
C                           ET+STEP may bracket a state transition and
C                           definitely do not bracket more than one
C                           state transition. Units are TDB seconds.
C
C                If a constant step size is desired, the routine
C
C                   GFSTEP
C
C                may be used. This is the default option. If using
C                GFSTEP, the step size must be set by calling
C
C                   GFSSTP(STEP)
C
C                prior to calling this routine.
C
C     UDREFN     the routine that computes a refinement in the times
C                that bracket a transition point. In other words, once
C                a pair of times have been detected such that the system
C                is in different states at each of the two times, UDREFN
C                selects an intermediate time which should be closer to
C                the transition state than one of the two known times.
C                The calling sequence for UDREFN is:
C
C                   CALL UDREFN ( T1, T2, S1, S2, T )
C
C                where the inputs are:
C
C                   T1    a time when the system is in state S1.
C
C                   T2    a time when the system is in state S2. T2
C                         is assumed to be larger than T1.
C
C                   S1    a logical indicating the state of the system
C                         at time T1.
C
C                   S2    a logical indicating the state of the system
C                         at time T2.
C
C                UDREFN may use or ignore the S1 and S2 values.
C
C                The output is:
C
C                   T     a time to check for a state transition
C                         between T1 and T2.
C
C                If a simple bisection method is desired, the routine
C                GFREFN may be used. This is the default option.
C
C     BAIL       is a logical indicating whether or not interrupt
C                signaling is enabled. When `bail' is set to TRUE,
C                the input function UDBAIL (see description below)
C                is used to determine whether an interrupt has been
C                issued.
C
C     UDBAIL     the routine that indicates whether an interrupt signal
C                has been issued (for example, from the keyboard).
C                UDBAIL has no arguments and returns a logical.
C                The return value is .TRUE. if an interrupt has
C                been issued; otherwise the value is .FALSE.
C
C                ZZGFSOLVX uses UDBAIL only when BAIL (see above) is set
C                to .TRUE., indicating that interrupt handling is
C                enabled. When interrupt handling is enabled, ZZGFSOLVX
C                and will call UDBAIL to determine whether to terminate
C                processing and return immediately.
C
C                If interrupt handing is not enabled, a logical
C                function must still be passed as an input argument.
C                The function
C
C                   GFBAIL
C
C                may be used for this purpose.
C
C     CSTEP      is a logical indicating whether or not the step size
C                used in searching is constant.  If it is, the value
C                STEP is used. Note that even if UDSTEP has the value
C                GFSTEP, i.e. the public, constant step routine, CSTEP
C                should still be .FALSE., in which case STEP is ignored.
C
C     STEP       is the step size to be used in the search. STEP must
C                be short enough for a search using this step size
C                to locate the time intervals where the geometric
C                event function is monotone increasing or decreasing.
C                However, STEP must not be *too* short, or the
C                search will take an unreasonable amount of time.
C
C                The choice of STEP affects the completeness but not
C                the precision of solutions found by this routine;
C                precision is controlled by the convergence
C                the tolerance, TOL.
C
C                STEP has units of TDB seconds.
C
C     START      is the beginning of the interval over which the state
C                is to be detected.
C
C     FINISH     is the end of the interval over which the state is
C                to be detected.
C
C     TOL        is a tolerance value used to determine convergence of
C                root-finding operations. TOL is measured in seconds
C                and is greater than zero.
C
C     RPT        is a logical variable which controls whether the
C                progress reporter is enabled. When RPT is TRUE,
C                progress reporting is enabled and the routine
C                UDREPU (see description  below) reports progress.
C
C     UDREPU     the routine that updates the progress report for a
C                search. The calling sequence of UDREPU is
C
C                   UDREPU (IVBEG, IVEND, ET )
C
C                   DOUBLE PRECISION      ET
C                   DOUBLE PRECISION      IVBEG
C                   DOUBLE PRECISION      IVEND
C
C                where ET is an epoch belonging to the confinement
C                window, IVBEG and IVEND are the start and stop times,
C                respectively of the current confinement window
C                interval.  The ratio of the measure of the portion
C                of CNFINE that precedes ET to the measure of CNFINE
C                would be a logical candidate for the searches
C                completion percentage; however the method of
C                measurement is up to the user.
C
C                If the user doesn't wish to provide a custom set of
C                progress reporting functions, the routine
C
C                   GFREPU
C
C                may be used.
C
C     RESULT     is an initialized SPICE window. RESULT may not be empty
C                on entry and must be large enough to hold all of the
C                intervals found by the search.
C
C$ Detailed_Output
C
C     RESULT     is a SPICE window containing the intersection of the
C                results of the search and the contents of RESULT
C                on entry.
C
C$ Parameters
C
C     LBCELL     is the SPICELIB cell lower bound.
C
C$ Exceptions
C
C     1)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE)
C         will signal.
C
C     2)  If START +/- TOL is indistinguishable from START or
C         FINISH +/- TOL is indistinguishable from FINISH, the
C         error SPICE(INVALIDVALUE) will signal.
C
C     3)  If START is greater than FINISH or SVDTIM is greater than
C         CURTIM, SPICE(BADTIMECASE) will signal.
C
C     4) If the inner convergence loop fails to converge to TOL
C        within MXLOOP iterations, the error SPICE(NOCONVERG)
C        will signal.
C
C$ Files
C
C     This routine computes states using SPK files that have been
C     loaded into the SPICE system, normally via the kernel loading
C     interface routine FURNSH. See the routine FURNSH and the SPK
C     and KERNEL Required Reading for further information on loading
C     (and unloading) kernels.
C
C$ Particulars
C
C     This routine implements a strategy for searching for geometric
C     state events important for planning solar system observations.
C     The actual details of selecting time steps while searching for
C     a state change as well as the scheme used for zeroing in on the
C     actual time of transition are handled by lower level routines.
C
C     By delegating the work of selecting search time steps and the
C     process of refining a transition time estimate to lower level
C     routines, the common work of the search can be isolated here.
C     The routines that do the decision making, can be modified
C     and made smarter as time permits.
C
C$ Examples
C
C      See GFOCCE and ZZGFREL.
C
C$ Restrictions
C
C      It is important that the user understand how the routines
C      UDCOND, UDSTEP and UDREFN are to be used and that the
C      calling sequences match precisely with the descriptions given
C      here.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L. S. Elson    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0,  24-OCT-2010 (EDW)
C
C       TOL error check now returns SPICE(INVALIDTOLERANCE) instead of 
C       previous return SPICE(VALUEOUTOFRANGE).
C
C-    SPICELIB Version 1.0.1  21-DEC-2009 (EDW)
C
C        Edit to Abstract to document sister routine ZZGFSOLVX. Added
C        N.J. Bachman citation to Author_and_Institution section.
C
C-    SPICELIB Version 1.0.0, 17-MAR-2009 (EDW)(LSE)(NJB)
C
C-&

C$ Index_Entries
C
C     find times of an event
C
C-&

C
C     SPICELIB functions.
C

      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      TOUCHD

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      CURTIM
      DOUBLE PRECISION      SVDTIM
      DOUBLE PRECISION      T
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      TIMEST
      DOUBLE PRECISION      TRNSTN

      LOGICAL               CURSTE
      LOGICAL               INSTAT
      LOGICAL               S
      LOGICAL               STATE1
      LOGICAL               SAVST
      LOGICAL               L1
      LOGICAL               L2

      CHARACTER*(256)      CONTXT

      INTEGER              NLOOP

C
C     The maximum number of search loop iterations to execute.
C     The default refinement method is bisection, a very slow
C     method to convergence. Since 2**1000 ~ 10**301,
C     1000 loop iterations represents enough effort to assume
C     either the search will not converge or that the refinement
C     function operates slower than would bisection, in which
C     case the user should use the default GFREFN function.
C
      INTEGER               MXLOOP
      PARAMETER           ( MXLOOP = 1000 )

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFSOLV' )


C
C     Check the convergence tolerance.
C
      IF ( TOL .LE. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be positive but was #.' )
         CALL ERRDP  ( '#',  TOL                               )
         CALL SIGERR ( 'SPICE(INVALIDTOLERANCE)'               )
         CALL CHKOUT ( 'ZZGFSOLV'                              )
         RETURN

      END IF

C
C     Make sure that START is not greater than FINISH. Signal an
C     error for START > FINISH.
C
      IF ( START .GT. FINISH) THEN

         CALL SETMSG ( 'Bad time interval result, '      //
     .                 'START > FINISH.' )
         CALL SIGERR ( 'SPICE(BADTIMECASE)' )
         CALL CHKOUT ( 'ZZGFSOLV'           )
         RETURN

      END IF

C
C     Make sure that TOL is not too small, i.e. that neither
C     START + TOL nor START - TOL equals START.
C
      IF (  ( TOUCHD (START - TOL) .EQ. START )
     .     .OR.
     .      ( TOUCHD (START + TOL) .EQ. START ) ) THEN

         CALL SETMSG ('TOL has value #1. '                      //
     .                'This value is too small to distinguish ' //
     .                'START - TOL or START + TOL from '        //
     .                'START, #2.'                               )
         CALL ERRDP  ( '#1', TOL                                 )
         CALL ERRDP  ( '#2', START                               )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                     )
         CALL CHKOUT ('ZZGFSOLV'                                 )
         RETURN

      END IF


C
C     Make sure that TOL is not too small, i.e. that neither
C     FINISH + TOL nor FINISH - TOL equals FINISH.
C
      IF (  ( TOUCHD (FINISH - TOL) .EQ. FINISH )
     .     .OR.
     .      ( TOUCHD (FINISH + TOL) .EQ. FINISH ) ) THEN

         CALL SETMSG ('TOL has value #1. '                      //
     .                'This value is too small to distinguish ' //
     .                'FINISH - TOL or FINISH + TOL from '      //
     .                'FINISH, #2.'                              )
         CALL ERRDP  ( '#1', TOL                                 )
         CALL ERRDP  ( '#2', FINISH                              )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                     )
         CALL CHKOUT ('ZZGFSOLV'                                 )
         RETURN

      END IF


C
C     If active, update the progress reporter.
C
      IF ( RPT ) THEN
         CALL UDREPU ( START, FINISH, START )
      END IF

C
C     This algorithm determines those intervals when a given state
C     is observed to occur within a specified search interval.
C
C     Pairs of times are recorded.  The first member of each pair
C     denotes the time when the system changes to the state of
C     interest.  The second denotes a transition out of that state.
C
C     If the system is in the state of interest at the beginning of
C     the interval, the beginning of the time interval will be
C     recorded.  This may or may not be a transition point.
C
C     Similarly if the system is in the state of interest at the end
C     of the interval, the end of the interval will be recorded.
C     Again, this may or may not be a transition point.
C

C
C     Initially the current time is the beginning of the search
C     interval.
C
      CURTIM = START

C
C     Determine if the state at the current time satisfies some
C     constraint. This constraint may indicate only existence of
C     a state.
C
      CALL UDCOND ( CURTIM,  CURSTE )

      IF ( FAILED() ) THEN
         CALL CHKOUT (  'ZZGFSOLV' )
         RETURN
      END IF

C
C     If the system is in the state of interest, record the initial
C     time of the search interval.
C
      IF ( CURSTE ) THEN

         INSTAT = .TRUE.
         BEGIN  = CURTIM

      ELSE

         INSTAT = .FALSE.

      END IF

C
C     If the step size is constant, use the value supplied.
C
      IF ( CSTEP ) THEN
         TIMEST = STEP
      END IF

C
C     Save the current time and state somewhere.
C
      SVDTIM  = CURTIM
      SAVST   = CURSTE

C
C     Once initializations have been performed keep working
C     until the search interval has been exhausted.
C
C     While time remains in the search interval.
C
      DO WHILE ( SVDTIM .LT. FINISH )

C
C        Using the current window and internally stored
C        information about the current state, select a new current
C        time.
C
         IF ( .NOT. CSTEP ) THEN

            CALL UDSTEP ( CURTIM, TIMEST )

            IF ( FAILED() ) THEN
               CALL CHKOUT (  'ZZGFSOLV' )
               RETURN
            END IF

         END IF

C
C        Add the time step to the current time.  Make sure that the
C        time does not move beyond the end of the search interval.
C

         CURTIM = MIN ( CURTIM + TIMEST, FINISH )

C
C        Compute the state at time CURTIM.
C
         CALL UDCOND ( CURTIM,  CURSTE )

         IF ( FAILED() ) THEN
            CALL CHKOUT (  'ZZGFSOLV' )
            RETURN
         END IF

C
C        While the state remains unchanged and the interval is not
C        completely searched ...
C

         DO WHILE ( ( SAVST .EQV. CURSTE )
     .              .AND.
     .              ( SVDTIM .LT. FINISH ) )

C
C           First check for an interrupt signal if checking is enabled.
C
            IF ( BAIL )  THEN
               IF ( UDBAIL () ) THEN
                  CALL CHKOUT (  'ZZGFSOLV' )
                  RETURN
               END IF
            END IF

C
C           Report the current time to the monitoring utility, if
C           appropriate.
C
            IF ( RPT ) THEN
               CALL UDREPU ( START, FINISH, SVDTIM )
            END IF

C
C           Save the current time and state somewhere.
C
            SVDTIM  = CURTIM
            SAVST   = CURSTE

C
C           Compute a new current time so that we will not step
C           past the end of the interval.  This time will be
C           based on:
C
C                 1. The kind of event we are looking for.
C                 2. The objects and observer class.
C                 3. Transition times already found.
C                 4. A minimum time step allowed.
C
            IF ( .NOT. CSTEP ) THEN

               CALL UDSTEP ( CURTIM, TIMEST )

               IF ( FAILED() ) THEN
                  CALL CHKOUT (  'ZZGFSOLV' )
                  RETURN
               END IF

            END IF

            CURTIM = MIN ( CURTIM + TIMEST, FINISH )

C
C           Compute the current state
C
            CALL UDCOND ( CURTIM,  CURSTE )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFSOLV' )
               RETURN
            END IF

C
C           Loop back to see if the state has changed.
C

         END DO

C
C        If we have detected a state change and not merely run out
C        of the search interval...
C

         IF ( SAVST .NEQV. CURSTE ) THEN

C
C           Call the previous state STATE1
C           Call the current  state STATE2
C
C           Call the time at state STATE1, T1
C           Call the time at state STATE2, T2
C
C           Save the current time.
C
            STATE1 = SAVST
            T1     = SVDTIM
            T2     = CURTIM

C
C           Make sure that T1 is not greater than T2. Signal an
C           error for T1 > T2.
C

            IF ( T1 .GT. T2) THEN
               CALL SETMSG ( 'Bad time interval result, T1 > T2.' )
               CALL SIGERR ( 'SPICE(BADTIMECASE)' )
               CALL CHKOUT ( 'ZZGFSOLV'           )
               RETURN
            END IF

            SVDTIM  =  CURTIM
            SAVST   =  CURSTE

C
C           T1 and T2 bracket the time of transition.  Squeeze this
C           interval down until it is less than some tolerance in
C           length.  Do it as described below...
C
C           Loop while the difference between the times T1 and T2
C           exceeds a specified tolerance.
C

            NLOOP = 0

            DO WHILE ( TOUCHD (T2 - T1) .GT. TOL )

               NLOOP = NLOOP + 1

C
C              This loop count error exists to catch pathologies
C              in the refinement function. The default bisection
C              refinement will converge before 1000 iterations if
C              a convergence is numerically possible. Any other
C              refinement function should require fewer iterations
C              compared to bisection. If not, the user should
C              probably use bisection.
C
               IF ( NLOOP .GE. MXLOOP ) THEN
                  CALL SETMSG ( 'Loop run exceeds maximum loop count. '
     .         //               'Unable to converge to TOL value #1 '
     .         //               'within MXLOOP value #2 iterations.')
                  CALL ERRDP  ( '#1', TOL          )
                  CALL ERRINT ( '#2', MXLOOP       )
                  CALL SIGERR ( 'SPICE(NOCONVERG)' )
                  CALL CHKOUT ( 'ZZGFSOLV'         )
                  RETURN
               END IF


               IF ( BAIL )  THEN
                  IF ( UDBAIL () ) THEN
                     CALL CHKOUT (  'ZZGFSOLV' )
                     RETURN
                  END IF
               END IF

C
C              Select a time T, between T1 and T2 (possibly based on the
C              values of L1 and L2).
C
               CALL UDREFN ( T1, T2, L1, L2, T )

C
C              Check for an error signal. The default refinement
C              routine, GFREFN, does not include error checks.
C
               IF ( FAILED() ) THEN
                  CALL CHKOUT (  'ZZGFSOLV' )
                  RETURN
               END IF

C
C              Check whether T is between T1 and T2.  If
C              not then assume that we have gone as far as
C              we can in refining our estimate of the transition
C              point. Set T1 and T2 equal to T.
C

               T = BRCKTD ( T, T1, T2 )

               IF ( T .EQ. T1 ) THEN

                  T2 = T

               ELSE IF ( T .EQ. T2 ) THEN

                  T1 = T

               ELSE

C
C                 Compute the state time T. If this state, S,
C                 equals STATE1, set T1 to T, otherwise set
C                 T2 to T.
C
                  CALL UDCOND ( T,  S )

                  IF ( S .EQV. STATE1 ) THEN

                     T1  = T

                  ELSE

                     T2 = T

                  END IF

               END IF

            END DO

C
C           Let TRNSTN be the midpoint of [T1, T2].  Record this
C           time as marking the transition from STATE1 to STATE2.
C
            TRNSTN = BRCKTD( (T1 + T2)*0.5D0, T1, T2 )

C
C           In state-of-interest or not?
C
            IF ( INSTAT )  THEN
C
C              We were in the state of interest, TRNSTN marks the
C              point in time when the state changed to "not of
C              interest" We need to record the interval from BEGIN to
C              FINISH and note that we are no longer in the state of
C              interest.
C

C
C              Add an interval starting at BEGIN and ending at TRNSTN
C              to the result window.
C

               CONTXT = 'Adding interval [BEGIN,TRNSTN] to RESULT. '
     .         //       'TRNSTN represents time of passage out of the '
     .         //       'state-of-interest.'

               CALL ZZWNINSD ( BEGIN, TRNSTN, CONTXT, RESULT )

            ELSE
C
C              We were not in the state of interest.  As a result
C              TRNSTN marks the point where we are changing to
C              the state of interest.  Note that we have transitioned
C              to the state of interest and record the time at
C              which the transition occurred.
C
               BEGIN  = TRNSTN

            END IF

C
C           A transition occurred either from from in-state to
C           out-of-state or the inverse. Reverse the value of the
C           INSTAT flag to signify the transition event.
C
            INSTAT = .NOT. INSTAT

C
C        That's it for this detection of state change.
C
         END IF

C
C        Continue if there is more time in the search interval.
C
      END DO


C
C     Check if in-state at this time (FINISH). If so record the
C     interval.
C

      IF ( INSTAT ) THEN

C
C        Add an interval starting at BEGIN and ending at FINISH to the
C        window.
C
         CONTXT = 'Adding interval [BEGIN,FINISH] to RESULT. FINISH '
     .         // 'represents end of the search interval.'

         CALL ZZWNINSD ( BEGIN, FINISH, CONTXT, RESULT )

      END IF

C
C     If active, update the progress reporter before exiting this
C     routine.
C

      IF ( RPT ) THEN
         CALL UDREPU ( START, FINISH, FINISH )
      END IF

C
C     Check-out then return.
C
      CALL CHKOUT (  'ZZGFSOLV' )
      RETURN
      END

