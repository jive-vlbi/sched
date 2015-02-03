C$Procedure GFUDB ( GF, user defined boolean )

      SUBROUTINE GFUDB ( UDFUNS, UDFUNB, STEP, CNFINE, RESULT )

C$ Abstract
C
C     Perform a GF search on a user defined boolean quantity.
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

      EXTERNAL              UDFUNS
      EXTERNAL              UDFUNB
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      DOUBLE PRECISION      RESULT ( LBCELL : * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LBCELL     P   SPICE Cell lower bound.
C     CNVTOL     P   Convergence tolerance.
C     UDFUNS     I   Name of the routine that computes a scalar
C                    quantity corresponding to an ET.
C     UDFUNB     I   Name of the routine returning the boolean value
C                    corresponding to an ET.
C     STEP       I   Constant step size in seconds for finding geometric
C                    events.
C     CNFINE     I   SPICE window to which the search is restricted.
C     RESULT    I-O  SPICE window containing results.
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
C                           J2000 TDB at which to evaluate UDFUNS.
C
C                   VALUE   is the value of the scalar quantity
C                           at ET.
C
C     UDFUNB     the user defined routine returning a boolean value
C                for an epoch ET. The calling sequence for UNFUNB is:
C
C                   CALL UDFUNB ( UDFUNS, ET, BOOL )
C
C                where:
C
C                   UDFUNS   the name of the scalar function as
C                            defined above.
C
C                   ET       a double precision value representing
C                            ephemeris time, expressed as seconds past
C                            J2000 TDB, at which to evaluate UDFUNB.
C
C                   BOOL     the boolean value at ET.
C
C                GFUDB will correctly operate only for boolean
C                functions with true conditions defining non zero
C                measure time intervals.
C
C                Note, UDFUNB need not call UDFUNS. The use of UDFUNS
C                is determined by the needs of the calculation and
C                the user's design.
C
C     STEP       the step size to be used in the search. STEP must
C                be shorter than any interval, within the confinement
C                window, over which the user defined boolean function
C                is met. In other words, STEP must be shorter than the
C                shortest time interval for which the boolean function
C                is true; STEP must also be shorter than the shortest
C                time interval between two boolean function true events
C                occurring within the confinement window (see below).
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
C                will be discarded before GFUDB conducts its search.
C
C$ Detailed_Output
C
C     RESULT     is a SPICE window containing the time intervals within
C                the confinement window, during which the specified
C                boolean quantity is true.
C
C                If no times within the confinement window satisfy the
C                search, RESULT will be returned with a cardinality
C                of zero.
C
C$ Parameters
C
C     LBCELL     the integer value defining the lower bound for
C                SPICE Cell arrays (a SPICE window is a kind of cell).
C
C     CNVTOL     is the convergence tolerance used for finding
C                endpoints of the intervals comprising the result
C                window. CNVTOL is used to determine when binary
C                searches for roots should terminate: when a root is
C                bracketed within an interval of length CNVTOL, the
C                root is considered to have been found.
C
C                The accuracy, as opposed to precision, of roots found
C                by this routine depends on the accuracy of the input
C                data. In most cases, the accuracy of solutions will be
C                inferior to their precision.
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
C     3)  If the size of the SPICE window RESULT is less than 2 or
C         not an even value, the error SPICE(INVALIDDIMENSION) will
C         signal. If RESULT has insufficient capacity to contain the
C         number of intervals on which the specified condition
C         is met, the error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     4)  If required ephemerides or other kernel data are not
C         available, an error is signaled by a routine in the call tree
C         of this routine.
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
C     This routine determines a set of one or more time intervals
C     within the confinement window when the boolean function
C     evaluates to true. The resulting set of intervals is returned
C     as a SPICE window.
C
C     Below we discuss in greater detail aspects of this routine's
C     solution process that are relevant to correct and efficient
C     use of this routine in user applications.
C
C     UDFUNS Default Template
C     =======================
C
C     The boolean function includes an argument for an input scalar
C     function. Use of a scalar function during the evaluation of
C     the boolean function is not required. SPICE provides a no-op
C     scalar routine, UDF, as a dummy argument for instances when
C     the boolean function does not need to call the scalar function.
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
C     In some cases, the confinement window can be used to make
C     searches more efficient. Sometimes it's possible to do an
C     efficient search to reduce the size of the time period over
C     which a relatively slow search of interest must be performed.
C     See the "CASCADE" example program in gf.req for a demonstration.
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
C              KERNELS_TO_LOAD = ( 'de418.bsp',
C                                  'pck00009.tpc',
C                                  'naif0009.tls'  )
C
C           \begintext
C
C     Example(1):
C
C     Calculate the time intervals when the position of the moon
C     relative to the earth in the IAU_EARTH frame has a positive value
C     for the Z position component, also with a positive value for the
C     Vz velocity component.
C
C
C        Code:
C
C           PROGRAM GFUDB_T
C
C           EXTERNAL                  UDF
C           EXTERNAL                  GFB
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
C           PARAMETER           ( MAXWIN = 100 )
C
C           DOUBLE PRECISION      LEFT
C           DOUBLE PRECISION      RIGHT
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      ETS
C           DOUBLE PRECISION      ETE
C           DOUBLE PRECISION      STEP
C           DOUBLE PRECISION      STATE (6)
C           DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C
C           INTEGER               I
C
C           CHARACTER*(32)        UTC
C
C     C
C     C     SPICELIB functions.
C     C
C           INTEGER               WNCARD
C           DOUBLE PRECISION      SPD
C
C     C
C     C     Initialize windows.
C     C
C           CALL SSIZED ( MAXWIN, RESULT )
C           CALL SSIZED ( 2,      CNFINE )
C
C
C     C
C     C     Load needed kernels.
C     C
C           CALL FURNSH ( 'standard.tm' )
C
C     C
C     C     Store the time bounds of our search interval in
C     C     the confinement window. One year, 2011.
C     C
C           CALL STR2ET ( 'Jan 1 2011', ETS )
C           CALL STR2ET ( 'Jan 1 2012', ETE )
C           CALL WNINSD ( ETS, ETE, CNFINE )
C
C     C
C     C     The moon orbit about the earth-moon barycenter is
C     C     twenty-eight days. The event condition occurs
C     C     during (very) approximately a quarter of the orbit. Use
C     C     a step of five days.
C     C
C           STEP = 5.D0 * SPD()
C
C           CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
C
C           IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C                 WRITE (*, '(A)') 'Result window is empty.'
C
C           ELSE
C
C              DO I = 1, WNCARD(RESULT)
C
C     C
C     C           Fetch and display each RESULT interval.
C     C
C                 CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C                 WRITE (*,*) 'Interval ', I
C
C                 CALL ET2UTC ( LEFT, 'C', 4, UTC )
C                 WRITE (*, *) '   Interval start: ', UTC
C
C                 CALL SPKEZ ( 301, LEFT, 'IAU_EARTH', 'NONE', 399,
C          .                   STATE, LT )
C                 WRITE (*, *) '                Z= ', STATE(3)
C                 WRITE (*, *) '               Vz= ', STATE(6)
C
C                 CALL ET2UTC ( RIGHT, 'C', 4, UTC )
C                 WRITE (*, *) '   Interval end  : ', UTC
C
C                 CALL SPKEZ ( 301, RIGHT, 'IAU_EARTH', 'NONE', 399,
C          .                   STATE, LT )
C                 WRITE (*, *) '                Z= ', STATE(3)
C                 WRITE (*, *) '               Vz= ', STATE(6)
C                 WRITE (*, *) ' '
C
C              END DO
C
C           END IF
C
C           END
C
C
C
C     C-Procedure GFB
C     C
C     C     User defined boolean routine.
C     C
C
C           SUBROUTINE GFB ( UDFUNS, ET, BOOL )
C           IMPLICIT NONE
C
C     C- Abstract
C     C
C     C     User defined geometric boolean function:
C     C
C     C        Z >= 0 with dZ/dt > 0.
C     C
C
C           EXTERNAL              UDFUNS
C
C           DOUBLE PRECISION      ET
C           LOGICAL               BOOL
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
C
C     C
C     C     Initialization. Retrieve the vector from the earth to
C     C     the moon in the IAU_EARTH frame, without aberration
C     C     correction.
C     C
C           TARG   = 301
C           REF    = 'IAU_EARTH'
C           ABCORR = 'NONE'
C           OBS    = 399
C
C     C
C     C     Evaluate the state of TARG from OBS at ET with
C     C     correction ABCORR.
C     C
C           CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT )
C
C     C
C     C     Calculate the boolean value.
C     C
C           BOOL = (STATE(3) .GE. 0.D0) .AND. (STATE(6) .GT. 0.D0 )
C
C           RETURN
C
C           END
C
C     The program outputs:
C
C      Interval            1
C         Interval start: 2011 JAN 09 15:24:23.4155
C                      Z=  -3.67969050785177387E-008
C                     Vz=   0.39698408492943960
C         Interval end  : 2011 JAN 16 16:08:28.5634
C                      Z=    156247.48820202681
C                     Vz=   3.76859567857712463E-013
C
C      Interval            2
C         Interval start: 2011 FEB 05 23:17:57.3590
C                      Z=  -3.98442807636456564E-008
C                     Vz=   0.39678128322307005
C         Interval end  : 2011 FEB 13 01:38:28.4256
C                      Z=    157016.05516171581
C                     Vz=   3.22388166509868235E-013
C
C      Interval            3
C         Interval start: 2011 MAR 05 06:08:17.6680
C                      Z=  -1.16190221888246015E-008
C                     Vz=   0.39399025399881443
C         Interval end  : 2011 MAR 12 10:27:45.1887
C                      Z=    157503.77393430873
C                     Vz=  -3.41879302645509142E-013
C
C                        ...
C
C      Interval           12
C         Interval start: 2011 NOV 05 18:43:39.7428
C                      Z=  -1.80199890564836096E-008
C                     Vz=   0.37393762954280635
C         Interval end  : 2011 NOV 13 03:50:17.1540
C                      Z=    153172.08661820635
C                     Vz=  -3.62962481251227764E-013
C
C      Interval           13
C         Interval start: 2011 DEC 03 01:16:40.8174
C                      Z=   1.30391470065660542E-007
C                     Vz=   0.37425784503188919
C         Interval end  : 2011 DEC 10 09:51:07.7182
C                      Z=    152511.72037686800
C                     Vz=   2.11386680729064302E-013
C
C      Interval           14
C         Interval start: 2011 DEC 30 09:48:57.4099
C                      Z=   9.79434844339266419E-009
C                     Vz=   0.37733320145276139
C         Interval end  : 2012 JAN 01 00:00:00.0000
C                      Z=    50793.083312689421
C                     Vz=   0.35454996926793847
C
C
C     Example(2):
C
C     Calculate the time intervals when the Z component of the earth
C     to moon position vector in the IAU_EARTH frame has value
C     between -1000 km and 1000 km (e.g. above and below the equatorial
C     plane).
C
C
C        Code:
C
C           PROGRAM GFUDB_T2
C
C           EXTERNAL                  GFB
C           EXTERNAL                  GFQ
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
C           PARAMETER           ( MAXWIN = 100 )
C
C           DOUBLE PRECISION      LEFT
C           DOUBLE PRECISION      RIGHT
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      ETS
C           DOUBLE PRECISION      ETE
C           DOUBLE PRECISION      STEP
C           DOUBLE PRECISION      POS (3)
C           DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C
C           INTEGER               I
C
C           CHARACTER*(32)        UTC
C
C     C
C     C     SPICELIB functions.
C     C
C           INTEGER               WNCARD
C           DOUBLE PRECISION      SPD
C
C     C
C     C     Initialize windows.
C     C
C           CALL SSIZED ( MAXWIN, RESULT )
C           CALL SSIZED ( 2,      CNFINE )
C
C
C     C
C     C     Load needed kernels.
C     C
C           CALL FURNSH ( 'standard.tm' )
C
C     C
C     C     Store the time bounds of our search interval in
C     C     the confinement window. One year, 2011.
C     C
C           CALL STR2ET ( 'Jan 1 2011', ETS )
C           CALL STR2ET ( 'Jan 1 2012', ETE )
C           CALL WNINSD ( ETS, ETE, CNFINE )
C
C     C
C     C     The duration of the event is approximately ninety minutes.
C     C     Use a step of one hour.
C     C
C           STEP = 60.D0*60.D0
C
C           CALL GFUDB ( GFQ, GFB, STEP, CNFINE, RESULT )
C
C           IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C                 WRITE (*, '(A)') 'Result window is empty.'
C
C           ELSE
C
C              DO I = 1, WNCARD(RESULT)
C
C     C
C     C           Fetch and display each RESULT interval.
C     C
C                 CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C                 WRITE (*,*) 'Interval ', I
C
C                 CALL ET2UTC ( LEFT, 'C', 4, UTC )
C                 WRITE (*, *) '   Interval start: ', UTC
C
C                 CALL SPKEZP ( 301, LEFT, 'IAU_EARTH', 'NONE', 399,
C          .                   POS, LT )
C                 WRITE (*, *) '                Z= ', POS(3)
C
C                 CALL ET2UTC ( RIGHT, 'C', 4, UTC )
C                 WRITE (*, *) '   Interval end  : ', UTC
C
C                 CALL SPKEZP ( 301, RIGHT, 'IAU_EARTH', 'NONE', 399,
C          .                   POS, LT )
C                 WRITE (*, *) '                Z= ', POS(3)
C                 WRITE (*, *) ' '
C
C              END DO
C
C           END IF
C
C           END
C
C
C
C     C-Procedure GFQ
C     C
C     C     User defined scalar routine.
C     C
C
C           SUBROUTINE GFQ ( ET, VALUE )
C           IMPLICIT NONE
C
C     C- Abstract
C     C
C     C     Return the Z component of the POS vector.
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
C           DOUBLE PRECISION      POS ( 3 )
C           DOUBLE PRECISION      LT
C
C     C
C     C     Initialization. Retrieve the vector from the earth to
C     C     the moon in the IAU_EARTH frame, without aberration
C     C     correction.
C     C
C           TARG   = 301
C           REF    = 'IAU_EARTH'
C           ABCORR = 'NONE'
C           OBS    = 399
C
C     C
C     C     Evaluate the position of TARG from OBS at ET with
C     C     correction ABCORR.
C     C
C           CALL SPKEZP ( TARG, ET, REF, ABCORR, OBS, POS, LT )
C
C           VALUE = POS(3)
C
C           RETURN
C           END
C
C
C
C     C-Procedure GFB
C     C
C     C     User defined boolean routine.
C     C
C
C           SUBROUTINE GFB ( UDFUNS, ET, BOOL )
C           IMPLICIT NONE
C
C     C- Abstract
C     C
C     C     User defined boolean function:
C     C
C     C        VALUE >= LIM1 with VALUE <= LIM2.
C     C
C
C           EXTERNAL              UDFUNS
C
C           DOUBLE PRECISION      ET
C           LOGICAL               BOOL
C           DOUBLE PRECISION      VALUE
C
C
C           DOUBLE PRECISION      LIM1
C           DOUBLE PRECISION      LIM2
C
C           LIM1 = -1000.D0
C           LIM2 =  1000.D0
C
C           CALL UDFUNS ( ET, VALUE )
C
C     C
C     C     Calculate the boolean value.
C     C
C           BOOL = (VALUE .GE. LIM1) .AND. (VALUE .LE. LIM2 )
C
C           RETURN
C           END
C
C     The program outputs:
C
C      Interval            1
C         Interval start: 2011 JAN 09 14:42:24.4846
C                      Z=   -999.99999990308515
C         Interval end  : 2011 JAN 09 16:06:22.5021
C                      Z=    1000.0000000900436
C
C      Interval            2
C         Interval start: 2011 JAN 23 04:07:44.4554
C                      Z=    1000.0000001154267
C         Interval end  : 2011 JAN 23 05:23:06.2437
C                      Z=   -1000.0000001147444
C
C      Interval            3
C         Interval start: 2011 FEB 05 22:35:57.1561
C                      Z=   -999.99999997469570
C         Interval end  : 2011 FEB 05 23:59:57.7487
C                      Z=    999.99999989149978
C
C                        ...
C
C      Interval           25
C         Interval start: 2011 DEC 03 00:32:08.8206
C                      Z=   -999.99999987966544
C         Interval end  : 2011 DEC 03 02:01:12.7695
C                      Z=    999.99999987608885
C
C      Interval           26
C         Interval start: 2011 DEC 17 10:17:24.0390
C                      Z=    1000.0000000822058
C         Interval end  : 2011 DEC 17 11:40:37.2235
C                      Z=   -999.99999997521718
C
C      Interval           27
C         Interval start: 2011 DEC 30 09:04:47.2759
C                      Z=   -1000.0000000487748
C         Interval end  : 2011 DEC 30 10:33:07.6707
C                      Z=    999.99999986779312
C
C     Recall the default convergence tolerance for the GF system has
C     value 10^-6 seconds.
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
C-   SPICELIB Version 1.0.0, 15-JUL-2014 (EDW)
C
C-&

C$ Index_Entries
C
C   GF user defined boolean function search
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

      DOUBLE PRECISION      TOL
      LOGICAL               OK

      LOGICAL               GFBAIL
      EXTERNAL              GFBAIL

      LOGICAL               NOBAIL
      PARAMETER           ( NOBAIL = .FALSE. )

      LOGICAL               NORPT
      PARAMETER           ( NORPT  = .FALSE. )


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN( 'GFUDB' )

C
C     Check the result window size.
C
      IF ( (SIZED(RESULT) .LT. 2) .OR. ODD( SIZED(RESULT) ) ) THEN

         CALL SETMSG ( 'Result window size was #; size must be '
     .   //            'at least 2 and an even value.'             )
         CALL ERRINT ( '#', SIZED(RESULT)                          )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFUDB'                                     )
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

      CALL ZZGFUDB ( UDFUNS, UDFUNB, TOL,     GFSTEP,  GFREFN,
     .               NORPT,  GFREPI,  GFREPU,  GFREPF,
     .               NOBAIL, GFBAIL,  CNFINE,  RESULT )

      CALL CHKOUT( 'GFUDB' )

      RETURN
      END

