C$Procedure ZZGFRELX ( Private --- GF, geometric relation finder )

      SUBROUTINE ZZGFRELX ( UDSTEP,   UDREFN,  UDQDEC,  UDCOND,
     .                      UDFUNC,   RELATE,  REFVAL,
     .                      TOL,      ADJUST,  CNFINE,  MW,
     .                      NW,       WORK,    RPT,     UDREPI,
     .                      UDREPU,   UDREPF,  RPTPRE,  RPTSUF,
     .                      BAIL,     UDBAIL,  RESULT           )

C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     This routine determines time intervals when the value of some
C     geometric quantity related to one or more objects and an observer
C     satisfies a user specified constraint within time intervals
C     specified by the window CNFINE.
C
C     Sister routine to ZZGFREL. Copy any edits to ZZGFREL or ZZGFRELX
C     to the sister routine.
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
C     SPK
C     TIME
C     NAIF_IDS
C     FRAMES
C
C$ Keywords
C
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NWREQ
      PARAMETER           ( NWREQ  = 5 )

      EXTERNAL              UDSTEP
      EXTERNAL              UDREFN
      EXTERNAL              UDQDEC
      EXTERNAL              UDCOND
      EXTERNAL              UDFUNC
      CHARACTER*(*)         RELATE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK ( LBCELL : MW, NW )
      LOGICAL               RPT
      EXTERNAL              UDREPI
      EXTERNAL              UDREPU
      EXTERNAL              UDREPF
      CHARACTER*(*)         RPTPRE ( * )
      CHARACTER*(*)         RPTSUF ( * )
      LOGICAL               UDBAIL
      LOGICAL               BAIL
      EXTERNAL              UDBAIL
      DOUBLE PRECISION      RESULT ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LBCELL     P   SPICELIB cell lower bound.
C     NWREQ      P   Minimum number of workspace windows.
C     UDSTEP     I   Name of the routine that computes and returns a
C                    time step.
C     UDREFN     I   Name of the routine that computes a refined time.
C     UDQDEC     I   Name of the routine that computes whether the
C                    scalar quantity is decreasing.
C     UDCOND     I   Name of the routine that computes the scalar
C                    quantity condition with-respect-to the constraint.
C     UDFUNC     I   The routine that computes the scalar quantity of
C                    interest.
C     RELATE     I   Operator that either looks for an extreme value
C                    (max, min, local, absolute) or compares the
C                    scalar quantity value and a number.
C     REFVAL     I   Value used as reference for scalar quantity
C                    condition.
C     TOL        I   Convergence tolerance in seconds.
C     ADJUST     I   Allowed variation for absolute extremal
C                    scalar conditions.
C     CNFINE     I   Confinement window
C     MW         I   Size of workspace windows.
C     NW         I   Number of workspace windows.
C     WORK       I   Array containing workspace windows
C     RPT        I   Progress reporter on ( .TRUE.) or off ( .FALSE. )
C     UDREPI     I   Function that initializes progress reporting.
C     UDREPU     I   Function that updates the progress report.
C     UDREPF     I   Function that finalizes progress reporting.
C     RPTPRE     I   Progress reporter beginning message.
C     RPTSUF     I   Progress reporter ending message.
C     BAIL       I   Logical indicating program interrupt monitoring.
C     UDBAIL     I   Name of a routine that signals a program interrupt.
C     RESULT    I-O  SPICE window containing results.
C
C
C$ Detailed_Input
C
C     UDSTEP     the routine that computes a time step in an attempt to
C                find a transition of the scalar quantity. In the 
C                context of this routine's algorithm, a "transition"
C                occurs where the scalar quantity value changes from
C                "decreasing" to "not decreasing" or vice versa.
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
C                   ET      is the input start time from which the
C                           algorithm is to search forward for a state
C                           transition. ET is expressed as seconds past
C                           J2000 TDB. ET is a DOUBLE PRECISION number.
C
C                   STEP    is the output step size.  STEP indicates
C                           how far to advance ET so that ET and
C                           ET+STEP may bracket a state transition and
C                           definitely do not bracket more than one
C                           state transition.  STEP is a DOUBLE
C                           PRECISION number.  Units are TDB seconds.
C
C                If a constant step size is desired, the routine
C                GFSTEP may be used. This is the default option.
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
C     UDQDEC     the routine that determines if the scalar quantity
C                calculated by UDFUNC is decreasing.
C
C                The calling sequence:
C
C                   CALL UDQDEC ( UDFUNC, ET, ISDECR )
C
C                where:
C
C                   ET       a double precision value representing
C                            ephemeris time, expressed as seconds past
C                            J2000 TDB, at which to determine the time
C                            derivative of UDFUNC.
C
C                   ISDECR   a logical return indicating whether
C                            or not the scalar value returned by UDFUNC
C                            is decreasing. ISDECR returns true if the
C                            time derivative of UDFUNC at ET is
C                            negative.
C
C     UDCOND     the routine that determines if UDFUNC satisfies
C                some constraint condition at epoch ET.
C
C                The calling sequence:
C
C                   CALL UDCOND ( UDFUNC, ET, IN_CON )
C
C                where:
C
C                   ET       a double precision value representing
C                            ephemeris time, expressed as seconds past
C                            J2000 TDB, at which to evaluate UDFUNC.
C
C                   IN_CON   a logical value indicating whether
C                            or not UDFUNC satisfies the constraint
C                            at ET (TRUE) or not (FALSE).
C
C     UDFUNC     the routine that returns the value of the scalar
C                quantity of interest at time ET. The calling sequence
C                for UDFUNC is:
C
C                   CALL UDFUNC ( ET, VALUE )
C
C                where:
C
C                   ET      a double precision value representing
C                           ephemeris time, expressed as seconds past
C                           J2000 TDB, at which to determine the scalar
C                           value.
C
C                   VALUE   the double precision value of the scalar
C                           quantity at ET.
C
C     RELATE     is a comparison operator, indicating the numeric
C                constraint of interest. Values are:
C
C                '>'   value of scalar quantity greater than REFVAL.
C
C                '='   value of scalar quantity equal to REFVAL.
C
C                '<'   value of scalar quantity less than REFVAL.
C
C                ABSMAX-the scalar quantity is at an absolute
C                maximum.
C
C                ABSMIN-the scalar quantity is at an absolute
C                minimum.
C
C                LOCMAX-the scalar quantity is at an local maximum.
C
C                LOCMIN-the scalar quantity is at an local minimum.
C
C     REFVAL     reference value for scalar quantity (in
C                radians, radians/sec, km, or km/sec as appropriate).
C
C     TOL        is a tolerance value used to determine convergence of
C                root-finding operations.  TOL is measured in seconds
C                and is greater than zero.
C
C     ADJUST     the amount by which the numerical quantity is
C                allowed to vary from an absolute extremum. If ADJUST
C                is non-zero, the resulting window contains
C                intervals when the scalar quantity has
C                values either between ABSMIN and ABSMIN + ADJUST
C                or between ABSMAX and ABSMAX - ADJUST. ADJUST must
C                not be negative.
C
C     CNFINE     is a SPICE window that confines the bounds of the
C                search. Note that like all windows (see windows.req)
C                CNFINE can contain multiple time intervals. See the
C                Examples section for information on how to create this
C                window.
C
C     MW         is the cell size of the windows in the workspace array
C                WORK.
C
C     NW         is the number of windows in the workspace array WORK.
C                NW must be at least as large as the parameter NWREQ.
C
C     WORK       is an array used to store workspace windows. This
C                array has dimensions WORK (-5 : MW, NW).
C
C     RPT        is a logical variable which controls whether the
C                progress reporter is on or off. The progress reporter
C                writes to the user's terminal.
C
C     UDREPI     the routine that initializes a progress report.
C                When progress reporting is enabled, UDREPI
C                is called at the start of a search.  The calling
C                sequence of UDREPI is:
C
C                   UDREPI ( CNFINE, RPTPRE, RPTSUF )
C
C                   DOUBLE PRECISION    CNFINE ( LBCELL : * )
C                   CHARACTER*(*)       RPTPRE
C                   CHARACTER*(*)       RPTSUF
C
C                where
C
C                   CNFINE
C
C                is the confinement window passed into ZZGFRELX, and
C
C                   RPTPRE
C                   RPTSUF
C
C                are prefix and suffix strings used in the progress
C                report:  these strings are intended to bracket a
C                representation of the fraction of work done.
C
C                If the user has no progress reporting initialization
C                routine, the SPICELIB routine GFREPI may be used. This
C                is the default option.
C
C     UDREPU     the routine that updates the progress report for a
C                search.  The calling sequence of UDREPU is:
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
C                would be a logical candidate for the search's
C                completion percentage; however the method of
C                measurement is up to the user.
C
C                If the user has no progress reporting update routine,
C                the SPICELIB routine GFREPU may be used. This is the
C                default option.
C
C     UDREPF     the routine that finalizes a progress report. UDREPF
C                has no arguments.
C
C                If the user has no progress reporting finalizing
C                routine, the SPICELIB routine GFREPF may be used. This
C                is the default option.
C
C     RPTPRE     is an array of strings containing the prefixes of
C                the output messages reported by the progress reporter.
C                The Ith element of RPTPRE is the prefix for the
C                message corresponding to the Ith traversal of the
C                confinement window executed by this routine; such
C                traversals are called "passes." The number of passes
C                executed depends on the relational operator RELATE.
C                Searches for local extrema and unadjusted absolute
C                extrema require one pass; searches for adjusted
C                absolute extrema, equalities, and inequalities require
C                two passes.
C
C                An example of the contents of RPTPRE for a distance
C                equality search:
C
C                   RPTPRE(1) = 'Distance pass 1 of 2'
C                   RPTPRE(2) = 'Distance pass 2 of 2'
C
C     RPTSUF     is an array of strings containing the suffixes of
C                the output messages reported by the progress reporter.
C                The Ith element of RPTSUF is the suffix for the
C                message corresponding to the Ith pass.
C
C                An example of the contents of RPTSUF for a distance
C                equality search:
C
C                   RPTSUF(1) = 'done.'
C                   RPTSUF(2) = 'done.'
C
C                For this search, the complete progress report message
C                for the Ith pass has the form
C
C                   'Distance pass I of 2 xxx.xx% done.'
C
C     BAIL       is a logical indicating whether or not interrupt
C                signaling is enabled.
C
C     UDBAIL     the routine that checks to see whether an interrupt
C                signal has been issued from, e.g. the keyboard. If
C                this capability is not to be used, a dummy function,
C                GFBAIL must be supplied.
C
C     RESULT     is an initialized SPICE window. RESULT is large
C                enough to hold all of the intervals, within the
C                confinement window, on which the specified condition
C                is met.
C
C$ Detailed_Output
C
C     RESULT     is a SPICE window containing the time intervals within
C                the confinement window, over which the specified
C                condition is met.
C
C                RESULT is emptied before new values are assigned to
C                it.
C
C$ Parameters
C
C     LBCELL     is the SPICELIB cell lower bound.
C
C     NWREQ      is the required number of workspace windows; the
C                input argument NW must not be less than NWREQ.
C
C$ Exceptions
C
C     1)  A negative value for ADJUST causes the routine to signal
C         the error SPICE(VALUEOUTOFRANGE). A non-zero value for ADJUST
C         when RELATE has any value other than "ABSMIN" or "ABSMAX",
C         causes the routine to signal the error SPICE(INVALIDVALUE).
C
C     2)  If an improper comparison operator is specified, the error
C         SPICE(NOTRECOGNIZED) is signaled.
C
C     3)  If TOL is not greater than zero, the error
C         SPICE(VALUEOUTOFRANGE) will be signaled by routines called
C         from this routine.
C
C     4)  If the number of workspace windows is less than NWREQ, the
C         error SPICE(TOOFEWWINDOWS) is signaled.
C
C     5)  If the window size MW is less than 2, the error
C         SPICE(INVALIDDIMENSION) will be signaled.
C
C     6)  If the output SPICE window RESULT has insufficient capacity
C         to contain the number of intervals on which the specified
C         visibility condition is met, the error will be diagnosed
C         by a routine in the call tree of this routine. If the result
C         window has size less than 2, the error SPICE(WINDOWTOOSMALL)
C         will be signaled by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine determines time intervals when the value of some
C     scalar quantity related to one or more objects and an observer
C     satisfies a user specified constraint. It puts these times in a
C     result window called RESULT. It does this by first finding
C     windows when the quantity of interest is either
C     monotonically increasing or decreasing. These windows are then
C     manipulated to give the final result. Note that the determination
C     of "=" involves finding intervals where the quantity is "less
C     than" to a tolerance of TOL. This means that the end points of
C     these intervals are within TOL of being equal to the value.
C
C$ Examples
C
C     See GFEVNT.
C
C$ Restrictions
C
C     The kernel files to be used by ZZGFRELX must be loaded (normally
C     via the SPICELIB routine FURNSH) before ZZGFRELX is called.
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
C-    SPICELIB Version 1.1.1  08-DEC-2010  (EDW)
C
C        Edit to replace term "schedule" with "window." Edit to 
C        Procedure line text.
C
C        Argument list changed, removing the argument for
C        the reference value update routine, UDQREF. Setting and
C        updating the reference value now occurrs in this 
C        (ZZGFRELX) routine.
C
C        Edit to Detailed I/O description for UDREPI, UDREPU, UDREPF,
C        and UDBAIL. The descriptions stated the wrong name for the
C        default GF functions corresponding to these arguments.
C
C-    SPICELIB Version 1.1.0  16-FEB-2010  (EDW)
C
C        Modified version of ZZGFREL. This version calls ZZGFSOLVX.
C
C-    SPICELIB Version 1.0.0  21-FEB-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
C
C-&

C$ Index_Entries
C
C numeric scalar quantity satisfies a condition
C
C-&

C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               ISRCHC
      INTEGER               SIZED
      INTEGER               WNCARD

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               LEFT
      PARAMETER           ( LEFT   = 1 )

      INTEGER               RIGHT
      PARAMETER           ( RIGHT  = 2 )

      INTEGER               MAXCLN
      PARAMETER           ( MAXCLN = 80 )

C
C     Workspace window indices:
C
      INTEGER               CONFIN
      PARAMETER           ( CONFIN = 3 )

      INTEGER               COPY
      PARAMETER           ( COPY   = 4 )

      INTEGER               DECRES
      PARAMETER           ( DECRES = 2 )

      INTEGER               INCRES
      PARAMETER           ( INCRES = 1 )

      INTEGER               TEMPW
      PARAMETER           ( TEMPW  = 5 )


C
C     Number of supported comparison operators.
C
      INTEGER               NC
      PARAMETER           ( NC     = 7 )

C     One-letter alias for LBCELL to make references to the workspace
C     array tolerable:
C
      INTEGER               B
      PARAMETER           ( B      = LBCELL )

C
C     Context string length:
C
      INTEGER               CTXLEN
      PARAMETER           ( CTXLEN = 500 )

C
C     Local variables
C

      CHARACTER*(MAXCLN)    CNAMES ( NC )
      CHARACTER*(CTXLEN)    CONTXT
      CHARACTER*(MAXCLN)    LOCREL

      DOUBLE PRECISION      ADDL
      DOUBLE PRECISION      ADDR
      DOUBLE PRECISION      ENDPT  (  2 )
      DOUBLE PRECISION      EXTREM
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      REFER2
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      VALUE

      INTEGER               CASE
      INTEGER               COUNT
      INTEGER               I
      INTEGER               MAXAT
      INTEGER               MINAT
      INTEGER               NAME    (  2 )
      INTEGER               PASS
      INTEGER               QCNUM
      INTEGER               WANT
      INTEGER               WINSIZ

      LOGICAL               CSTEP
      LOGICAL               NEED


C
C     Saved variables
C
      SAVE                  CNAMES

C
C     Below we initialize the list of comparison operator names.
C
      DATA                  CNAMES / '<',
     .                               '=',
     .                               '>',
     .                               'LOCMIN',
     .                               'ABSMIN',
     .                               'LOCMAX',
     .                               'ABSMAX'  /


C
C     Set constant step parameter to .FALSE..
C
      DATA                  CSTEP / .FALSE. /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFRELX' )

C
C     Make sure we have enough workspace windows.
C
      IF ( NW .LT. NWREQ ) THEN

         CALL SETMSG ( 'The number of workspace windows (#) is less '
     .   //            'than the minimum #.'                        )
         CALL ERRINT ( '#',  NW                                     )
         CALL ERRINT ( '#',  NWREQ                                  )
         CALL SIGERR ( 'SPICE(TOOFEWWINDOWS)'                       )
         CALL CHKOUT ( 'ZZGFRELX'                                   )
         RETURN

      END IF

C
C     Make sure the workspace windows can contain at least one interval.
C
      IF ( MW .LT. 2 ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be '
     .   //            'at least 2.'                               )
         CALL ERRINT ( '#',  MW                                    )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'ZZGFRELX'                                  )
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
         CALL CHKOUT ( 'ZZGFRELX'                                 )
         RETURN

      END IF

C
C     Make sure the requested comparison is one we recognize.
C
      CALL LJUST ( RELATE, LOCREL )
      CALL UCASE ( LOCREL, LOCREL )

      QCNUM = ISRCHC ( LOCREL, NC, CNAMES )

      IF ( QCNUM .EQ. 0 ) THEN

        CALL SETMSG ('The comparison operator, # is not '            //
     .               'recognized.  Supported quantities are: '       //
     .               '<, '                                           //
     .               '=, '                                           //
     .               '>, '                                           //
     .               'LOCMIN, '                                      //
     .               'ABSMIN, '                                      //
     .               'LOCMAX, '                                      //
     .               'ABSMAX.'                                        )
         CALL ERRCH  ('#', RELATE                                     )
         CALL SIGERR ('SPICE(NOTRECOGNIZED)'                          )
         CALL CHKOUT ('ZZGFRELX'                                       )
         RETURN

      END IF

C
C     Confirm ADJUST is non-negative.
C
      IF ( ADJUST .LT. 0.D0 ) THEN

         CALL SETMSG ( 'ADJUST was #; must be non-negative.' )
         CALL ERRDP  ( '#',  ADJUST                          )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'              )
         CALL CHKOUT ( 'ZZGFRELX'                             )
         RETURN

      END IF

C
C    Confirm ADJUST equals zero unless LOCREL (RELATE) has value
C    "ABSMAX" or "ABSMIN."
C
      IF(  (LOCREL .NE. 'ABSMIN')  .AND. (LOCREL .NE. 'ABSMAX' ) ) THEN

         IF ( ADJUST .NE. 0.D0 ) THEN

            CALL SETMSG ( 'ADJUST should have value zero for all '  //
     .                    'comparison operators except ABSMAX and ' //
     .                    'ABSMIN' )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                     )
            CALL CHKOUT ( 'ZZGFRELX'                                 )
            RETURN

         END IF

      END IF

C
C     If the confinement window is empty, the result window must
C     be empty as well. In this case, there's not much to do.
C
      IF ( CARDD(CNFINE) .EQ. 0 ) THEN

         CALL SCARDD ( 0, RESULT )
         CALL CHKOUT ('ZZGFRELX' )
         RETURN

      END IF

C
C     We need to set up several working windows, one each for
C     increasing and decreasing windows, one for the confining
C     window and one for copying.
C
      CALL SSIZED ( MW, WORK(B,DECRES) )
      CALL SSIZED ( MW, WORK(B,INCRES) )
      CALL SSIZED ( MW, WORK(B,CONFIN) )
      CALL SSIZED ( MW, WORK(B,COPY  ) )
      CALL SSIZED ( MW, WORK(B,TEMPW ) )

      NAME(1) = DECRES
      NAME(2) = INCRES

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFRELX' )
         RETURN
      END IF

C
C     For equality constraints, we work with a somewhat expanded
C     version of the confinement window so we can find equality
C     solutions that lie on the boundary of the original confinement
C     window. The expansion amount is ADDWIN. For other cases the
C     expansion amount is set to zero.
C
      IF ( RELATE .EQ.  '=' ) THEN

         ADDL = ADDWIN
         ADDR = ADDWIN
      ELSE
         ADDL = 0.0D0
         ADDR = 0.0D0
      END IF

      CALL COPYD  ( CNFINE,     WORK(B,CONFIN) )
      CALL WNEXPD ( ADDL, ADDR, WORK(B,CONFIN) )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFRELX' )
         RETURN
      END IF

C
C     Set the reference value.
C 
      CALL ZZGFREF( REFVAL )

C
C     Make a local copy of the reference value.
C
      REFER2 = REFVAL


C
C     Set the pass number for progress reporting.
C
      PASS = 1

C
C     Initialize the work in progress reporter.
C
      IF ( RPT ) THEN
         CALL UDREPI ( WORK(B,CONFIN), RPTPRE(PASS), RPTSUF(PASS) )
      END IF

C
C     Look up the size of the confinement window...
C
      COUNT = WNCARD ( WORK(B,CONFIN) )

C
C     Start the window that contains intervals when the quantity of
C     interest is decreasing. The result will contain all intervals in
C     (expanded) CNFINE when the selected scalar quantity function
C     is decreasing, since this is how ZZGFSOLVX is configured.
C
      DO I = 1, COUNT
C
C        Locate the bounds for the I'th interval of the confinement
C        window. Results are accumulated in the WORK array.
C
         CALL WNFETD ( WORK(B,CONFIN), I, START, FINISH  )

         CALL ZZGFSOLVX ( UDFUNC,  UDQDEC, UDSTEP, UDREFN,  BAIL,
     .                    UDBAIL,  CSTEP,  STEP,   START,
     .                    FINISH,  TOL,    RPT,    UDREPU,
     .                    WORK(B,DECRES) )

         IF (  FAILED()  ) THEN
            CALL CHKOUT ( 'ZZGFRELX'  )
            RETURN
         END IF

         IF ( BAIL ) THEN

            IF ( UDBAIL () ) THEN

               IF ( RPT ) THEN
                  CALL UDREPF
               END IF

               CALL CHKOUT ( 'ZZGFRELX' )
               RETURN

            END IF

         END IF

      END DO

      IF ( RPT ) THEN
         CALL UDREPF
      END IF


C
C     Let's think about what we have now. We have the intervals in the
C     confinement window when a value of some kind is decreasing.
C
C     The left endpoints are points at which the quantity begins
C     decreasing, thus they are times when the quantity is at a local
C     maximum (at least in the interior of the confinement window).
C
C     The right endpoints are where the quantity stops decreasing. Thus
C     those endpoints in the interior of the confinement window are
C     local minima of the quantity.
C
C     The complement relative to the confinement window is the set of
C     intervals within the confinement window for which the quantity is
C     increasing. At the left endpoints of the complement the
C     function is increasing. Thus the interior left endpoints are
C     local minima within the confinement window. The interior right
C     endpoints are local maxima within the confinement window.
C
C     Moreover, to within our ability to detect local extrema, there
C     are no local extrema within any of the intervals. Thus, the
C     function may be regarded as monotone within each of
C     the intervals of these windows. Thus for any desired value of the
C     quantity, there is at most one time within each of the intervals
C     that the desired value is achieved.
C
      IF ( LOCREL .EQ. 'LOCMIN' ) THEN
C
C        We are interested in only interior minima of the quantity.
C        These occur at right endpoints of the intervals in TEMPW
C        that are interior points of CNFINE. First extract the right
C        endpoints. Then find those that are contained in the initial
C        confinement window, excluding endpoints.
C
         CALL WNEXTD ( 'R', WORK(B,DECRES) )

         CALL ZZGFWSTS ( WORK(B,DECRES), CNFINE, '()', RESULT )

         CALL CHKOUT (  'ZZGFRELX' )
         RETURN


      ELSE IF ( LOCREL .EQ. 'LOCMAX' ) THEN
C
C        We are interested in only interior maxima of the quantity.
C        These occur at right endpoints of the intervals in TEMPW
C        that are interior points of CNFINE.
C
         CALL WNEXTD ( 'L', WORK(B,DECRES) )

         CALL ZZGFWSTS ( WORK(B,DECRES), CNFINE,  '()',  RESULT )

         CALL CHKOUT (  'ZZGFRELX' )
         RETURN

      END IF

C
C     We will need the intervals when the quantity of interest is
C     increasing in value.
C
      IF ( ( LOCREL .EQ. 'ABSMIN') .OR. ( LOCREL .EQ. 'ABSMAX') ) THEN
C
C        We need an absolute max or min over the window CNFINE.
C        But we have decreasing values in WORK(B,DECRES).
C        Make a copy of WORK(B,DECRES) then compute the windows
C        of decreasing or increasing quantity over the window CNFINE.
C
         CALL COPYD  ( WORK(B,DECRES), WORK(B,COPY) )

         CALL WNINTD ( CNFINE,         WORK(B,DECRES), WORK(B,TEMPW ) )
         CALL COPYD  ( WORK(B,TEMPW),                  WORK(B,DECRES) )

         CALL WNDIFD ( CNFINE,         WORK(B,DECRES), WORK(B,TEMPW ) )
         CALL COPYD  ( WORK(B,TEMPW),                  WORK(B,INCRES) )

C
C        Here's what we plan to do, we want to look over two windows
C        DECREASING and INCREASING to search for the absolute max or
C        min.  We start with DECREASING.  In this window the max is
C        always at the left endpoint,  The min is at the right
C        endpoint.  In the INCREASING window the min is at the LEFT
C        endpoint of an interval, the max is at the RIGHT endpoint of
C        an interval
C
         MINAT = RIGHT
         MAXAT = LEFT

C
C        As yet we still need to compute our first extremum.
C
         NEED  = .TRUE.

C
C        The extrema search is logically the same for both
C        maximum and minimum. We just need to keep track of
C        our extremum and when we find a more extreme value
C        replace it. DECREASING is first.
C
         DO CASE = 1, 2

            IF ( LOCREL .EQ. 'ABSMIN' ) THEN

               WANT  = MINAT

            ELSE IF ( LOCREL .EQ. 'ABSMAX' ) THEN

               WANT  = MAXAT

            END IF


            WINSIZ = WNCARD (  WORK( B, NAME(CASE) )  )

            DO I = 1, WINSIZ

               CALL WNFETD ( WORK( B,  NAME(CASE) ), I,
     .                       ENDPT(1), ENDPT(2)  )

               CALL UDFUNC ( ENDPT(WANT), VALUE )

               IF (  FAILED() ) THEN
                  CALL CHKOUT (  'ZZGFRELX' )
                  RETURN
               END IF

C
C              Initialize the extreme value. This step will
C              be executed on the first pass through the
C              DECREASING interval.
C
               IF ( NEED ) THEN

                  NEED   = .FALSE.
                  EXTREM = VALUE

               END IF

C
C              Check to see if current VALUE is more extreme than
C              EXTREM.
C
               IF ( LOCREL .EQ. 'ABSMIN' ) THEN

                  IF (       ( ADJUST .EQ. 0.0D0  )
     .                 .AND. ( VALUE  .LE. EXTREM )  ) THEN
C
C                    Let's save the epoch in case it's that of the
C                    absolute min. Add this endpoint as a singleton
C                    interval to the RESULT window.
C
                     CALL SCARDD ( 0, RESULT )

                     CONTXT = 'Saving current candidate epoch at '
     .               //       'which an absolute minimum may occur.'

                     CALL ZZWNINSD ( ENDPT(WANT), ENDPT(WANT),
     .                               CONTXT,      RESULT      )

                  END IF

                  EXTREM = MIN ( EXTREM, VALUE )


               ELSE

                  IF (       ( ADJUST .EQ. 0.0D0  )
     .                 .AND. ( VALUE  .GE. EXTREM )  ) THEN
C
C                    Let's save the epoch in case it's that of the
C                    absolute max. Add this endpoint as a singleton
C                    interval to the RESULT window.
C
                     CALL SCARDD ( 0, RESULT )

                     CONTXT = 'Saving current candidate epoch at '
     .               //       'which an absolute maximum may occur.'

                     CALL ZZWNINSD ( ENDPT(WANT), ENDPT(WANT),
     .                               CONTXT,      RESULT      )

                  END IF

                  EXTREM = MAX ( EXTREM, VALUE )

               END IF

            END DO

            IF (  FAILED() ) THEN
               CALL CHKOUT (  'ZZGFRELX' )
               RETURN
            END IF

C
C           When we go to the next window, the min and max are at
C           opposite ends of the intervals.
C
            CALL SWAPI ( MINAT, MAXAT )

         END DO

C
C        If the adjustment is zero, we're done.
C
         IF ( ADJUST .EQ. 0.0D0 ) THEN

            CALL CHKOUT (  'ZZGFRELX' )
            RETURN

         END IF

C
C        We have a non-zero adjustment. we have the extreme value. Now
C        we need to find the epochs when the extreme value is achieved,
C        allowing for adjustment.
C
         IF ( LOCREL .EQ. 'ABSMIN' ) THEN

            REFER2 = EXTREM + ADJUST

         ELSE
         
C
C           The only other possible value of LOCREL within this block
C           is 'ABSMAX'.
C
            REFER2 = EXTREM - ADJUST

         END IF

C
C        If we reach this point, we need to re-establish the
C        original expanded coverage of 'DECREASING' and 'INCREASING'.
C
         CALL COPYD ( WORK(B,COPY), WORK(B,DECRES) )

      END IF

      CALL WNDIFD ( WORK(B,CONFIN), WORK(B,DECRES), WORK(B,INCRES) )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFRELX' )
         RETURN
      END IF


C
C     We have some kind of greater than, less than, or equal to
C     relation to solve for. Note that ABSMAX and ABSMIN are for case
C     where there is a non-zero adjustment. Reset the reference value,
C     which may have been changed in the ABSOLUTE MAX or MIN blocks
C     above.
C
      CALL ZZGFREF ( REFER2 )


C
C     If progress reporting is enabled, initialize the progress
C     reporter for a second pass over the confinement window.
C
      IF ( RPT ) THEN

C
C        Note that the window passed to UDREPI need not contain the
C        same intervals as those passed to UDREPU; the window passed to
C        UPREPI need only have the correct measure. From UDREPI's
C        perspective, the sole purpose of this window is to convey to
C        the progress reporting system the sum of the measures of the
C        increasing and decreasing windows.
C
         PASS = 2

         CALL UDREPI ( WORK(B,CONFIN), RPTPRE(PASS), RPTSUF(PASS) )

      END IF

C
C     Find those intervals when the scalar quantity is less than
C     REFER2.
C
      CALL SCARDD ( 0, RESULT )

      DO CASE = 1, 2

         WINSIZ = WNCARD (  WORK(B, NAME(CASE))  )

C
C        Search each interval of the window identified by NAME(CASE) for
C        times when the quantity is less than the reference value.
C
         DO I = 1, WINSIZ

            CALL WNFETD ( WORK( B, NAME(CASE)), I, START, FINISH  )
C
C           For each interval, accumulate the result in RESULT.
C
C           Note we know that the behavior of the quantity is monotonic
C           within each window, so the step size can be large. In fact,
C           we use the interval length as the step size.
C
            STEP = FINISH - START

            CALL ZZGFSOLVX ( UDFUNC, UDCOND,  UDSTEP,  UDREFN,  BAIL,
     .                       UDBAIL, .TRUE.,  STEP,    START,
     .                       FINISH, TOL,     RPT,     UDREPU,
     .                       RESULT                           )

            IF (  FAILED()  ) THEN
               CALL CHKOUT ( 'ZZGFRELX'  )
               RETURN
            END IF

            IF ( BAIL ) THEN
               IF ( UDBAIL () ) THEN
                  CALL CHKOUT ( 'ZZGFRELX' )
                  RETURN
               END IF
            END IF

         END DO

      END DO


      IF ( RPT ) THEN
C
C        Finish the progress report for the second pass.
C
         CALL UDREPF

      END IF

C
C     RESULT is the window, within the expanded confinement window,
C     over which the function of interest is less than the reference
C     value. We can use this window to get whatever was requested.
C
      IF ( ( LOCREL .EQ. '<' ) .OR. ( LOCREL .EQ. 'ABSMIN' ) ) THEN
C
C        We simply need to restrict our result to the original
C        confinement window. Note that the ABSMIN search with
C        non-zero adjustment is now a search for values less than the
C        adjusted absolute minimum. Same for ABSMAX below.
C
         CALL WNINTD ( CNFINE,         RESULT,  WORK(B,TEMPW) )
         CALL COPYD  ( WORK(B,TEMPW),           RESULT        )


      ELSE IF ( ( LOCREL .EQ. '>' ) .OR. ( LOCREL .EQ. 'ABSMAX' ) ) THEN
C
C        Subtract from the confinement window the window where the
C        quantity is less than the reference value: the remainder is
C        the portion of the confinement window on which the quantity is
C        greater than or equal to the reference value.
C
         CALL WNDIFD ( CNFINE,         RESULT,  WORK(B,TEMPW) )
         CALL COPYD  ( WORK(B,TEMPW),           RESULT        )

      ELSE
C
C        This is the branch for the relational operator '='.
C
C        Create a window of singleton intervals from the endpoints
C        of RESULT.
C
         CALL SCARDD ( 0, WORK(B,TEMPW) )

         DO I = 1, CARDD(RESULT)

            CONTXT = 'Inserting endpoints of result window into '
     .      //       'workspace window WORK(B,TEMPW). These points '
     .      //       'are candidate epochs that may satisfy an '
     .      //       'equality constraint.'

            CALL ZZWNINSD ( RESULT(I), RESULT(I),
     .                      CONTXT,    WORK(B,TEMPW) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFRELX' )
               RETURN
            END IF

         END DO

C
C        The window WORK(B,TEMPW) contains singleton intervals where
C        either the equality constraint is met, or where a boundary
C        point of the expanded confinement window is located. We're not
C        interested in the boundary points; these are likely not
C        solution points and in any case are outside the original
C        confinement window.
C
C        Keep only the endpoints of RESULT that are contained in the
C        original confinement window CNFINE; these are by construction
C        interior points of the expanded confinement window.
C
         CALL WNINTD ( CNFINE,  WORK(B,TEMPW),  RESULT )

      END IF

      CALL CHKOUT ( 'ZZGFRELX' )
      RETURN
      END
