C$Procedure ZZGFCSLV ( GF, coordinate solver )

      SUBROUTINE ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, UDSTEP,
     .                      UDREFN, RPT,    UDREPI, UDREPU, UDREPF,
     .                      BAIL,   UDBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

C$ Abstract
C
C     Perform a coordinate search.
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
C     NAIF_IDS
C     FRAMES
C
C$ Keywords
C
C     GEOMETRY
C     PRIVATE
C     ROOT
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      CNTRCT
      PARAMETER           ( CNTRCT = 1.0D0 )

      CHARACTER*(*)         VECDEF
      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      CHARACTER*(*)         CRDSYS
      CHARACTER*(*)         CRDNAM
      CHARACTER*(*)         RELATE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      ADJUST
      EXTERNAL              UDSTEP
      EXTERNAL              UDREFN
      LOGICAL               RPT
      EXTERNAL              UDREPI
      EXTERNAL              UDREPU
      EXTERNAL              UDREPF
      LOGICAL               BAIL
      EXTERNAL              UDBAIL
      LOGICAL               UDBAIL
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK   ( LBCELL : MW, NW )
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      DOUBLE PRECISION      RESULT ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LBCELL     P   Cell lower bound.
C     CNTRCT     P   Existence window contraction magnitude.
C     VECDEF     I   Vector definition.
C     METHOD     I   Computation method.
C     TARGET     I   Target name.
C     REF        I   Reference frame name.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Observer name.
C     DREF       I   Ray's direction vector frame.
C     DVEC       I   Ray's direction vector.
C     CRDSYS     I   Coordinate system name.
C     CRDNAM     I   Coordinate name.
C     RELATE     I   Relational operator.
C     REFVAL     I   Reference value.
C     TOL        I   Convergence tolerance.
C     ADJUST     I   Absolute extremum adjustment value.
C     UDSTEP     I   Step size routine.
C     UDREFN     I   Search refinement routine.
C     RPT        I   Progress report flag.
C     UDREPI     I   Progress report initialization routine.
C     UDREPU     I   Progress report update routine.
C     UDREPF     I   Progress report termination routine.
C     BAIL       I   Bail-out flag.
C     UDBAIL     I   Bail-out status function.
C     MW         I   Workspace window size.
C     NW         I   Workspace window count.
C     WORK      I-O  Workspace window array.
C     CNFINE     I   Confinement window.
C     RESULT     O   Result window.
C
C$ Detailed_Input
C
C
C     VECDEF     Every coordinate computed by this routine is a
C                function of an underlying vector. VECDEF is a short
C                string describing the means by which the vector of
C                interest is defined. Only parameters from the Fortran
C                INCLUDE file zzgf.inc should be used. Parameter names
C                and meanings are:
C
C                   POSDEF               Vector is position of
C                                        target relative to observer.
C
C                   SOBDEF               Vector is sub-observer
C                                        point on target body.  Vector
C                                        points from target body
C                                        center to sub-observer point.
C                                        The target must be an extended
C                                        body modeled as a triaxial
C                                        ellipsoid.
C
C                   SINDEF               Vector is ray-surface intercept
C                                        point on target body. Vector
C                                        points from target body
C                                        center to sub-observer point.
C                                        The target must be an extended
C                                        body modeled as a triaxial
C                                        ellipsoid.
C
C                Case, leading and trailing blanks ARE significant
C                in the string VECDEF.
C
C
C     METHOD     is a string specifying the computational method
C                applicable to the vector of interest. When VECDEF
C                is the parameter
C
C                   SOBDEF
C
C                METHOD should be set to one of the values accepted
C                by the SPICELIB routine SUBPNT.
C
C                When VECDEF is the parameter
C
C                   SINDEF
C
C                METHOD should be set to one of the values accepted
C                by the SPICELIB routine SINCPT.
C
C                METHOD is ignored if VECDEF is set to
C
C                   POSDEF
C
C                Case, leading and trailing blanks are not significant
C                in the string METHOD.
C
C
C     TARGET     is the name of the target object.
C
C
C     REF        is the name of the reference frame relative to which
C                the vector of interest is specified. The specified
C                condition applies to the specified coordinate of
C                of this vector in frame REF.
C
C                When geodetic coordinates are used, the reference
C                ellipsoid is assumed to be that associated with
C                the central body of the frame designated by REF.
C                In this case, the central body of the frame must
C                be an extended body.
C
C                Case, leading and trailing blanks are not significant
C                in the string REF.
C
C
C     ABCORR     indicates the aberration corrections to be applied to
C                the state of the target body to account for one-way
C                light time and stellar aberration.  The orientation
C                of the target body will also be corrected for one-way
C                light time when light time corrections are requested.
C
C                Supported aberration correction options for
C                observation (case where radiation is received by
C                observer at ET) are:
C
C                  'NONE'          No correction.
C                  'LT'            Light time only.
C                  'LT+S'          Light time and stellar aberration.
C                  'CN'            Converged Newtonian (CN) light time.
C                  'CN+S'          CN light time and stellar aberration.
C
C                Supported aberration correction options for
C                transmission (case where radiation is emitted from
C                observer at ET) are:
C
C                  'XLT'           Light time only.
C                  'XLT+S'         Light time and stellar aberration.
C                  'XCN'           Converged Newtonian (CN) light time.
C                  'XCN+S'         CN light time and stellar aberration.
C
C                For detailed information, see the geometry finder
C                required reading, gf.req.  Also see the header of
C                SPKEZR, which contains a detailed discussion of
C                aberration corrections.
C
C                Case, leading and trailing blanks are not significant
C                in the string ABCORR.
C
C
C     OBSRVR     is the name of the observer.
C
C
C     DREF       is the name of the reference frame relative to which a
C                ray's direction vector is expressed. This may be any
C                frame supported by the SPICE system, including
C                built-in frames (documented in the Frames Required
C                Reading) and frames defined by a loaded frame kernel
C                (FK). The string DREF is case-insensitive, and leading
C                and trailing blanks in DREF are not significant.
C
C                When DREF designates a non-inertial frame, the
C                orientation of the frame is evaluated at an epoch
C                dependent on the frame's center and, if the center is
C                not the observer, on the selected aberration
C                correction. See the description of the direction
C                vector DVEC for details.
C
C
C     DVEC       Ray direction vector emanating from the observer. The
C                intercept with the target body's surface of the ray
C                defined by the observer and DVEC is sought.
C
C                DVEC is specified relative to the reference frame
C                designated by DREF.
C
C                Non-inertial reference frames are treated as follows:
C                if the center of the frame is at the observer's
C                location, the frame is evaluated at ET. If the frame's
C                center is located elsewhere, then letting LTCENT be
C                the one-way light time between the observer and the
C                central body associated with the frame, the
C                orientation of the frame is evaluated at ET-LTCENT,
C                ET+LTCENT, or ET depending on whether the requested
C                aberration correction is, respectively, for received
C                radiation, transmitted radiation, or is omitted.
C                LTCENT is computed using the method indicated by
C                ABCORR.
C
C
C     CRDSYS     is the name of the coordinate system to which the
C                coordinate of interest belongs. Allowed values are
C                those defined in the GF Fortran INCLUDE file
C
C                   zzgf.inc.
C
C                Note that when geodetic coordinates are used, the
C                reference ellipsoid is that associated with the
C                central body of the reference frame designated by REF.
C                The central body must be an extended body in this
C                case.
C
C                Case, leading and trailing blanks are not significant
C                in the string CRDSYS.
C
C
C     CRDNAM     is the name of the coordinate of interest:  this is
C                the coordinate to which the specified condition
C                applies.  The set of coordinate names is a function of
C                the coordinate system. Allowed values are those
C                defined in the GF Fortran INCLUDE file
C
C                   zzgf.inc.
C
C                Case, leading and trailing blanks are not significant
C                in the string CRDNAM.
C
C
C     RELATE      is a relational operator used to define a constraint
C                 on the specified coordinate. The result window found
C                 by this routine indicates the time intervals where
C                 the constraint is satisfied. Supported values of
C                 RELATE and corresponding meanings are shown below:
C
C                    '>'      Coordinate is greater than the reference
C                             value REFVAL.
C
C                    '='      Coordinate is equal to the reference
C                             value REFVAL.
C
C                    '<'      Coordinate is less than the reference
C                             value REFVAL.
C
C
C                   'ABSMAX'  Coordinate is at an absolute maximum.
C
C                   'ABSMIN'  Coordinate is at an absolute  minimum.
C
C                   'LOCMAX'  Coordinate is at a local maximum.
C
C                   'LOCMIN'  Coordinate is at a local minimum.
C
C                The caller may indicate that the region of interest
C                is the set of time intervals where the coordinate is
C                within a specified tolerance of an absolute extremum.
C                The argument ADJUST (described below) is used to
C                specify this tolerance.
C
C                Local extrema are considered to exist only in the
C                interiors of the intervals comprising the confinement
C                window:  a local extremum cannot exist at a boundary
C                point of the confinement window.
C
C                Case is not significant in the string RELATE.
C
C
C     REFVAL     is the reference value used to define equality or
C                inequality conditions.
C
C                If the coordinate has the dimension "length," then
C                REFVAL has units of kilometers.
C
C                If the coordinate has the dimension "angle," then
C                REFVAL has units of radians.
C
C                When the coordinate of interest is longitude, REFVAL
C                is interpreted as though it were translated, if
C                necessary, by an integer multiple of 2*pi to place it
C                in the standard range for longitude: (-pi, pi].
C                Similarly, when the coordinate of interest is right
C                ascension, REFVAL is interpreted as though it were
C                translated, if necessary, by an integer multiple of
C                2*pi into the range [0, 2*pi).
C
C                Example:  suppose REFVAL is set to -4.5. Then the
C                          condition
C
C                   longitude equals REFVAL
C
C                is interpreted as
C
C                   longitude equals -0.5 * pi
C
C                so the solution window for this condition may well
C                be non-empty.
C
C                REFVAL is ignored if RELATE is not an equality or
C                inequality operator.
C
C
C     TOL        is a tolerance value used to determine convergence of
C                root-finding operations. TOL is measured in TDB
C                seconds and is greater than zero.
C
C
C     ADJUST     The amount by which the coordinate is allowed to vary
C                from an absolute extremum. ADJUST is not used for
C                equality or inequality conditions. ADJUST must not be
C                negative.
C
C                If ADJUST is positive and a search for an absolute
C                minimum is performed, the resulting schedule contains
C                time intervals when the specified coordinate has
C                values between ABSMIN and ABSMIN + ADJUST.
C
C                If the search is for an absolute maximum, the
C                corresponding range is  between ABSMAX - ADJUST and
C                ABSMAX.
C
C
C     UDSTEP     is a routine that computes a time step used to search
C                for a transition of the state of the specified
C                coordinate. In the context of this routine's
C                algorithm, a "state transition" occurs where the
C                coordinate's time derivative changes from negative to
C                non-negative or vice versa.
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
C                   STEP    is the output step size. STEP indicates
C                           how far to advance ET so that ET and
C                           ET+STEP may bracket a state transition and
C                           definitely do not bracket more than one
C                           state transition. STEP is a DOUBLE
C                           PRECISION number. Units are TDB seconds.
C
C                If a constant step size is desired, the routine GFSTEP
C                may be used. GFSTEP returns the step size that was set
C                via the most recent call to GFSSTP.
C
C
C     UDREFN     is the name of the externally specified routine that
C                computes a refinement in the times that bracket a
C                transition point. In other words, once a pair of
C                times have been detected such that the system is in
C                different states at each of the two times, UDREFN
C                selects an intermediate time which should be closer to
C                the transition state than one of the two known times.
C                The calling sequence for UDREFN is:
C
C                   CALL UDREFN ( T1, T2, S1, S2, T )
C
C                where the inputs are:
C
C                   T1    is a time when the system is in state S1. T1
C                         is a DOUBLE PRECISION number.
C
C                   T2    is a time when the system is in state S2. T2
C                         is a DOUBLE PRECISION number and is assumed
C                         to be larger than T1.
C
C                   S1    is the state of the system at time T1.
C                         S1 is a LOGICAL value.
C
C                   S2    is the state of the system at time T2.
C                         S2 is a LOGICAL value.
C
C                UDREFN may use or ignore the S1 and S2 values.
C
C                The output is:
C
C                   T     is next time to check for a state transition.
C                         T is a DOUBLE PRECISION number between T1 and
C                         T2.
C
C                If a simple bisection method is desired, the routine
C                GFREFN may be used. This is the default option.
C
C
C     RPT        is a logical variable which controls whether the
C                progress reporter is on or off; setting RPT
C                to .TRUE. enables progress reporting.
C
C
C     UDREPI     is a user-defined subroutine that initializes a
C                progress report. When progress reporting is
C                enabled, UDREPI is called at the start of a search
C                pass (see the implementation of ZZGFREL for details on
C                search passes).  The calling sequence of UDREPI is
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
C                is the confinement window passed into ZZGFREL, and
C
C                   RPTPRE
C                   RPTSUF
C
C                are prefix and suffix strings used in the progress
C                report:  these strings are intended to bracket a
C                representation of the fraction of work done.
C
C                SPICELIB provides the default progress reporting
C                initialization routine GFREPI. If GFREPI is used, then
C                the progress reporting update and termination routines
C                GFREPU and GFREPF must be used as well.
C
C
C     UDREPU     is a user-defined subroutine that updates the
C                progress report for a search pass. The calling
C                sequence of UDREPU is
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
C
C     UDREPF     is a user-defined subroutine that finalizes a
C                progress report. UDREPF has no arguments.
C
C
C     BAIL       is a logical flag indicating whether or not interrupt
C                signal handling is enabled. Setting BAIL to .TRUE.
C                enables interrupt signal handling: the GF system will
C                then call UDBAIL to check for interrupt signals.
C
C
C     UDBAIL     is the name of a user defined logical function that
C                checks to see whether an interrupt signal has been
C                issued from, e.g. the keyboard. UDBAIL is used only
C                when BAIL is set to .TRUE. If interrupt handling is
C                not used, the SPICELIB function GFBAIL should be
C                passed in as the actual bail-out function argument.
C
C
C     MW         is the cell size of the windows in the workspace array
C                WORK.
C
C
C     NW         is the number of windows in the workspace array WORK.
C                NW must be at least as large as the parameter NWMAX.
C
C
C     WORK       is an array used to store workspace windows. This
C                array has dimensions ( LBCELL : MW, NW).
C
C
C     CNFINE     is a SPICE window that confines the bounds of the
C                search.
C
C                For coordinates defined by ray-target surface
C                intercepts, the effective confinement window is
C                obtained by searching for times within CNFINE when the
C                specified intercept and its derivative with respect to
C                time are computable. The window resulting from this
C                search is then contracted by CNTRCT+TOL seconds at
C                both left and right endpoints; this contracted window
C                is called the "existence window," since the surface
C                intercept and its time derivative are expected to be
C                computable on this contracted window. The user must
C                select CNFINE so that this requirement is met.
C
C
C     RESULT     is an initialized SPICE window. RESULT must be large
C                enough to hold all of the intervals, within the
C                confinement window, on which the specified condition
C                is met.
C
C                RESULT must be initialized by the caller via the
C                SPICELIB routine SSIZED.
C
C
C$ Detailed_Output
C
C     WORK       has undefined contents on output.
C
C
C     RESULT     is a SPICELIB window containing the intersection of
C                the confinement window and the set of time intervals
C                when the value of the specified coordinate satisfies
C                constraints specified by RELATE and ADJUST.
C
C                For coordinates defined by ray-target surface
C                intercepts, RESULT is further restricted to the window
C                over which the intercept and its derivative with
C                respect to time are computable. See the description of
C                CNFINE above for details.
C
C$ Parameters
C
C     LBCELL     is the lower bound for SPICELIB cells.
C
C     CNTRCT     is the contraction magnitude used to prepare the
C                "existence window" for use as a confinement window.
C                The existence window is applicable only to coordinates
C                of surface intercepts: it is the result of contracting
C                the window over which the surface intercept and its
C                time derivative are computable by CNTRCT+TOL. Units
C                are TDB seconds.
C
C$ Exceptions
C
C     1)  If the workspace window count NW is less than NWMAX, the
C         error SPICE(TOOFEWWINDOWS) is signaled.
C
C     2)  If the workspace window size MW is less than 2, the
C         error SPICE(WINDOWSTOOSMALL) is signaled.
C
C     3)  If a workspace window or the result window is too small
C         to accommodate the required number of intervals,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     4)  If either the observer or target names cannot be mapped
C         to ID codes, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C     5)  If the observer and target have the same ID codes, the
C         error will be diagnosed by routines in the call tree of this
C         routine.
C
C     6)  If the vector definition VECDEF is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     7)  If the computation method METHOD is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     8)  If the aberration correction ABCORR is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     9)  If the coordinate system name CRDSYS is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     10) If the coordinate name CRDNAM is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     11) If the frame REF is not recognized by the frames subsystem,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     12) If VECDEF calls for a computation involving a target surface
C         intercept point and the name and ID code of the frame DREF
C         associated with the target body are not available from the
C         frame subsystem, the error will be diagnosed by routines in
C         the call tree of this routine.
C
C     13) If VECDEF calls for a computation involving a target surface
C         intercept point and the direction vector DVEC is the zero
C         vector, the error will be diagnosed by routines in the call
C         tree of this routine.
C
C     14) If VECDEF calls for a computation involving a target surface
C         point and the radii defining the reference ellipsoid
C         associated with the target body are not available in the
C         kernel pool, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C     15) If VECDEF calls for a computation involving a target surface
C         point and the frame REF is not centered on the target body,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     16) If geodetic or planetographic coordinates are used and the
C         radii defining the reference ellipsoid associated with the
C         center of the frame REF are not available in the kernel pool,
C         the error will be diagnosed by routines in the call tree of
C         this routine.
C
C     17) If geodetic or planetographic coordinates are used and the
C         first equatorial radius of the reference ellipsoid associated
C         with the center of the frame REF is zero, the error will be
C         diagnosed by routines in the call tree of this routine.
C
C     18) If geodetic or planetographic coordinates are used and the
C         equatorial radii of the reference ellipsoid associated
C         with the center of the frame REF are unequal, the error
C         SPICE(NOTSUPPORTED) is signaled.
C
C     19) If geodetic or planetographic coordinates are used and the
C         reference ellipsoid associated with the center of the frame
C         REF is degenerate (one or more radii are non-positive),
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     20) If ADJUST is negative, the error SPICE(VALUEOUTOFRANGE)
C         is signaled.
C
C     21) If TOL is non-positive, the error SPICE(VALUEOUTOFRANGE)
C         is signaled.
C
C     21) If RELATE is not a supported relational operator
C         specification, the error SPICE(NOTRECOGNIZED) is signaled.
C
C$ Files
C
C     See the discussion in the Files section of the header of the
C     umbrella subroutine ZZGFCOU.
C
C$ Particulars
C
C     This routine handles coordinate search set-up and execution
C     activities for GFEVNT.
C
C     For a surface intercept coordinate search, this routine finds the
C     "existence window," within the input confinement window, for the
C     surface intercept and its time derivative. The existence window
C     is contracted by CNTRCT seconds; this contracted window is then
C     used as the confinement window for the search.
C
C$ Examples
C
C     See GFEVNT and ZZGFLONG.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCSLV must be called prior to use of any of the other
C         entry points.
C
C     3)  This routine has the following couplings with other
C         SPICE routines:
C
C            - The set of allowed aberration corrections must
C              be kept in sync with the set supported by the
C              SPK API routines.
C
C            - The set of vector definitions must be kept in
C              sync with the set supported by GFEVNT.
C
C            - The set of supported coordinate systems must be kept in
C              sync with the set supported by zzgf.inc.
C
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
C-    SPICELIB Version 1.2.0, 04-APR-2011 (EDW)
C
C        Replaced use of rooutines ZZGFREL with ZZGFRELX, and
C        ZZGFSOLV with ZZGFSOLVX. ZZGFCOIN argument list edited
C        to remove the unneeded argument REFVAL.
C
C        The code changes for ZZGFRELX use should not affect the
C        numerical results of GF computations.
C
C-    SPICELIB Version 1.0.0 06-MAR-2009 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     coordinate search
C
C-&

      EXTERNAL              UDF

C
C     SPICELIB functions
C
      INTEGER               ISRCHC
      INTEGER               WNCARD

      LOGICAL               FAILED
      LOGICAL               RETURN

      EXTERNAL              ZZGFCODC
      EXTERNAL              ZZGFCOG
      EXTERNAL              ZZGFCOEX
      EXTERNAL              ZZGFUDLT

C
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MXPASS
      PARAMETER           ( MXPASS  = 3 )

      INTEGER               XPASS
      PARAMETER           ( XPASS  = 3 )

C
C     Number of supported comparison operators:
C
      INTEGER               NC
      PARAMETER           ( NC     = 7 )

C
C     MAXOP is the maximum string length for comparison operators.
C     MAXOP may grow if new comparisons are added.
C
      INTEGER               MAXOP
      PARAMETER           ( MAXOP = 6 )


C
C     Local variables
C
      CHARACTER*(MAXOP)     CNAMES ( NC )
      CHARACTER*(LNSIZE)    LOCCRD
      CHARACTER*(LNSIZE)    LOCVDF
      CHARACTER*(MXBEGM)    PREBUF ( MXPASS )
      CHARACTER*(MXBEGM)    RPTPRE ( MXPASS )
      CHARACTER*(MXENDM)    RPTSUF ( MXPASS )
      CHARACTER*(MAXOP)     UOP

      DOUBLE PRECISION      EXCON
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      START

      INTEGER               I
      INTEGER               LOC
      INTEGER               NPASS

      LOGICAL               LOCALX
      LOGICAL               NOADJX

C
C     Saved variables
C
      SAVE                  RPTPRE
      SAVE                  RPTSUF

C
C     Initial values
C
C
C     Below we initialize the list of comparison operator names.
C
      DATA                  CNAMES / '>',
     .                               '=',
     .                               '<',
     .                               'ABSMAX',
     .                               'ABSMIN',
     .                               'LOCMAX',
     .                               'LOCMIN'   /

      DATA                  RPTPRE / 'Coordinate pass 1 of # ',
     .                               'Coordinate pass 2 of # ',
     .                               'Intercept existence pass 1 of 1' /

      DATA                  RPTSUF / 3 * 'done.' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFCSLV' )

C
C     Check the workspace window count.
C
      IF ( NW .LT. NWMAX ) THEN

         CALL SETMSG ( 'Workspace window count was # but must be '
     .   //            'at least #.'                              )
         CALL ERRINT ( '#', NW                                    )
         CALL ERRINT ( '#', NWMAX                                 )
         CALL SIGERR ( 'SPICE(TOOFEWWINDOWS)'                     )
         CALL CHKOUT ( 'ZZGFCSLV'                                 )
         RETURN

      END IF

C
C     Check the workspace window size. The minimum size that
C     makes any sense is 2.
C
      IF ( MW .LT. 2 ) THEN

         CALL SETMSG ( 'Workspace window size was # but must be '
     .   //            'at least 2.'                              )
         CALL ERRINT ( '#', MW                                    )
         CALL SIGERR ( 'SPICE(WINDOWSTOOSMALL)'                   )
         CALL CHKOUT ( 'ZZGFCSLV'                                 )
         RETURN

      END IF

C
C     Make sure ADJUST is non-negative.
C
      IF ( ADJUST .LT. 0.D0 ) THEN

         CALL SETMSG ( 'ADJUST was #; must be non-negative.' )
         CALL ERRDP  ( '#',  ADJUST                          )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'              )
         CALL CHKOUT ( 'ZZGFCSLV'                            )
         RETURN

      END IF

C
C     Make sure TOL is positive.
C
      IF ( TOL .LE. 0.D0 ) THEN

         CALL SETMSG ( 'TOL was #; must be positive.' )
         CALL ERRDP  ( '#',  TOL                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'       )
         CALL CHKOUT ( 'ZZGFCSLV'                     )
         RETURN

      END IF

C
C     Make sure that the requested comparison operation is one we
C     recognize.
C
      CALL LJUST ( RELATE,  UOP )
      CALL UCASE ( UOP,     UOP )

      LOC = ISRCHC ( UOP, NC, CNAMES )

      IF ( LOC .EQ. 0 ) THEN

         CALL SETMSG ( 'The comparison operator, # is not '     //
     .                 'recognized.  Supported operators are: ' //
     .                 '>,'                                     //
     .                 '=,'                                     //
     .                 '<,'                                     //
     .                 'ABSMAX,'                                //
     .                 'ABSMIN,'                                //
     .                 'LOCMAX,'                                //
     .                 'LOCMIN.'                                )
         CALL ERRCH  ( '#', RELATE                              )
         CALL SIGERR ( 'SPICE(NOTRECOGNIZED)'                   )
         CALL CHKOUT ( 'ZZGFCSLV'                               )
         RETURN

      END IF

C
C     Initialize the workspace windows.
C
      DO I = 1, NW
         CALL SSIZED ( MW, WORK(LBCELL,I) )
      END DO

C
C     Initialize the result window.
C
      CALL SCARDD ( 0, RESULT )

C
C     Create a left-justified, compressed copy of the
C     input vector definition method.
C
      CALL LJUST  ( VECDEF,         LOCVDF )
      CALL CMPRSS ( ' ', 1, LOCVDF, LOCVDF )
      CALL UCASE  ( LOCVDF,         LOCVDF )

C
C     If the vector definition method is "surface intercept,"
C     find the "existence window": the window over which
C     the intercept and its time derivative are computable.
C
      IF ( LOCVDF .EQ. SINDEF ) THEN
C
C        Initialize the search for the existence window.
C
         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                   REF,   ABCORR,  OBSRVR, DREF,
     .                   DVEC,  CRDSYS,  CRDNAM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFCSLV' )
            RETURN
         END IF

C
C        This routine presumes that UDSTEP has been initialized, so we
C        don't attempt to reset the step.
C
C        If progress reporting is enabled, initialize the progress
C        report for the existence window search.
C
         IF ( RPT ) THEN
            CALL UDREPI ( CNFINE, RPTPRE(XPASS), RPTSUF(XPASS) )
         END IF

C
C        ZZGFSOLV will add the result of each search to the workspace
C        window
C
C           WORK(LBCELL,EXWIDX)
C
C        Initialize this window.
C
         CALL SSIZED ( MW, WORK(LBCELL,EXWIDX) )

C
C        Search each interval of the confinement window.
C
         DO I = 1, WNCARD( CNFINE )

            CALL WNFETD ( CNFINE, I, START, FINISH )

            CALL ZZGFSOLVX( UDF,    ZZGFCOEX,  UDSTEP,   UDREFN,
     .                      BAIL,   UDBAIL,    .FALSE.,  0.D0,
     .                      START,  FINISH,    TOL,      RPT,
     .                      UDREPU, WORK(LBCELL,EXWIDX)      )

            IF (  FAILED()  ) THEN
               CALL CHKOUT ( 'ZZGFCSLV'  )
               RETURN
            END IF
C
C           If interrupt processing is enabled, check to see
C           whether an interrupt has occurred.
C
            IF ( BAIL ) THEN

               IF ( UDBAIL () ) THEN
                  CALL CHKOUT ( 'ZZGFCSLV' )
                  RETURN
               END IF

            END IF

         END DO

C
C        If progress reporting is enabled, terminate the report
C        for this pass.
C
         IF ( RPT ) THEN
            CALL UDREPF
         END IF

C
C        For safety, contract the existence window. Store
C        the result in the workspace.
C
         EXCON = TOL + CNTRCT

         CALL WNCOND ( EXCON, EXCON, WORK(LBCELL,EXWIDX) )

      ELSE
C
C        Simply copy the confinement window to the workspace.
C
         CALL COPYD ( CNFINE, WORK(LBCELL, EXWIDX ) )

      END IF


C
C     If progress reporting is enabled, set the report prefix array
C     according to the quantity and the relational operator.
C
      IF ( RPT ) THEN

C
C        We'll use the logical flag LOCALX to indicate a local extremum
C        operator and the flag NOADJX to indicate an absolut extremum
C        operator with zero adjustment.
C
         LOCALX =   ( UOP .EQ. 'LOCMIN' ) .OR. ( UOP .EQ. 'LOCMAX' )

         NOADJX =  ( ADJUST .EQ.  0.D0    ) .AND.
     .           ( ( UOP    .EQ. 'ABSMIN' ) .OR. ( UOP .EQ. 'ABSMAX' ) )
     .

         IF ( LOCALX .OR. NOADJX ) THEN
C
C           These operators correspond to 1-pass searches.
C
            NPASS = 1
         ELSE
            NPASS = 2
         END IF

C
C        Fill in the prefix strings.
C
         DO I = 1, NPASS
            CALL REPMI ( RPTPRE(I), '#', NPASS, PREBUF(I) )
         END DO

      END IF

C
C     Create a left-justified, compressed, upper case copy of the
C     input coordinate name.
C
      CALL LJUST  ( CRDNAM,         LOCCRD )
      CALL CMPRSS ( ' ', 1, LOCCRD, LOCCRD )
      CALL UCASE  ( LOCCRD,         LOCCRD )

C
C     If the coordinate of interest is longitude or right ascension, we
C     have a special case, since the mapping from Cartesian to
C     latitudinal coordinates has a branch discontinuity.
C
      IF ( ( LOCCRD .EQ. LONCRD ) .OR. ( LOCCRD .EQ. RACRD ) ) THEN

C
C        The coordinate is longitude or right ascension.
C
         CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                   OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                   RELATE, REFVAL, TOL,    ADJUST, UDSTEP,
     .                   UDREFN, RPT,    UDREPI, UDREPU, UDREPF,
     .                   BAIL,   UDBAIL, MW,     NW,     WORK,
     .                   WORK(LBCELL, EXWIDX ),  RESULT         )

      ELSE

C
C        This is the normal case.
C
C        Initialize the coordinate quantity utilities.
C
         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                   REF,   ABCORR,  OBSRVR, DREF,
     .                   DVEC,  CRDSYS,  CRDNAM )

C
C        Perform the search.
C
         CALL ZZGFRELX( UDSTEP,   UDREFN,              ZZGFCODC,
     .                  ZZGFUDLT, ZZGFCOG,
     .                  RELATE,   REFVAL,              TOL,
     .                  ADJUST,   WORK(LBCELL,EXWIDX), MW,
     .                  NW,       WORK,                RPT,
     .                  UDREPI,   UDREPU,              UDREPF,
     .                  PREBUF,   RPTSUF,              BAIL,
     .                  UDBAIL,   RESULT                        )

      END IF

      CALL CHKOUT ( 'ZZGFCSLV' )
      RETURN

      END

