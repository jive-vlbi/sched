C$Procedure ZZGFLONG ( GF, longitude solver )

      SUBROUTINE ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, UDSTEP,
     .                      UDREFN, RPT,    UDREPI, UDREPU, UDREPF,
     .                      BAIL,   UDBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine determines time windows when the longitude
C     or right ascension of a specified vector satisfies a specified
C     mathematical condition.
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
C     PRIVATE
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

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
C                CRDSYS must refer to a system in which longitude
C
C                or right ascension is a coordinate. Note that when
C                geodetic coordinates are used, the reference ellipsoid
C                is that associated with the central body of the
C                reference frame designated by REF. The central body
C                must be an extended body in this case.
C
C                Case, leading and trailing blanks are not significant
C                in the string CRDSYS.
C
C
C     CRDNAM     is the name of the coordinate of interest: this is
C                the coordinate to which the specified condition
C                applies. Supported coordinates are
C
C                   Planetocentric longitude
C                   Right ascension
C
C                which are designated respectively by the parameters
C
C                   LONCRD
C                   RACRD
C
C                See the INCLUDE file
C
C                   zzgf.inc
C
C                for the declarations of these parameters.
C
C                For the
C
C                   Latitudinal
C                   Geodetic
C                   Spherical
C
C                coordinate systems, longitude lies in the range
C
C                   ( -pi, pi ]
C
C                For the
C
C                   Cylindrical
C                   Planetographic
C
C                coordinate systems, longitude lies in the range
C
C                   [ 0, 2*pi )
C
C                Right ascension lies in the range
C
C                   [ 0, 2*pi )
C
C                Case, leading and trailing blanks are not significant
C                in the string CRDNAM.
C
C
C     RELATE      is a relational operator used to define a constraint
C                 on longitude or right ascension of the specified
C                 vector. The result window found by this routine
C                 indicates the time intervals where the constraint is
C                 satisfied. Supported values of RELATE and
C                 corresponding meanings are shown below:
C
C                    '>'      Longitude or RA is greater than the
C                             reference value REFVAL.
C
C                    '='      Longitude or RA is equal to the reference
C                             value REFVAL.
C
C                    '<'      Longitude or RA is less than the
C                             reference value REFVAL.
C
C
C                   'ABSMAX'  Longitude or RA is at an absolute maximum.
C
C                   'ABSMIN'  Longitude or RA is at an absolute
C                             minimum.
C
C                   'LOCMAX'  Longitude or RA is at a local maximum.
C
C                   'LOCMIN'  Longitude or RA is at a local minimum.
C
C                The caller may indicate that the region of interest
C                is the set of time intervals where the quantity is
C                within a specified tolerance of an absolute extremum.
C                The argument ADJUST (described below) is used to
C                specify this tolerance.
C
C                Local extrema are considered to exist only in the
C                interiors of the intervals comprising the confinement
C                window: a local extremum cannot exist at a boundary
C                point of the confinement window.
C
C                Case is not significant in the string RELATE.
C
C
C     REFVAL     is the reference value used to define equality or
C                inequality conditions.
C
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
C                values between
C
C                         ABSMIN
C                   and   MIN ( ABSMIN + ADJUST, MX )
C
C                where MX is the maximum value of the coordinate's
C                range.
C
C                If the search is for an absolute maximum, the
C                corresponding range is  between
C
C                         MAX ( ABSMAX - ADJUST, MN )
C                   and   ABSMAX
C
C                where MN is the minimum value of the coordinate's
C                range.
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
C                The windows contained WORK that used by this routine
C                are initialized here to have size MW. The other
C                elements of WORK are not modified.
C
C
C     CNFINE     is a SPICE window that confines the bounds of the
C                search.
C
C                For coordinates defined by ray-target surface
C                intercepts, the intercept and its time derivative are
C                expected to be computable on the confinement window.
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
C$ Detailed_Output
C
C
C     WORK       has undefined contents on output, with the exception
C                of the windows occupying the range
C
C                   ( LBCELL : NW, EXWIDX : NWMAX )
C
C                which are not modified by this routine.
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
C                Due to computational accuracy limitations, the
C                coordinate of interest *may not satisfy the
C                specified condition* at all points belonging to
C                RESULT.  For example, if the caller specifies
C                a tolerance of 1.E-6 seconds and seeks the
C                solution set for the condition
C
C                   The planetocentric longitude of the geometric
C                   earth-sun vector in the J2000 frame is greater
C                   than or equal to zero
C
C                the right endpoints of some intervals in RESULT may be
C                times that map to negative longitude values very close
C                to -pi radians.
C
C                The user (of SPICE API routines dependent on this
C                routine) may wish to contract RESULT using WNCOND in
C                order to guarantee that the specified condition
C                is satisfied on RESULT. Selection of a suitable
C                contraction value is dependent on the user's
C                requirements and the specific problem to be solved.
C
C$ Parameters
C
C     LBCELL     is the SPICELIB cell lower bound.
C
C$ Exceptions
C
C     1)  In order for this routine to produce correct results,
C         the external step size routine UDGSTP must return step sizes
C         appropriate for the problem at hand.  Step sizes that
C         are too large may cause this routine to miss roots;
C         step sizes that are too small may cause this routine to
C         run unacceptably slowly and in some cases, find spurious
C         roots.
C
C         This routine does not diagnose invalid step sizes,
C         except that if the step size is non-positive, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C     2)  In order for this routine to produce correct results,
C         the convergence tolerance TOL must be appropriate for the
C         problem at hand.  The error in any interval endpoint
C         contained in RESULT should be expected to be no smaller
C         than TOL; depending on the behavior of the coordinate
C         and the condition, the error could be much larger.  For
C         example, for some functions, finding correct, unique
C         extrema is notoriously difficult.
C
C         The user should keep in mind that the minimum separation
C         between successive values of ET is about 1.E-7 seconds
C         for SPICE platforms and values of ET not extremely close to
C         J2000.
C
C         This routine does not diagnose invalid tolerance values,
C         except that if the tolerance is non-positive, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C     3)  A negative value for ADJUST causes the routine to signal
C         the error SPICE(VALUEOUTOFRANGE). A non-zero value for ADJUST
C         when RELATE has any value other than "ABSMIN" or "ABSMAX",
C         causes the routine to signal the error SPICE(INVALIDVALUE).
C
C     4)  If the operator string RELATE doesn't contain a recognized
C         value, the error SPICE(NOTRECOGNIZED) is signaled.
C
C     5)  If any error occurs while initializing the coordinate
C         utility package, the error will be diagnosed by routines
C         in the call tree of ZZGFCOIN.
C
C     6)  If any error occurs while performing computations
C         to determine if a quantity of interest is decreasing
C         at a specified time, the error will be diagnosed by
C         routines in the call tree of this routine.
C
C     7)  If any error occurs while performing computations
C         to determine if a quantity of interest is less than a
C         specified reference value at a specified time, the error will
C         be diagnosed by routines in the call tree of this routine.
C
C     8)  If an error (typically cell overflow) occurs while performing
C         window arithmetic, the error will be diagnosed by
C         routines in the call trees of window routines called by
C         this routine.
C
C     9)  Due to numerical errors, in particular,
C
C            - Truncation error in time values
C            - Finite tolerance value
C            - Errors in computed geometric quantities
C
C         it is *normal* that the condition of interest is not
C         satisfied on the entire result window.
C
C         The result window may need to be contracted slightly by the
C         caller to achieve desired results, in particular to remove
C         times where discontinuities of longitude or right ascension
C         are crossed.
C
C     10) Most relational conditions involving longitude or
C         right ascension make sense only when latitude or declination
C         is bounded away from +/- pi/2 radians.  Users should
C         select the confinement window accordingly.
C
C     11) The user must take care when searching for an extremum
C         (ABSMAX, ABSMIN, LOCMAX, LOCMIN) of  LONGITUDE or
C         RIGHT ASCENSION values. Since these quantities are cyclical,
C         rather than monotonically increasing or decreasing, an
C         extremum may be hard to interpret. In particular, if an
C         extremum is found near the cycle boundary (- PI for
C         longitude, 2 PI for RIGHT ASCENSION) it may not be
C         numerically reasonable. For example, the search for times
C         when a longitude coordinate is at its absolute maximum may
C         result in a time when the longitude value is - PI, due to
C         roundoff error.
C
C$ Files
C
C     This routine doesn't directly participate in SPICE kernel loading
C     or unloading.  However, a variety of SPICE kernels must be loaded
C     in order for this routine to work:
C
C        - Since all coordinate computations supported by this routine
C          depend on observer-target vectors, at a minimum, SPK files
C          providing ephemeris data enabling computation of these
C          vectors are required.
C
C        - If non-inertial reference frames are used, then PCK
C          files, frame kernels, C-kernels, and SCLK kernels may be
C          needed.
C
C        - If the coordinate of interest is defined in terms of a target
C          surface point, then (currently) a PCK providing radii for a
C          triaxial shape model must be loaded.
C
C        - If geodetic coordinates are used, then a PCK providing radii
C          for a triaxial shape model must be loaded.
C
C     See the Files section of GFEVNT's header for further information.
C
C
C$ Particulars
C
C     Since this is a private SPICELIB routine, the header comments
C     make many references to the routine's implementation.  This
C     is done to help the maintenance programmer understand the
C     routine; however, these comments may themselves need to be
C     updated if the GF subsystem implementation changes.
C
C     This routine determines time windows when the longitude or right
C     ascension of a specified vector satisfies a specified
C     mathematical condition.  This routine can (in some cases, by
C     means of multiple calls) answer questions such as
C
C        When does the moon pass over the earth's prime meridian?
C
C        Given a time window when the geodetic latitude of the MGS
C        spacecraft relative to the IAU_MARS frame is between -30 : +30
C        degrees, when within this window is the planetographic
C        longitude of the spacecraft between 15 and 16 degrees?
C
C     For brevity, throughout this routine, we'll refer to the vector
C     whose longitude or right ascension is of interest as "the vector"
C     or "the vector of interest." We'll also call the longitude or
C     right ascension "the coordinate" or "the coordinate of interest."
C
C     A note concerning processing speed:  the algorithm used by this
C     routine takes a "divide and conquer" approach that involves, in
C     many cases, multiple calls to the low-level GF root finding
C     routines.  So the user can expect most longitude or right
C     ascension computations to be relatively slow.  Using a
C     confinement window that is more easily computed, say one
C     involving latitude constraints, can be very helpful.
C
C$ Examples
C
C     See usage in GFEVNT.
C
C$ Restrictions
C
C     1)  The interface and functionality of this routine may change
C         without notice.  This routine should be called only by
C         SPICELIB routines.
C
C     2)  Root-finding problems of the sort solved by this routine are,
C         when a computer is involved, replete with mathematical
C         complications. We've tried to cover all the angles in the
C         Detailed_Input, Detailed_Output, and Exceptions header
C         sections.  No doubt some issues remain unaddressed.  Correct
C         usage of this routine depends in good measure on the user
C         posing "reasonable" problems to solve.
C
C     3)  The kernel files to be used by ZZGFLONG must be loaded
C         (normally via the SPICELIB routine FURNSH) before ZZGFLONG is
C         called.
C
C     4)  This routine has the side effect of re-initializing the
C         coordinate quantity utility package. Callers may themselves
C         need to re-initialize the coordinate quantity utility package
C         after calling this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E.D. Wright    (JPL)
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0 04-APR-2011 (EDW)
C
C        Replaced use of rooutines ZZGFREL with ZZGFRELX. ZZGFCOIN
C        argument list edited to remove the unneeded argument REFVAL.
C
C        The 2.1.0 changes should not affect the numerical results
C        of GF coordinate computations.
C
C-    SPICELIB Version 2.0.0 12-MAY-2009 (NJB)
C
C        Upgraded to support targets and observers having
C        no names associated with their ID codes.
C
C-    SPICELIB Version 1.0.0  23-FEB-2009 (NJB) (EDW)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

      INTEGER               CARDD
      INTEGER               ISRCHC
      INTEGER               LNKNXT
      INTEGER               WNCARD

      LOGICAL               ELEMI

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               SMSGND

C
C     Entry points in the coordinate utility package.
C     We have the usual GF entry points for the coordinate, plus
C     utilities for the cosine and sine of the coordinate.
C
C     Names and meanings:
C
C        ZZGFCODC      Is coordinate decreasing?
C        ZZGFCOG       Get coordinate value.
C        ZZGFCOCD      Is cosine of the coordinate decreasing?
C        ZZGFCOCG      Get cosine of the coordinate value.
C        ZZGFCOSD      Is sine of the coordinate decreasing?
C        ZZGFCOSG      Get sine of the coordinate value.
C
      EXTERNAL ZZGFCODC
      EXTERNAL ZZGFCOG
      EXTERNAL ZZGFCOCD
      EXTERNAL ZZGFCOCG
      EXTERNAL ZZGFCOSD
      EXTERNAL ZZGFCOSG

C
C    The routine to test UDFUNC < REFVAL.
C
      EXTERNAL ZZGFUDLT


C
C     Local parameters
C
C
C
C     Margin for branch cut avoidance. Units are radians:
C
      DOUBLE PRECISION      BCAMRG
      PARAMETER           ( BCAMRG = 1.D-11 )

C
C     Margin for local extrema search. Units are radians:
C
      DOUBLE PRECISION      LCXMRG
      PARAMETER           ( LCXMRG = 1.D-12 )

C
C     Short alias for LBCELL:
C
      INTEGER               LB
      PARAMETER           ( LB     = LBCELL )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = LBCELL )

C
C     Number of supported comparison operators:
C
      INTEGER               NC
      PARAMETER           ( NC     = 7 )

C
C     Assorted string lengths:
C
C     Maximum body name length:
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

C
C     NAMLEN is the maximum length of both a frame name and of
C     any kernel pool variable name.
C
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

C
C     OPLEN is the maximum string length for comparison operators.
C     OPLEN may grow if new comparisons are added.
C
      INTEGER               OPLEN
      PARAMETER           ( OPLEN = 6 )

C
C     FUNLEN is the length of the function name string.
C
      INTEGER               FUNLEN
      PARAMETER           ( FUNLEN = 50 )

C
C     CRDLEN is the maximum length of a coordinate name.
C
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

C
C     SYSLEN is the maximum length of a coordinate system name.
C
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )

C
C     RPTLEN is the maximum length of a progress reporter message.
C
      INTEGER               RPTLEN
      PARAMETER           ( RPTLEN = 80 )
C
C     Local variables
C
      CHARACTER*(CRDLEN)    NRMCRD
      CHARACTER*(SYSLEN)    NRMSYS
      CHARACTER*(OPLEN)     OPS    ( NC )
      CHARACTER*(CRDLEN)    PRXCRD
      CHARACTER*(FUNLEN)    PRXFUN
      CHARACTER*(SYSLEN)    PRXSYS
      CHARACTER*(BDNMLN)    RCTRNM
      CHARACTER*(RPTLEN)    RPTPRE ( 2 )
      CHARACTER*(RPTLEN)    RPTSUF ( 2 )
      CHARACTER*(RPTLEN)    TMPLAT
      CHARACTER*(NAMLEN)    RLIST  ( NWLONG )
      CHARACTER*(OPLEN)     UOP
      CHARACTER*(OPLEN)     PRXREL

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      CMPVAL
      DOUBLE PRECISION      CV
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EXTVAL
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LOCREF
      DOUBLE PRECISION      LOCTOL
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      PRXVAL
      DOUBLE PRECISION      R2OVR2
      DOUBLE PRECISION      START
      DOUBLE PRECISION      SV
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      XRFVAL
      DOUBLE PRECISION      Y      ( 3 )

      INTEGER               BOT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               COMPL
      INTEGER               F1
      INTEGER               F2
      INTEGER               FRCODE
      INTEGER               HEAD
      INTEGER               I
      INTEGER               LEFT
      INTEGER               N
      INTEGER               NEEDWN ( LBCELL : NWLONG )
      INTEGER               NEXT
      INTEGER               NL
      INTEGER               NODE
      INTEGER               Q1
      INTEGER               Q2
      INTEGER               Q3
      INTEGER               Q4
      INTEGER               QUAD
      INTEGER               REFCTR
      INTEGER               REGION ( 3 )
      INTEGER               RES
      INTEGER               RES1
      INTEGER               RES2
      INTEGER               RIGHT
      INTEGER               S
      INTEGER               TOP
      INTEGER               TOTAL
      INTEGER               WH
      INTEGER               WIX    (             NWLONG )
      INTEGER               WWPOOL ( 2, LBPOOL : NWLONG )

      LOGICAL               FLIP
      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE                  OPS
      SAVE                  Y

C
C     Initial values
C
      DATA                  OPS / '<',
     .                            '=',
     .                            '>',
     .                            'LOCMIN',
     .                            'ABSMIN',
     .                            'LOCMAX',
     .                            'ABSMAX'  /

      DATA                  Y   / 0.D0, 1.D0, 0.D0 /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGFLONG' )
      END IF

C
C     Overview
C     ========
C
C
C     Terminology
C     -----------
C
C        - Proxy function
C
C          In many cases, instead of finding a time window
C          where the coordinate of interest satisfies a specified
C          condition, we'll find a time window where a second, related
C          function satisfies a related condition.  We'll call this
C          second function the "proxy function."
C
C          The proxy function will be one that is "better behaved"
C          than the original in the domain of interest.  For
C          example, when searching for times when longitude is
C          equal to pi radians, we may instead intersect the
C          confinement window with a window on which cosine of
C          longitude is negative, and then within that more
C          restricted intersection, find the times when the sine
C          of longitude is zero.  In this example sine(longitude)
C          is a proxy function for longitude.
C
C        - Resolution of a function
C
C          Below we'll refer to the "resolution" of a proxy function.
C          In order to find roots accurately, it's necessary for
C          a proxy function to change a by a reasonable amount
C          when the function it represents changes.  Mathematically,
C          the magnitude of the derivative of the proxy function
C          with respect to the function it represents should not
C          be too much less than 1.  An example of a *bad* choice
C          of a proxy function would be to use cosine of longitude
C          as a proxy function for longitude in a confinement
C          window in which longitude is close to zero.  This
C          choice would lead to considerable loss of accuracy.  On
C          the other hand, sine of longitude would be a reasonable
C          proxy function for this case.
C
C        - The unit circle
C
C          In the discussion below, we'll freely associate angular
C          coordinates with locations on the unit circle. For example,
C          we might say "longitude is in the upper half of the unit
C          circle."
C
C        - Window aliases
C
C          We're going to make extensive use workspace windows.
C          In many cases, we'll need to reuse various windows for
C          different purposes at different times.  So instead
C          of using mnemonic parameter names for window indices,
C          we'll use variables we call window aliases.  For example,
C          when we want to use the 8th workspace window to hold
C          the window of times when longitude is in the upper half
C          of the unit circle, we'll set the alias UPPER equal to
C          8, so we can refer to the window by
C
C              WORK( LB, UPPER )
C
C          and keep track of what we're using the window for.
C
C          Some of the aliases aren't wonderful names:  we use
C          F1, F2, etc.  to represent "free" window 1, 2, etc.
C
C
C     Algorithm
C     ---------
C
C        -  Equality
C
C           We use sine or cosine of the coordinate as proxy functions.
C           The proxy function having the better resolution is
C           selected.  For example, to find times when right ascension
C           is 2*pi/3, we search for the times when cosine of right
C           ascension is equal to -1/2. Since these searches can produce
C           spurious roots, we cast out any such roots after completing
C           the search.
C
C
C        -  Local extrema
C
C           We first find local extrema in the right and left half
C           circles, using longitude as a proxy function on the right
C           half and right ascension on the left.
C
C
C        -  Absolute extrema
C
C           We deal with absolute extrema before inequalities because
C           this allows us to use the code (later in this routine) for
C           inequality relations when the user specifies a non-zero
C           ADJUST value. When ADJUST is non-zero, having the actual
C           extreme value in hand, we can easily solve for the window
C           in which the coordinate is greater than
C
C              <absolute maximum> - ADJUST
C
C           or less than
C
C              <absolute minimum> + ADJUST
C
C           Below, "Searching in a region" means that we find the
C           window when the coordinate is in the region (and of course
C           in the confinement window), then use this window as the
C           confinement window.
C
C           Finding absolute extrema is a matter of successively
C           searching for extrema in different parts of the unit
C           circle.  For example, when we search for an absolute
C           maximum of longitude, we first search in the second
C           quadrant, then if we find nothing, the right half circle,
C           then if we find nothing, the fourth quadrant.
C
C           We always use longitude as a proxy function on the right
C           half circle and right ascension as a proxy function on
C           the left half circle.
C
C
C        -  Inequality
C
C           In general, we use  proxy functions and break up the unit
C           circle into regions where the proxy functions are single
C           valued. The exact solution approach depends on where the
C           reference value is.  For example, to find the window on
C           which longitude is less than 3*pi/4, we first search
C           for the solution in the second quadrant.  We then
C           combine this result window with the window of times
C           when longitude is in the right half circle, and with
C           the window of times when longitude is in the third
C           quadrant.
C
C
C     Code layout
C     -----------
C
C        We've tried to arrange the code to minimize calls to
C        ZZGFREL, primarily because these expensive in terms of
C        run time.  They also take up a lot of space.
C
C        The code starts out by re-formulating the constraint,
C        if necessary, as one applying to planetocentric longitude
C        or right ascension. This simplifies the subsequent logic.
C
C        Equality searches are handled before the rest. The routine
C        exits after processing a search having an equality constraint.
C
C        Searches for local extrema are handled next. Again, the
C        routine exits after processing these types of searches.
C
C        The next portion of the code is devoted to dealing with
C        absolute extrema. If the search is for absolute extrema and
C        AJDUST is non-zero, we use the results from this portion of
C        the code to set up an inequality search, which is done below.
C
C        After the portion of the code dealing with absolute extrema
C        with ADJUST equal to zero, we perform setup functions to
C        prepare to call ZZGFREL. In general, what's happening here is
C        that we're deciding what regions of the unit circle we're
C        going to use in our solution, and we prepare to find windows
C        when the coordinate is in the various regions of interest.
C        This setup code includes assignment of window aliases,
C        selection of proxy functions, and setting flags indicating
C        which windows corresponding to search regions must be
C        computed.
C
C        Next, the windows corresponding to times when the coordinate
C        is in the selected regions are found using ZZGFREL.
C

C
C     Check the workspace window count.
C
      IF ( NW .LT. NWMAX ) THEN

         CALL SETMSG ( 'Workspace window count was # but must be '
     .   //            'at least #.'                              )
         CALL ERRINT ( '#', NW                                    )
         CALL ERRINT ( '#', NWMAX                                 )
         CALL SIGERR ( 'SPICE(TOOFEWWINDOWS)'                     )
         CALL CHKOUT ( 'ZZGFLONG'                                 )
         RETURN

      END IF

C
C     We can't initialize the whole workspace, but we can initialize
C     the windows we actually own. Do so.
C
      DO I = 1, NWLONG
         CALL SSIZED ( MW, WORK(LB, NWREL+I) )
      END DO

C
C     Initialize the workspace window pool. Set up the parallel
C     array of window indices.
C
      CALL LNKINI ( NWLONG, WWPOOL )

      DO I = 1, NWLONG
         WIX(I) = NWREL + I
      END DO

C
C     Get an upper case, left-justified version of the
C     requested comparison operation.
C
      CALL LJUST ( RELATE, UOP )
      CALL UCASE ( UOP,    UOP )

C
C     Reject bad operators.
C
C     Use the original operator string in the error message.
C
      I = ISRCHC( UOP, NC, OPS )

      IF ( I .EQ. 0 ) THEN

        CALL SETMSG ('The comparison operator, # is not '            //
     .               'recognized.  Supported quantities are: '       //
     .               '<, '                                           //
     .               '=, '                                           //
     .               '>, '                                           //
     .               'LOCMIN, '                                      //
     .               'ABSMIN, '                                      //
     .               'LOCMAX, '                                      //
     .               'ABSMAX.'                                        )
         CALL ERRCH  ( '#',  RELATE                                   )
         CALL SIGERR ('SPICE(NOTRECOGNIZED)'                          )
         CALL CHKOUT ( 'ZZGFLONG'                                     )
         RETURN

      END IF

C
C     Make sure TOL is positive.
C
      IF ( TOL .LE. 0.D0 ) THEN

         CALL SETMSG ( 'TOL was #; must be positive.' )
         CALL ERRDP  ( '#',  TOL                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'       )
         CALL CHKOUT ( 'ZZGFLONG'                     )
         RETURN

      END IF

C
C     We'll use a local tolerance equal to 1/5 of the input value.
C     This will allow us to keep the total round-off error within
C     the desired tolerance.
C
      LOCTOL = MAX ( 1.D-7,  TOL / 10.D0 )

C
C     Make sure ADJUST is non-negative.
C
      IF ( ADJUST .LT. 0.D0 ) THEN

         CALL SETMSG ( 'ADJUST was #; must be non-negative.' )
         CALL ERRDP  ( '#',  ADJUST                          )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'              )
         CALL CHKOUT ( 'ZZGFLONG'                            )
         RETURN

      END IF

C
C    Confirm ADJUST equals zero unless UOP (RELATE) has value
C    "ABSMAX" or "ABSMIN."
C
      IF(  (UOP .NE. 'ABSMIN')  .AND. (UOP .NE. 'ABSMAX' ) ) THEN

         IF ( ADJUST .NE. 0.D0 ) THEN

            CALL SETMSG ( 'ADJUST should have value zero for all '  //
     .                    'comparison operators except ABSMAX and ' //
     .                    'ABSMIN' )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                     )
            CALL CHKOUT ( 'ZZGFLONG'                                )
            RETURN

         END IF

      END IF


C
C     Get an upper case, left-justified, compressed versions of the
C     coordinate system and coordinate names.
C
      CALL LJUST  ( CRDSYS,           NRMSYS )
      CALL CMPRSS ( ' ',   0, NRMSYS, NRMSYS )
      CALL UCASE  ( NRMSYS,           NRMSYS )

      CALL LJUST  ( CRDNAM,           NRMCRD )
      CALL CMPRSS ( ' ',   1, NRMCRD, NRMCRD )
      CALL UCASE  ( NRMCRD,           NRMCRD )

C
C     Make an initial call to the coordinate utility initialization
C     routine to invoke error checking. We don't want to have
C     to duplicate the checking here. Later, when necessary, we'll
C     re-initialize the utilities.
C
      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR, DREF,
     .                DVEC,   NRMSYS, NRMCRD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN
      END IF

C
C     We've done the basic error checking. Empty the result window and
C     return now if the confinement window is empty.
C
      IF ( WNCARD(CNFINE) .EQ. 0 ) THEN

         CALL SCARDD ( 0, RESULT )

         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN

      END IF

C
C     Initialize the total number of search passes performed.
C
      TOTAL = 0

C
C     To eliminate special cases, we'll check for inequality
C     constraints that are always met or can't be met.
C
      IF (      ( NRMSYS .EQ. CYLSYS )
     .     .OR. ( NRMSYS .EQ. PGRSYS )
     .     .OR. ( NRMSYS .EQ. RADSYS )  ) THEN

         IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
C
C           The reference value lies on the branch cut at 0.
C
            IF ( UOP .EQ. '<' ) THEN
C
C              These coordinates can never be less than zero.
C
               CALL SCARDD( 0, RESULT )

               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN

            ELSE IF ( UOP .EQ. '>' ) THEN
C
C              The solution is the whole confinement window. This
C              is because the inequality operators really act like
C              '>=' and '<=' operators, and because we assume the
C              quantity is increasing or decreasing except on a
C              set of measure zero.
C
               CALL COPYD ( CNFINE, RESULT )

               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN

            END IF

         END IF


      ELSE IF (      ( NRMSYS .EQ. GEOSYS )
     .          .OR. ( NRMSYS .EQ. LATSYS )
     .          .OR. ( NRMSYS .EQ. SPHSYS )  ) THEN

         IF ( COS(REFVAL) .EQ. -1.D0 ) THEN
C
C           The reference value lies on the branch cut at pi.
C
            IF ( UOP .EQ. '<' ) THEN
C
C              The solution is the whole confinement window.
C
               CALL COPYD ( CNFINE, RESULT )

               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN

            ELSE IF ( UOP .EQ. '>' ) THEN
C
C              These coordinates can never be greater
C              than pi.
C
               CALL SCARDD( 0, RESULT )

               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN

            END IF

         END IF

      END IF

C
C     At this point, we make some adjustments to simplify the
C     remaining code. We map the input coordinate system to
C     either "latitudinal" or "RA/DEC" and modify the
C     constraint if the original system is "planetographic."
C     The longitude coordinate is renamed accordingly, if necessary.
C     The mapping is as follows:
C
C        Spherical      ( longitude range is (-pi, pi] ) -> Latitudinal
C
C        Cylindrical    ( longitude range is [0, 2*pi] ) -> RA/Dec
C           Longitude                                    -> RA
C
C        Planetographic ( longitude range is [0, 2*pi] ) -> RA/Dec
C           Longitude                                    -> RA
C
C
C     For planetographic coordinates, if the longitude is positive
C     west, and since REFVAL does not lie on the branch cut, we can
C     make the following additional adjustments:
C
C        Input relational operator           Transformed operator
C        -------------------------           --------------------
C        ABSMAX                              ABSMIN
C        ABSMAX - ADJUST                     ABSMIN + ADJUST
C        ABSMIN                              ABSMAX
C        ABSMIN + AJDUST                     ABSMAX - ADJUST
C        LOCMAX                              LOCMIN
C        LOCMIN                              LOCMAX
C        <        REFVAL                     >        2*pi - REFVAL
C        >        REFVAL                     <        2*pi - REFVAL
C        =        REFVAL                     =        2*pi - REFVAL
C
C
      XRFVAL = REFVAL


      IF ( NRMSYS .EQ. SPHSYS ) THEN

         NRMSYS = LATSYS
         XRFVAL = REFVAL

      ELSE IF ( NRMSYS .EQ. CYLSYS ) THEN

         NRMSYS = RADSYS
         NRMCRD = RACRD
         XRFVAL = REFVAL

      ELSE IF ( NRMSYS .EQ. PGRSYS ) THEN

         NRMSYS = RADSYS
         NRMCRD = RACRD

C
C        If the planetographic coordinates are positive West, we'll
C        need to transform the constraint and reference value.
C
C        Get the name of the central body of frame REF.
C
C        NOTE: We omit error checking here because ZZGFCOIN has done
C        it already.
C
         CALL NAMFRM ( REF,    FRCODE )
         CALL FRINFO ( FRCODE, REFCTR,  CLASS, CLSSID, FOUND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFLONG'                       )
            RETURN
         END IF

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'FRINFO didn''t find data for frame # '
     .      //            'which has frame ID code #. This frame '
     .      //            'should have been validated by '
     .      //            'ZZGFCOIN.'                             )
            CALL ERRCH  ( '#', REF                                )
            CALL ERRINT ( '#', FRCODE                             )
            CALL SIGERR ( 'SPICE(BUG)'                            )
            CALL CHKOUT ( 'ZZGFLONG'                              )
            RETURN

         END IF

         CALL BODC2S ( REFCTR, RCTRNM )

C
C        Find the longitude of the +Y axis. If this longitude
C        is greater than pi, the sense is positive West. Note
C        that we don't need to use realistic values of the
C        equatorial radius and flattening factor: 1 and 0,
C        respectively, are just fine.
C
         CALL RECPGR ( RCTRNM, Y, 1.D0, 0.D0, LON, LAT, ALT )

C
C        Planetographic longitude ranges from 0 to 2*pi, so
C        longitudes corresponding to positive Y values are
C        in the range pi to 2*pi.
C
         IF (  LON .GT. PI() ) THEN
C
C           Planetographic longitude for the frame center is positive
C           West.
C
C           Note that no action is required to modify non-zero
C           extremum adjustment values.
C
            IF ( UOP .EQ. 'ABSMAX' ) THEN

               UOP = 'ABSMIN'

            ELSE IF ( UOP .EQ. 'ABSMIN' ) THEN

               UOP = 'ABSMAX'

            ELSE IF ( UOP .EQ. 'LOCMAX' ) THEN

               UOP = 'LOCMIN'

            ELSE IF ( UOP .EQ. 'LOCMIN' ) THEN

               UOP = 'LOCMAX'

            ELSE IF ( UOP .EQ. '=' ) THEN

               XRFVAL = TWOPI() - REFVAL

            ELSE IF ( UOP .EQ. '<' ) THEN

               UOP    = '>'
               XRFVAL = TWOPI() - REFVAL

            ELSE IF ( UOP .EQ. '>' ) THEN

               UOP    = '<'
               XRFVAL = TWOPI() - REFVAL

            ELSE
C
C              We shouldn't get here.
C
               CALL SETMSG ( 'Unexpected UOP value: #' )
               CALL ERRCH  ( '#', UOP                  )
               CALL SIGERR ( 'SPICE(BUG)'              )
               CALL CHKOUT ( 'ZZGFLONG'                )
               RETURN

            END IF

         ELSE
C
C           Longitude is positive East, so we treat
C           the constraint as though the coordinate were RA.
C
            XRFVAL = REFVAL

         END IF

      END IF

C
C     From this point on, we use:
C
C        Coordinate system:  NRMSYS
C        Coordinate:         NRMCRD
C        Operator:           UOP
C        Reference value:    XRFVAL
C
C
C     The result window must be initialized by the caller of the GF
C     system (usually a user application). We simply empty the result
C     window here.
C
      CALL SCARDD ( 0, RESULT )

C
C     We use the constant 0.5 * 2**0.5 quite a bit.  Create a
C     "macro" variable for it.
C
      R2OVR2 = SQRT(2.D0) / 2.D0

C
C     Set the progress report suffix strings.
C
      RPTSUF(1) = 'done.'
      RPTSUF(2) = 'done.'


C
C     Case:  '='
C
      IF ( UOP .EQ. '=' ) THEN
C
C        Equality constraints are the simplest to handle, so we'll get
C        them out of the way now. Our approach is to use sine or cosine
C        as proxy functions; we'll select the proxy function with the
C        highest resolution at the reference value. For the proxy
C        function f, our proxy constraint is
C
C           f(x) = f(XRFVAL)
C
C        This may yield spurious roots; we'll delete these after we've
C        done our search.
C
C        Find the sine and cosine of the reference value. We'll use
C        these both to locate the quadrant of the reference value and
C        to have continuously differentiable functions to work with.
C        Note that if the original reference value is not in the
C        standard range, this presents no problem.
C
         CV = COS ( XRFVAL )
         SV = SIN ( XRFVAL )

C
C        Decide which proxy function to use.
C
         IF ( ABS(SV) .GE. R2OVR2 ) THEN
C
C           The reference value lies in the top or bottom quarter of
C           the unit circle. The "comparison value" CMPVAL will be
C           used later to delete solutions with matching sines but
C           non-matching cosines.
C
            PRXFUN = 'COS'
            PRXVAL = CV
            CMPVAL = SV

         ELSE

            PRXFUN = 'SIN'
            PRXVAL = SV
            CMPVAL = CV

         END IF

C
C        Set up the progress reporting prefix strings. We have one
C        ZZGFREL call which performs two passes.
C
         RPTPRE(1) = 'Coordinate pass 1 of 2'
         RPTPRE(2) = 'Coordinate pass 2 of 2'

C
C        Allocate a workspace window.
C
         CALL LNKAN ( WWPOOL, NODE )
         F1 = WIX(NODE)

C
C        Make sure the coordinate utilities have been initialized
C        with the actual values we'll use for our search.
C
         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                   REF,    ABCORR, OBSRVR, DREF,
     .                   DVEC,   NRMSYS, NRMCRD )

C
C        Now we're ready to compute the window in which our proxy
C        function satisfies the proxy constraint.
C
         IF ( PRXFUN .EQ. 'SIN' ) THEN
C
C           Find the window where the sine of the coordinate satisfies
C           the proxy constraint.
C

            CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOSD,
     .                     ZZGFUDLT,     ZZGFCOSG,
     .                     '=',          PRXVAL,          LOCTOL,
     .                     0.D0,         CNFINE,          MW,
     .                     NW,           WORK,            RPT,
     .                     UDREPI,       UDREPU,          UDREPF,
     .                     RPTPRE,       RPTSUF,          BAIL,
     .                     UDBAIL,       WORK(LB,F1)               )

         ELSE
C
C           Find the window where the cosine of the coordinate
C           satisfies the proxy constraint.
C

            CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOCD,
     .                     ZZGFUDLT,     ZZGFCOCG,
     .                     '=',          PRXVAL,          LOCTOL,
     .                     0.D0,         CNFINE,          MW,
     .                     NW,           WORK,            RPT,
     .                     UDREPI,       UDREPU,          UDREPF,
     .                     RPTPRE,       RPTSUF,          BAIL,
     .                     UDBAIL,       WORK(LB,F1)               )

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF

C
C        Handle interrupts if necessary.
C
         IF ( BAIL ) THEN
            IF ( UDBAIL() ) THEN
               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN
            END IF
         END IF

C
C        Remove any spurious results.
C
         N = CARDD( WORK(LB,F1) )

         DO I = 1, N, 2

            START = WORK(I,F1)

            IF ( PRXFUN .EQ. 'SIN' ) THEN
C
C              Get the cosine of the coordinate at the interval start
C              time. If this cosine has the same sign as the cosine of
C              the reference value, we have a winner. Note that the
C              cosines of spurious values won't ever be close to the
C              correct values, so round-off isn't an issue.
C
               CALL ZZGFCOCG ( START, VALUE )

            ELSE
C
C              Same deal, but here we're using sines.
C
               CALL ZZGFCOSG ( START, VALUE )

            END IF

            IF (  SMSGND( CMPVAL, VALUE )  ) THEN
C
C              This is a winner.
C
               CALL WNINSD ( START, START, RESULT )

            END IF

         END DO
C
C        All done.
C
         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN

      END IF


C
C     Case: local minimum or local maximum
C
      IF (      ( UOP .EQ. 'LOCMAX' )
     .     .OR. ( UOP .EQ. 'LOCMIN' )  ) THEN
C
C        This algorithm uses 4 ZZGFREL calls, 2 of which perform
C        2 passes and 2 of which perform 1 pass.
C
         RPTSUF(1) = 'done.'
         RPTSUF(2) = 'done.'

C
C        Empty the result window.
C
         CALL SCARDD ( 0, RESULT )

C
C        We'll first find two windows covering the left and right
C        halves of the unit circle, with both halves extended
C        slightly to ensure no roots are missed. We start by
C        finding the window on which the cosine of the coordinate
C        is less than cos(LCXMRG) (which is a small, positive number).
C
         CALL LNKAN  ( WWPOOL, NODE )
         LEFT = WIX(NODE)

         RPTPRE(1) = 'Coordinate pass 1 of 6'
         RPTPRE(2) = 'Coordinate pass 2 of 6'

         PRXREL = '<'
         PRXVAL = COS(LCXMRG)

         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                   REF,    ABCORR, OBSRVR, DREF,
     .                   DVEC,   NRMSYS, NRMCRD )

         CALL ZZGFRELX ( UDSTEP,    UDREFN,              ZZGFCOCD,
     .                   ZZGFUDLT,  ZZGFCOCG, 
     .                   PRXREL,    PRXVAL,              LOCTOL,
     .                   0.D0,      CNFINE,              MW,
     .                   NW,        WORK,                RPT,
     .                   UDREPI,    UDREPU,              UDREPF,
     .                   RPTPRE,    RPTSUF,              BAIL,
     .                   UDBAIL,    WORK(LB,LEFT)                 )

C
C        Handle interrupts if necessary.
C
         IF ( BAIL ) THEN
            IF ( UDBAIL() ) THEN
               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN
            END IF
         END IF

C
C        Now search for the time period when the cosine of the
C        coordinate is greater than -cos(LCXMRG). We can save some time
C        by searching within the window designated by LEFT for the
C        complement of this window and then complementing the result of
C        that search.
C
         CALL LNKAN  ( WWPOOL, NODE )
         COMPL = WIX(NODE)

         CALL LNKAN  ( WWPOOL, NODE )
         RIGHT = WIX(NODE)


         RPTPRE(1) = 'Coordinate pass 3 of 6'
         RPTPRE(2) = 'Coordinate pass 4 of 6'

         PRXREL = '<'
         PRXVAL = -COS(LCXMRG)

         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                   REF,    ABCORR, OBSRVR, DREF,
     .                   DVEC,   NRMSYS, NRMCRD )

         CALL ZZGFRELX ( UDSTEP,    UDREFN,              ZZGFCOCD,
     .                   ZZGFUDLT,  ZZGFCOCG,
     .                   PRXREL,    PRXVAL,              LOCTOL,
     .                   0.D0,      WORK(LB,LEFT),       MW,
     .                   NW,        WORK,                RPT,
     .                   UDREPI,    UDREPU,              UDREPF,
     .                   RPTPRE,    RPTSUF,              BAIL,
     .                   UDBAIL,    WORK(LB,COMPL)               )

C
C        Handle interrupts if necessary.
C
         IF ( BAIL ) THEN
            IF ( UDBAIL() ) THEN
               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN
            END IF
         END IF

C
C        WORK(LB,COMPL) contains the complement of the window
C        we want.
C
         CALL WNDIFD ( CNFINE, WORK(LB,COMPL), WORK(LB,RIGHT) )

C
C        We're now going to find local extrema of the coordinate in the
C        windows indexed by LEFT and RIGHT.
C
         DO I = 1, 2

            IF ( I .EQ. 1 ) THEN
C
C              The sector we're searching is indexed by LEFT.
C              We'll use RA as a proxy function, since RA has no
C              singularity on the left half circle.
C
               S      = LEFT
               PRXSYS = RADSYS
               PRXCRD = RACRD

               CALL LNKAN ( WWPOOL, NODE )
               RES1 = WIX(NODE)
               RES  = RES1

               RPTPRE(1) = 'Coordinate pass 5 of 6'
               RPTPRE(2) = ' '

            ELSE

               S         = RIGHT
               PRXSYS    = LATSYS
               PRXCRD    = LONCRD

               CALL LNKAN ( WWPOOL, NODE )
               RES2 = WIX(NODE)
               RES  = RES2

               RPTPRE(1) = 'Coordinate pass 6 of 6'
               RPTPRE(2) = ' '

            END IF


            CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                      REF,    ABCORR, OBSRVR, DREF,
     .                      DVEC,   PRXSYS, PRXCRD )

            CALL ZZGFRELX ( UDSTEP,    UDREFN,              ZZGFCODC,
     .                      ZZGFUDLT,  ZZGFCOG,
     .                      UOP,       0.D0,                LOCTOL,
     .                      0.D0,      WORK(LB,S),          MW,
     .                      NW,        WORK,                RPT,
     .                      UDREPI,    UDREPU,              UDREPF,
     .                      RPTPRE,    RPTSUF,              BAIL,
     .                      UDBAIL,    WORK(LB,RES)                  )

C
C           Handle interrupts if necessary.
C
            IF ( BAIL ) THEN
               IF ( UDBAIL() ) THEN
                  CALL CHKOUT ( 'ZZGFLONG' )
                  RETURN
               END IF
            END IF

         END DO

C
C        Combine the contributions of both searches in RESULT.
C
         CALL WNUNID ( WORK(LB,RES1), WORK(LB,RES2), RESULT )

C
C        End of the LOCMIN and LOCMAX cases. RESULT is set.
C
         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN

      END IF


C
C     The remaining operators are: ABSMAX, ABSMIN, '<', '>'.
C
C     Initialize the window aliases. A value of zero indicates the
C     corresponding region hasn't been computed.
C
      TOP    = 0
      BOT    = 0
      RIGHT  = 0
      LEFT   = 0
      Q1     = 0
      Q2     = 0
      Q3     = 0
      Q4     = 0
      S      = 0
      WH     = 0
      F1     = 0
      F2     = 0

C
C     If we have an absolute extremum or inequality relation,
C     we'll need to find times when the coordinate is in the
C     various quadrants. We'll start out by setting up windows
C     for the times when the coordinate is in the top and right
C     halves of the unit circle.
C
C     The ZZGFREL call below involves two passes.
C
      IF (  ( UOP .EQ. 'ABSMAX' ) .OR. ( UOP .EQ. 'ABSMIN' ) ) THEN

         IF ( ADJUST .EQ. 0 ) THEN
            TMPLAT = 'Coordinate pass # of 7'
         ELSE
            TMPLAT = 'Coordinate pass # of 7-9'
         END IF

      ELSE
C
C        Ordinary inequality searches use 8 passes.
C
         TMPLAT = 'Coordinate pass # of 8'

      END IF

      DO I = 1, 2
         CALL REPMI ( TMPLAT, '#', I, RPTPRE(I) )
      END DO

C
C     Find the window where the sine of the coordinate is greater than
C     the sine of the branch cut avoidance tolerance.
C
C     Make sure the coordinate utilities have been initialized
C     with the actual values we'll use for our search.
C
      CALL LNKAN ( WWPOOL, NODE )
      HEAD   = NODE
      TOP    = WIX(NODE)

      PRXVAL = SIN( BCAMRG )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR, DREF,
     .                DVEC,   NRMSYS, NRMCRD )

      CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOSD,
     .               ZZGFUDLT,     ZZGFCOSG, 
     .               '>',          PRXVAL,          LOCTOL,
     .               0.D0,         CNFINE,          MW,
     .               NW,           WORK,            RPT,
     .               UDREPI,       UDREPU,          UDREPF,
     .               RPTPRE,       RPTSUF,          BAIL,
     .               UDBAIL,       WORK(LB,TOP)              )

C
C     2 passes done.
C
      TOTAL = 2

      IF ( BAIL ) THEN
         IF ( UDBAIL() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF
      END IF

C
C     Find the window where the sine of the coordinate is less than
C     the negative of the sine of the branch cut avoidance tolerance.
C
C     Make sure the coordinate utilities have been initialized
C     with the actual values we'll use for our search.
C
C     The ZZGFREL call below involves two passes.
C
      DO I = 1, 2
         CALL REPMI ( TMPLAT, '#', TOTAL+I, RPTPRE(I) )
      END DO


      CALL LNKAN  ( WWPOOL, NODE )
      CALL LNKILA ( HEAD,   NODE,   WWPOOL )
      BOT = WIX(NODE)

      PRXVAL = -SIN( BCAMRG )


      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR, DREF,
     .                DVEC,   NRMSYS, NRMCRD )

      CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOSD,
     .               ZZGFUDLT,     ZZGFCOSG,
     .               '<',          PRXVAL,          LOCTOL,
     .               0.D0,         CNFINE,          MW,
     .               NW,           WORK,            RPT,
     .               UDREPI,       UDREPU,          UDREPF,
     .               RPTPRE,       RPTSUF,          BAIL,
     .               UDBAIL,       WORK(LB,BOT)              )

C
C     4 passes done.
C
      TOTAL = TOTAL + 2

      IF ( BAIL ) THEN
         IF ( UDBAIL() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF
      END IF

C
C     Find the window where the cosine of the coordinate is
C     greater than zero.
C
C
C     The ZZGFREL call below involves two passes.
C
      DO I = 1, 2
         CALL REPMI ( TMPLAT, '#', TOTAL+I, RPTPRE(I) )
      END DO

C
C     We'll keep all of the allocated nodes linked together.
C     Since the order of the nodes is unimportant, we insert
C     each new node following the head node; this is non-standard
C     but ensures the list head doesn't change until we delete
C     nodes from the list.
C
      CALL LNKAN  ( WWPOOL, NODE )
      CALL LNKILA ( HEAD,   NODE,  WWPOOL )
      RIGHT  = WIX(NODE)

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR, DREF,
     .                DVEC,   NRMSYS, NRMCRD )

      CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOCD,
     .               ZZGFUDLT,     ZZGFCOCG,
     .               '>',          0.D0,            LOCTOL,
     .               0.D0,         CNFINE,          MW,
     .               NW,           WORK,            RPT,
     .               UDREPI,       UDREPU,          UDREPF,
     .               RPTPRE,       RPTSUF,          BAIL,
     .               UDBAIL,       WORK(LB,RIGHT)            )

C
C     6 passes done.
C
      TOTAL = TOTAL + 2

      IF ( BAIL ) THEN
         IF ( UDBAIL() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF
      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN
      END IF


C
C     Now find the absolute extremum, if this was requested.
C
      IF (      ( UOP .EQ. 'ABSMAX' )
     .     .OR. ( UOP .EQ. 'ABSMIN' )  ) THEN
C
C        If we're looking for an absolute extremum and the
C        adjustment value is 0, each ZZGFREL call executes
C        on search pass; otherwise these calls execute two
C        search passes.
C
         IF ( NRMCRD .EQ. LONCRD ) THEN
C
C           We need windows when the coordinate is in quadrants 2 and
C           3. We can derive these from the windows TOP and RIGHT
C           without additional searches.
C
            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            Q2  = WIX(NODE)

            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            Q3  = WIX(NODE)

C
C           Compute windows for the second and third quadrants. Note
C           that these windows are bounded away from the branch cut
C           at pi radians, since windows TOP and BOT have been
C           trimmed.
C
            CALL WNDIFD( WORK(LB,TOP), WORK(LB,RIGHT), WORK(LB,Q2)  )
            CALL WNDIFD( WORK(LB,BOT), WORK(LB,RIGHT), WORK(LB,Q3 ) )

            IF ( UOP .EQ. 'ABSMAX' ) THEN

               REGION(1) = Q2
               REGION(2) = RIGHT
               REGION(3) = Q3
            ELSE
               REGION(1) = Q3
               REGION(2) = RIGHT
               REGION(3) = Q2
            END IF


         ELSE IF ( NRMCRD .EQ. RACRD ) THEN
C
C           We need windows when the coordinate is in quadrants 1 and
C           4, and the window when the coordinate is in the left half
C           of the unit circle. We can derive these from the windows
C           TOP and RIGHT without additional searches.
C
            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            Q1  = WIX(NODE)

            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            LEFT = WIX(NODE)

            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            Q4  = WIX(NODE)

C
C           Compute windows for the first and fourth quadrants. Note
C           that these windows are bounded away from the branch cut
C           at pi radians, since windows TOP and BOT have been
C           trimmed. Also compute the window LEFT, which is the
C           complement of window RIGHT.
C
            CALL WNINTD( WORK(LB,RIGHT), WORK(LB,TOP),   WORK(LB,Q1)   )
            CALL WNINTD( WORK(LB,RIGHT), WORK(LB,BOT),   WORK(LB,Q4)   )
            CALL WNDIFD( CNFINE,         WORK(LB,RIGHT), WORK(LB,LEFT) )

            IF ( UOP .EQ. 'ABSMAX' ) THEN

               REGION(1) = Q4
               REGION(2) = LEFT
               REGION(3) = Q1
            ELSE
               REGION(1) = Q1
               REGION(2) = LEFT
               REGION(3) = Q4
            END IF

         ELSE
C
C           We're not expecting to see a coordinate other than
C           longitude or RA here.
C
            CALL SETMSG ( 'Unexpected coordinate # (0)' )
            CALL ERRCH  ( '#', NRMCRD                   )
            CALL SIGERR ( 'SPICE(BUG)'                  )
            CALL CHKOUT ( 'ZZGFLONG'                    )
            RETURN

         END IF

C
C        Now search the list of regions for the specified
C        extremum.
C
         FOUND = .FALSE.
         I     = 1

         DO WHILE (  ( I .LE. 3 ) .AND. ( .NOT. FOUND )  )
C
C           Search region I. Set the reference and adjustment
C           values to 0 for this search.
C
C           The ZZGFREL call below executes 1 pass, since it's
C           doing an absolute extremum search with 0 adjustment
C           value (even if ADJUST is non-zero).
C
            CALL REPMI ( TMPLAT, '#', TOTAL+1, RPTPRE(1) )
            RPTPRE(2) = ' '

            CALL SCARDD ( 0, RESULT )
C
C           Perform our searches with functions that have no branch
C           cuts near the region boundaries.
C
            IF (      ( REGION(I) .EQ. Q1    )
     .           .OR. ( REGION(I) .EQ. Q4    )
     .           .OR. ( REGION(I) .EQ. RIGHT )  ) THEN

               PRXSYS = LATSYS
               PRXCRD = LONCRD
            ELSE
               PRXSYS = RADSYS
               PRXCRD = RACRD
            END IF

            CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                      REF,    ABCORR, OBSRVR, DREF,
     .                      DVEC,   PRXSYS, PRXCRD )

            CALL ZZGFRELX( UDSTEP,    UDREFN,              ZZGFCODC,
     .                     ZZGFUDLT,  ZZGFCOCG,
     .                     UOP,       0.D0,                LOCTOL,
     .                     0.D0,      WORK(LB,REGION(I)),  MW,
     .                     NW,        WORK,                RPT,
     .                     UDREPI,    UDREPU,              UDREPF,
     .                     RPTPRE,    RPTSUF,              BAIL,
     .                     UDBAIL,    RESULT                      )

C
C           ZZGFREL will have performed a pass only if the confinement
C           window was non-empty.
C
            IF (  CARDD( WORK(LB, REGION(I)) )  .GT. 0  ) THEN
C
C              Another pass has been completed.
C
               TOTAL = TOTAL + 1

            END IF

            IF ( BAIL ) THEN
               IF ( UDBAIL() ) THEN
                  CALL CHKOUT ( 'ZZGFLONG' )
                  RETURN
               END IF
            END IF


            IF ( WNCARD(RESULT) .GT. 0 ) THEN
C
C              We found an extremum. We don't have to search further.
C
               FOUND = .TRUE.
            ELSE
               I     = I + 1
            END IF

         END DO


         IF ( ADJUST .EQ. 0.D0 ) THEN
C
C           The result we have is the final result.
C
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN

         END IF

C
C        This is the case of an absolute extremum search with
C        non-zero adjustment value.
C
C        We'll need to obtain the extreme value.
C
         ET = RESULT(1)


         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                   REF,    ABCORR, OBSRVR, DREF,
     .                   DVEC,   NRMSYS, NRMCRD )

         CALL ZZGFCOG ( ET, EXTVAL )

C
C        Re-set the operator and reference value to enable
C        us to conduct an inequality search.
C
         IF ( UOP .EQ. 'ABSMAX' ) THEN

            IF ( NRMCRD .EQ. LONCRD ) THEN

               XRFVAL = MAX( EXTVAL - ADJUST,  -PI() )
            ELSE
               XRFVAL = MAX( EXTVAL - ADJUST,   0.D0 )
            END IF

            UOP    = '>'

         ELSE

            IF ( NRMCRD .EQ. LONCRD ) THEN

               XRFVAL = MIN( EXTVAL + ADJUST,   PI()    )
            ELSE
               XRFVAL = MIN( EXTVAL + ADJUST,   TWOPI() )
            END IF

            UOP    = '<'

         END IF

      END IF

C
C     Case: inequality
C
C     Searches for absolute extrema with non-zero adjustment values
C     also use this code block.
C
      IF (  ( UOP .EQ. '<' )  .OR.  ( UOP .EQ. '>' )  ) THEN
C
C        We'll find the window when the coordinate is less than
C        the reference value. If the relation is '>', we'll
C        complement the result. Let FLIP indicate whether
C        we need to take the complement of our result at the
C        end of the search.
C
         IF ( UOP .EQ. '>' ) THEN

            UOP  = '<'
            FLIP = .TRUE.
         ELSE
            FLIP = .FALSE.
         END IF

C
C        We'll need the sine and cosine of the reference value.
C
         CV = COS ( XRFVAL )
         SV = SIN ( XRFVAL )

C
C        Determine the quadrant QUAD of the reference value.
C
         LOCREF = DATAN2( SV, CV )

         IF ( LOCREF .LT. -PI()/2 ) THEN
            QUAD = 3
         ELSE IF ( LOCREF .LT. 0.D0 ) THEN
            QUAD = 4
         ELSE IF ( LOCREF .LT. PI()/2 ) THEN
            QUAD = 1
         ELSE
            QUAD = 2
         END IF

C
C        Create a list of region windows to compute. The order
C        of list items is significant: the regions will
C        be computed in the order in which they're listed.
C
         IF ( NRMCRD .EQ. LONCRD ) THEN

            NL       = 2
            RLIST(1) = 'Q2'
            RLIST(2) = 'Q3'

         ELSE

            NL       = 3
            RLIST(1) = 'LEFT'
            RLIST(2) = 'Q1'
            RLIST(3) = 'Q4'

         END IF

C
C        Compute all of the region windows.
C
C        We make use of the fact that windows TOP and RIGHT
C        have already been computed.
C
         DO I = 1, NL

            IF ( ( RLIST(I) .EQ. 'LEFT' ) .AND. ( LEFT .EQ. 0 ) ) THEN

               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               LEFT = WIX(NODE)

               CALL WNDIFD ( CNFINE, WORK(LB,RIGHT), WORK(LB,LEFT) )

            ELSE IF ( ( RLIST(I) .EQ. 'Q1' ) .AND. ( Q1 .EQ. 0 ) ) THEN

               IF ( Q1 .EQ. 0 ) THEN

                  CALL LNKAN  ( WWPOOL, NODE )
                  CALL LNKILA ( HEAD,   NODE,   WWPOOL )
                  Q1 = WIX(NODE)

               END IF

               CALL WNINTD ( WORK(LB,RIGHT), WORK(LB,TOP), WORK(LB,Q1) )


            ELSE IF ( ( RLIST(I) .EQ. 'Q2' ) .AND. ( Q2 .EQ. 0 ) ) THEN

               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               Q2 = WIX(NODE)

               CALL WNDIFD ( WORK(LB,TOP), WORK(LB,RIGHT), WORK(LB,Q2) )


            ELSE IF ( ( RLIST(I) .EQ. 'Q3' ) .AND. ( Q3 .EQ. 0 ) ) THEN
C
C              Note: we need the bottom window in order to compute Q3!
C
               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               Q3 = WIX(NODE)

               CALL WNDIFD ( WORK(LB,BOT), WORK(LB,RIGHT), WORK(LB,Q3) )


            ELSE IF ( ( RLIST(I) .EQ. 'Q4' ) .AND. ( Q4 .EQ. 0 ) ) THEN
C
C              NOTE: We need the bottom window in order to compute Q4!
C
               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               Q4 = WIX(NODE)

               CALL WNINTD ( WORK(LB,RIGHT), WORK(LB,BOT), WORK(LB,Q4) )

            END IF

         END DO

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF


C
C        Now decide the sector and proxy function we'll use to
C        search for the time when the reference value is hit.
C
         IF ( NRMCRD .EQ. LONCRD ) THEN

            IF ( QUAD .EQ. 1 ) THEN

               S      = RIGHT
               PRXFUN = LONCRD

            ELSE IF ( QUAD .EQ. 2 ) THEN

               S      = Q2
               PRXFUN = RACRD

            ELSE IF ( QUAD .EQ. 3 ) THEN

               S      = Q3
               PRXFUN = RACRD

            ELSE

               S      = RIGHT
               PRXFUN = LONCRD

            END IF

         ELSE

            IF ( QUAD .EQ. 1 ) THEN

               S      = Q1
               PRXFUN = LONCRD

            ELSE IF ( QUAD .EQ. 2 ) THEN

               S      = LEFT
               PRXFUN = RACRD

            ELSE IF ( QUAD .EQ. 3 ) THEN

               S      = LEFT
               PRXFUN = RACRD

            ELSE

               S      = Q4
               PRXFUN = LONCRD

            END IF


         END IF

C
C        Set the proxy reference value based on the input
C        reference value and the choice of proxy function.
C
         IF ( PRXFUN .EQ. LONCRD ) THEN

            PRXVAL = DATAN2 ( SV, CV )

         ELSE

            PRXVAL = DATAN2 ( SV, CV )

            IF ( PRXVAL .LT. 0.D0 ) THEN
               PRXVAL = PRXVAL + TWOPI()
            END IF

         END IF

C
C        We're going to need additional windows in order to search
C        quadrant Q. At this point, we're going to de-allocate all
C        windows except those needed for the upcoming searches.
C
C        Create the set NEEDWN of the windows we need to retain.
C
         CALL SSIZEI ( NWLONG, NEEDWN )


         IF ( NRMCRD .EQ. LONCRD ) THEN

            CALL INSRTI ( Q2,    NEEDWN )
            CALL INSRTI ( Q3,    NEEDWN )
            CALL INSRTI ( RIGHT, NEEDWN )
         ELSE
            CALL INSRTI ( Q1,    NEEDWN )
            CALL INSRTI ( Q4,    NEEDWN )
            CALL INSRTI ( LEFT,  NEEDWN )
         END IF

C
C        Now delete all windows not referenced by NEEDWN.
C
         NODE = HEAD

         DO WHILE ( NODE .GT. 0 )
C
C           Find the next node in the list.
C
            NEXT = LNKNXT ( NODE, WWPOOL )

            IF (  .NOT.  ELEMI( WIX(NODE), NEEDWN )  )  THEN
C
C              Delete NODE; update HEAD if we deleted the head node.
C
               CALL LNKFSL ( NODE, NODE, WWPOOL )

               IF ( HEAD .EQ. NODE ) THEN
                  HEAD = NEXT
               END IF

            END IF
C
C           Prepare to look at the next node.
C
            NODE = NEXT

         END DO



         IF ( NRMCRD .EQ. LONCRD ) THEN
C
C           This is a longitude search.
C
C           For each quadrant, identify or compute the window on which
C           the constraint is automatically satisfied. Store the result
C           in workspace window F1. If this window is empty, set F1 to
C           0.
C
            IF ( QUAD .EQ. 1 ) THEN

               F1 = Q3

            ELSE IF ( QUAD .EQ. 2 ) THEN

               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               F1 = WIX(NODE)

               CALL WNUNID ( WORK(LB,Q3), WORK(LB,RIGHT), WORK(LB,F1) )

            ELSE IF ( QUAD .EQ. 3 ) THEN

               F1 = 0

            ELSE
C
C              QUAD is 4.
C
               F1 = Q3

            END IF


         ELSE
C
C           We're working with RA.
C
            IF ( QUAD .EQ. 1 ) THEN

               F1 = 0

            ELSE IF ( QUAD .EQ. 2 ) THEN

               F1 = Q1

            ELSE IF ( QUAD .EQ. 3 ) THEN

               F1 = Q1

            ELSE
C
C              QUAD is 4.
C
               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               F1 = WIX(NODE)

               CALL WNUNID ( WORK(LB,LEFT), WORK(LB,Q1), WORK(LB,F1) )

            END IF

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF

C
C        Search sector S to find times when the relation
C
C           PRXFUN PRXREL PRXVAL
C
C        holds.
C
C        Allocate window F2 to hold the result of the search.
C
C
         DO I = 1, 2
            CALL REPMI ( TMPLAT, '#', TOTAL+I, RPTPRE(I) )
         END DO

         CALL LNKAN  ( WWPOOL, NODE )
         CALL LNKILA ( HEAD,   NODE,   WWPOOL )

         F2 = WIX(NODE)

         CALL SCARDD ( 0, WORK(LB,F2) )

         IF ( PRXFUN .EQ. LONCRD ) THEN

C
C           Initialize the proxy search in sector S, then perform the
C           search.
C
            CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                      REF,    ABCORR, OBSRVR, DREF,
     .                      DVEC,   LATSYS, LONCRD )

            CALL ZZGFRELX ( UDSTEP,    UDREFN,     ZZGFCODC,
     .                      ZZGFUDLT,  ZZGFCOG,
     .                      '<',       PRXVAL,      LOCTOL,
     .                      0.D0,      WORK(LB,S),  MW,
     .                      NW,        WORK,        RPT,
     .                      UDREPI,    UDREPU,      UDREPF,
     .                      RPTPRE,    RPTSUF,      BAIL,
     .                      UDBAIL,    WORK(LB,F2)          )

         ELSE

C
C           Initialize the proxy search in sector S, then perform the
C           search.
C
            CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                      REF,    ABCORR, OBSRVR, DREF,
     .                      DVEC,   RADSYS, RACRD )

            CALL ZZGFRELX ( UDSTEP,    UDREFN,      ZZGFCODC,
     .                      ZZGFUDLT,  ZZGFCOG,
     .                      '<',       PRXVAL,      LOCTOL,
     .                      0.D0,      WORK(LB,S),  MW,
     .                      NW,        WORK,        RPT,
     .                      UDREPI,    UDREPU,      UDREPF,
     .                      RPTPRE,    RPTSUF,      BAIL,
     .                      UDBAIL,    WORK(LB,F2)            )

         END IF

C
C        7 + 0:2 passes done for adjusted extrema.
C
         IF ( BAIL ) THEN
            IF ( UDBAIL() ) THEN
               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN
            END IF
         END IF

C
C        Combine the contents of windows F1 and F2 to obtain
C        the result.
C
         IF ( F1 .NE. 0 ) THEN

            CALL WNUNID ( WORK(LB,F1), WORK(LB,F2), RESULT )

         ELSE

            CALL COPYD  ( WORK(LB,F2),              RESULT )

         END IF

C
C        Last step: complement the result if necessary.
C
         IF ( FLIP ) THEN

C
C           Create the window relative to which we'll find
C           the complement of RESULT. The window we seek
C           is not CNFINE, but rather a union of windows
C           that avoids the branch cut.
C
            CALL LNKAN ( WWPOOL, NODE )
            WH = WIX(NODE)

            IF ( NRMCRD .EQ. LONCRD ) THEN

               CALL WNUNID ( WORK(LB,Q2), WORK(LB,RIGHT), WORK(LB,F2) )
               CALL WNUNID ( WORK(LB,Q3), WORK(LB,F2),    WORK(LB,WH) )
            ELSE
               CALL WNUNID ( WORK(LB,Q1), WORK(LB,LEFT),  WORK(LB,F2) )
               CALL WNUNID ( WORK(LB,Q4), WORK(LB,F2),    WORK(LB,WH) )
            END IF

C
C           We use F2 as a temporary window index, since F2 is
C           guaranteed to exist at this point and is distinct from WH.
C
            CALL WNDIFD ( WORK(LB,WH), RESULT, WORK(LB,F2) )
            CALL COPYD  ( WORK(LB,F2),         RESULT      )

         END IF

      END IF

      CALL CHKOUT (  'ZZGFLONG' )
      RETURN
      END
