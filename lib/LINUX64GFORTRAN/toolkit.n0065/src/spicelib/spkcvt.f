C$Procedure SPKCVT ( SPK, constant velocity target state )
 
      SUBROUTINE SPKCVT ( TRGSTA, TRGEPC, TRGCTR, TRGREF, ET,
     .                    OUTREF, REFLOC, ABCORR, OBSRVR, STATE, LT )

C$ Abstract
C
C     Return the state, relative to a specified observer, of a target
C     having constant velocity in a specified reference frame. The
C     target's state is provided by the calling program rather than by
C     loaded SPK files.
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
C     FRAMES
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzctr.inc'

      DOUBLE PRECISION      TRGSTA ( 6 )
      DOUBLE PRECISION      TRGEPC
      CHARACTER*(*)         TRGCTR
      CHARACTER*(*)         TRGREF
      DOUBLE PRECISION      ET
      CHARACTER*(*)         OUTREF
      CHARACTER*(*)         REFLOC
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      LT


C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TRGSTA     I   Target state relative to center of motion.
C     TRGEPC     I   Epoch of target state.
C     TRGCTR     I   Center of motion of target.
C     TRGREF     I   Frame of target state.
C     ET         I   Observation epoch.
C     OUTREF     I   Reference frame of output state.
C     REFLOC     I   Output reference frame evaluation locus.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Name of observing ephemeris object.
C     STATE      O   State of target with respect to observer.
C     LT         O   One way light time between target and
C                    observer.
C
C$ Detailed_Input
C
C     TRGSTA      is the geometric state of a target moving at constant
C                 velocity relative to its center of motion TRGCTR,
C                 expressed in the reference frame TRGREF, at the epoch
C                 TRGEPC.
C
C                 TRGSTA is a six-dimensional vector representing
C                 position and velocity in cartesian coordinates: the
C                 first three components represent the position of a
C                 target relative to its center of motion; the last
C                 three components represent the velocity of the
C                 target.
C
C                 Units are always km and km/sec.
C
C
C     TRGEPC      is the epoch, expressed as seconds past J2000 TDB, at
C                 which the target state TRGSTA is applicable. For
C                 other epochs, the position of the target relative to
C                 its center of motion is linearly extrapolated from
C                 the position at TRGEPC using the velocity component
C                 of TRGSTA.
C
C                 TRGEPC is independent of the epoch ET at which the
C                 state of the target relative to the observer is to be
C                 computed.
C
C
C     TRGCTR      is the name of the center of motion of TRGSTA. The
C                 ephemeris of TRGCTR is provided by loaded SPK files.
C
C                 Optionally, you may supply the integer ID code for
C                 the object as an integer string. For example both
C                 'MOON' and '301' are legitimate strings that indicate
C                 the moon is the center of motion.
C
C                 Case and leading and trailing blanks are not
C                 significant in the string TRGCTR.
C
C
C     TRGREF      is the name of the reference frame relative to which
C                 the input state TRGSTA is expressed. The target has
C                 constant velocity relative to its center of motion 
C                 in this reference frame.
C
C                 Case and leading and trailing blanks are not
C                 significant in the string TRGREF.
C
C
C     ET          is the ephemeris time at which the state of the
C                 target relative to the observer is to be computed. ET
C                 is expressed as seconds past J2000 TDB. ET refers to
C                 time at the observer's location.
C
C                 ET is independent of the target epoch TRGEPC.
C
C
C     OUTREF      is the name of the reference frame with respect to
C                 which the output state is expressed.
C
C                 When OUTREF is time-dependent (non-inertial), its
C                 orientation relative to the J2000 frame is evaluated
C                 in the manner commanded by the input argument REFLOC
C                 (see description below).
C
C                 Case and leading and trailing blanks are not
C                 significant in the string OUTREF.
C 
C
C     REFLOC      is a string indicating the output reference frame
C                 evaluation locus: this is the location associated
C                 with the epoch at which this routine is to evaluate
C                 the orientation, relative to the J2000 frame, of the
C                 output frame OUTREF. The values and meanings of
C                 REFLOC are:
C
C                    'OBSERVER'  Evaluate OUTREF at the observer's
C                                epoch ET.
C
C                                Normally the locus 'OBSERVER' should
C                                be selected when OUTREF is centered
C                                at the observer.
C
C
C                    'TARGET'    Evaluate OUTREF at the target epoch;
C                                letting LT be the one-way light time 
C                                between the target and observer, the
C                                target epoch is
C
C                                   ET-LT  if reception aberration
C                                          corrections are used
C
C                                   ET+LT  if transmission aberration
C                                          corrections are used
C
C                                   ET     if no aberration corrections
C                                          are used
C
C                                Normally the locus 'TARGET' should
C                                be selected when OUTREF is TRGREF,
C                                the frame in which the target state
C                                is specified.
C
C
C                    'CENTER'    Evaluate the frame OUTREF at the epoch
C                                associated its center. This epoch,
C                                which we'll call ETCTR, is determined
C                                as follows:
C
C                                   Let LTCTR be the one-way light time
C                                   between the observer and the center 
C                                   of OUTREF. Then ETCTR is
C
C                                      ET-LTCTR  if reception
C                                                aberration corrections
C                                                are used
C
C                                      ET+LTCTR  if transmission
C                                                aberration corrections
C                                                are used
C
C                                      ET        if no aberration
C                                                corrections are used
C
C
C                                The locus 'CENTER' should be selected
C                                when the user intends to obtain
C                                results compatible with those produced
C                                by SPKEZR. 
C 
C                 When OUTREF is inertial, all choices of REFLOC 
C                 yield the same results.
C     
C                 Case and leading and trailing blanks are not
C                 significant in the string REFLOC.
C
C
C     ABCORR      indicates the aberration corrections to be applied to
C                 observer-target state to account for one-way light
C                 time and stellar aberration.
C                  
C                 ABCORR may be any of the following:
C
C                    'NONE'     Apply no correction. Return the 
C                               geometric state of the target 
C                               relative to the observer.  
C
C                 The following values of ABCORR apply to the
C                 "reception" case in which photons depart from the
C                 target's location at the light-time corrected epoch
C                 ET-LT and *arrive* at the observer's location at ET:
C
C                    'LT'       Correct for one-way light time (also
C                               called "planetary aberration") using a
C                               Newtonian formulation. This correction
C                               yields the state of the target at the
C                               moment it emitted photons arriving at
C                               the observer at ET.
C
C                               The light time correction uses an
C                               iterative solution of the light time
C                               equation. The solution invoked by the
C                               'LT' option uses one iteration.
C
C                    'LT+S'     Correct for one-way light time and
C                               stellar aberration using a Newtonian
C                               formulation. This option modifies the
C                               state obtained with the 'LT' option to
C                               account for the observer's velocity
C                               relative to the solar system
C                               barycenter. The result is the apparent
C                               state of the target---the position and
C                               velocity of the target as seen by the
C                               observer.
C
C                    'CN'       Converged Newtonian light time
C                               correction. In solving the light time
C                               equation, the 'CN' correction iterates
C                               until the solution converges.
C
C                    'CN+S'     Converged Newtonian light time
C                               and stellar aberration corrections.
C
C
C                 The following values of ABCORR apply to the
C                 "transmission" case in which photons *depart* from
C                 the observer's location at ET and arrive at the
C                 target's location at the light-time corrected epoch
C                 ET+LT:
C
C                    'XLT'      "Transmission" case:  correct for
C                               one-way light time using a Newtonian
C                               formulation. This correction yields the
C                               state of the target at the moment it
C                               receives photons emitted from the
C                               observer's location at ET.
C
C                    'XLT+S'    "Transmission" case:  correct for
C                               one-way light time and stellar
C                               aberration using a Newtonian
C                               formulation  This option modifies the
C                               state obtained with the 'XLT' option to
C                               account for the observer's velocity
C                               relative to the solar system
C                               barycenter. The position component of
C                               the computed target state indicates the
C                               direction that photons emitted from the
C                               observer's location must be "aimed" to
C                               hit the target.
C
C                    'XCN'      "Transmission" case:  converged 
C                               Newtonian light time correction.
C
C                    'XCN+S'    "Transmission" case:  converged 
C                               Newtonian light time and stellar 
C                               aberration corrections.
C
C
C                 Neither special nor general relativistic effects are
C                 accounted for in the aberration corrections applied
C                 by this routine.
C
C                 Case and leading and trailing blanks are not
C                 significant in the string ABCORR.
C
C
C     OBSRVR      is the name of an observing body. Optionally, you
C                 may supply the ID code of the object as an integer
C                 string. For example, both 'EARTH' and '399' are
C                 legitimate strings to supply to indicate the
C                 observer is Earth.
C
C                 Case and leading and trailing blanks are not
C                 significant in the string OBSRVR.
C
C                                 
C$ Detailed_Output
C
C     STATE       is a Cartesian state vector representing the position
C                 and velocity of the target relative to the specified
C                 observer. STATE is corrected for the specified
C                 aberrations and is expressed with respect to the
C                 reference frame specified by OUTREF. The first three
C                 components of STATE represent the x-, y- and
C                 z-components of the target's position; the last three
C                 components form the corresponding velocity vector.
C
C                 The position component of STATE points from the
C                 observer's location at ET to the aberration-corrected
C                 location of the target. Note that the sense of the
C                 position vector is independent of the direction of
C                 radiation travel implied by the aberration
C                 correction.
C
C                 The velocity component of STATE is the derivative
C                 with respect to time of the position component of
C                 STATE.
C
C                 Units are always km and km/sec.
C
C                 When STATE is expressed in a time-dependent
C                 (non-inertial) output frame, the orientation of that
C                 frame relative to the J2000 frame is evaluated in the
C                 manner indicated by the input argument REFLOC (see
C                 description above).
C
C
C     LT          is the one-way light time between the observer and
C                 target in seconds. If the target state is corrected 
C                 for aberrations, then LT is the one-way light time 
C                 between the observer and the light time corrected 
C                 target location.
C
C 
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either the name of the center of motion or the observer
C         cannot be translated to its NAIF ID code, the error
C         SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If the reference frame OUTREF is unrecognized, the error
C         SPICE(UNKNOWNFRAME) will be signaled.
C
C     3)  If the reference frame TRGREF is unrecognized, the error will
C         be diagnosed by a routine in the call tree of this routine.
C
C     4)  If the frame evaluation locus REFLOC is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     5)  If the loaded kernels provide insufficient data to compute
C         the requested state vector, the deficiency will be diagnosed
C         by a routine in the call tree of this routine.
C
C     6)  If an error occurs while reading an SPK or other kernel file,
C         the error will be diagnosed by a routine in the call tree of
C         this routine.
C
C     7)  If the aberration correction ABCORR is not recognized, 
C         the error will be diagnosed by a routine in the call tree of
C         this routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        -  SPK data: ephemeris data for target center and observer
C           must be loaded. If aberration corrections are used, the
C           states of target center and observer relative to the solar
C           system barycenter must be calculable from the available
C           ephemeris data. Typically ephemeris data are made available
C           by loading one or more SPK files using FURNSH.
C
C     The following data may be required:
C
C        -  PCK data: if the target frame is a PCK frame, rotation data
C           for the target frame must be loaded. These may be provided
C           in a text or binary PCK file.
C
C        -  Frame data: if a frame definition not built into SPICE is
C           required, for example to convert the observer-target state
C           to the output frame, that definition must be available in
C           the kernel pool. Typically frame definitions are supplied
C           by loading a frame kernel using FURNSH.
C
C        -  Additional kernels: if any frame used in this routine's
C           state computation is a CK frame, then at least one CK and
C           corresponding SCLK kernel is required. If dynamic frames
C           are used, additional SPK, PCK, CK, or SCLK kernels may be
C           required.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine computes observer-target states for targets whose
C     trajectories are not provided by SPK files.
C
C     Targets supported by this routine must have constant velocity
C     with respect to a specified center of motion, expressed in a
C     caller-specified reference frame. The state of the center of
C     motion relative to the observer must be computable using 
C     loaded SPK data.
C
C     For applications in which the target has zero velocity
C     relative to its center of motion, the SPICELIB routine 
C
C        SPKCPT     { SPK, constant position target }
C
C     can be used. SPKCPT has a simpler interface than that of SPKCVT.
C     
C     This routine is suitable for computing states of landmarks on the
C     surface of an extended object, as seen by a specified observer,
C     in cases where no SPK data are available for those landmarks.
C
C     This routine's treatment of the output reference frame differs
C     from that of the principal SPK API routines
C
C        SPKEZR
C        SPKEZ
C        SPKPOS
C        SPKEZP
C
C     which require both observer and target ephemerides to be provided
C     by loaded SPK files:
C
C        The SPK API routines listed above evaluate the orientation of
C        the output reference frame (with respect to the J2000 frame)
C        at an epoch corrected for one-way light time between the
C        observer and the center of the output frame. When the center
C        of the output frame is not the target (for example, when the
C        target is on the surface of Mars and the output frame is
C        centered at Mars' center), the epoch of evaluation may not
C        closely match the light-time corrected epoch associated with
C        the target itself.
C 
C        This routine allows the caller to dictate how the orientation
C        of the output reference frame is to be evaluated. The caller
C        passes to this routine an input string called the output
C        frame's evaluation "locus." This string specifies the location
C        associated with the output frame's evaluation epoch. The three
C        possible values of the locus are
C
C           'TARGET'
C           'OBSERVER'
C           'CENTER'        
C
C        The choice of locus has an effect when aberration corrections
C        are used and the output frame is non-inertial.
C
C        When the locus is 'TARGET' and light time corrections are
C        used, the orientation of the output frame is evaluated at the
C        epoch obtained by correcting the observation epoch ET for
C        one-way light time LT. The evaluation epoch will be either
C        ET-LT or ET+LT for reception or transmission corrections
C        respectively.
C
C        For remote sensing applications where the target is a surface
C        point on an extended object, and the orientation of that
C        object should be evaluated at the emission time, the locus
C        'TARGET' should be used.
C
C        When the output frame's orientation should be evaluated at
C        the observation epoch ET, which is the case when the 
C        output frame is centered at the observer, the locus 
C        'OBSERVER' should be used.
C        
C        The locus option 'CENTER' is provided for compatibility
C        with existing SPK state computation APIs such as SPKEZR.
C
C        Note that the output frame evaluation locus does not affect
C        the computation of light time between the target and
C        observer. 
C
C
C     The SPK routines that compute observer-target states for
C     combinations of objects having ephemerides provided by the SPK
C     system and objects having constant position or constant velocity
C     are
C
C        SPKCPO {SPK, Constant position observer}
C        SPKCPT {SPK, Constant position target}
C        SPKCVO {SPK, Constant velocity observer}
C        SPKCVT {SPK, Constant velocity target}
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     1) Demonstrate use of this routine; in particular demonstrate
C        applications of the output frame evaluation locus. 
C     
C        The following program is not necessarily realistic: for
C        brevity, it combines several unrelated computations.
C
C        Task Description
C        ================
C
C        Find the state of a given surface point on earth, corrected
C        for light time and stellar aberration, relative to the Mars
C        Global Surveyor spacecraft, expressed in the earth fixed
C        reference frame ITRF93. The selected point is the position
C        of the DSN station DSS-14.
C
C        Contrast the states computed by setting the output frame
C        evaluation locus to 'TARGET' and to 'CENTER'. Show that the
C        latter choice produces results very close to those that
C        can be obtained using SPKEZR.
C
C        Also compute the state of a selected Mars surface point as
C        seen from MGS. The point we'll use is the narrow angle MOC
C        boresight surface intercept corresponding to the chosen
C        observation time. Express the state in a spacecraft-centered
C        reference frame. Use the output frame evaluation locus
C        'OBSERVER' for this computation.
C 
C        The observation epoch is 2003 OCT 13 06:00:00 UTC.
C
C
C        Kernels
C        =======     
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File name: spkcvt.tm
C
C           This is the meta-kernel file for the header code example for
C           the subroutine SPKCVT. The kernel files referenced by this
C           meta-kernel can be found on the NAIF website.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                        Contents
C              ---------                        --------
C              de421.bsp                        Planetary ephemeris
C              pck00010.tpc                     Planet orientation and
C                                               radii
C              naif0010.tls                     Leapseconds
C              earth_720101_070426.bpc          Earth historical
C                                               binary PCK
C              earthstns_itrf93_050714.bsp      DSN station SPK
C              mgs_moc_v20.ti                   MGS MOC instrument
C                                               parameters
C              mgs_sclkscet_00061.tsc           MGS SCLK coefficients
C              mgs_sc_ext12.bc                  MGS s/c bus attitude
C              mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris
C
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'de421.bsp',
C                               'pck00010.tpc',
C                               'naif0010.tls',
C                               'earth_720101_070426.bpc',
C                               'earthstns_itrf93_050714.bsp',
C                               'mgs_moc_v20.ti',
C                               'mgs_sclkscet_00061.tsc',
C                               'mgs_sc_ext12.bc',
C                               'mgs_ext12_ipng_mgs95j.bsp'  )
C
C           \begintext
C
C           End of meta-kernel.
C
C
C        Example code begins here.
C     
C
C        C
C        C     Program: EX1
C        C
C        C     This program demonstrates the use of SPKCVT.
C        C     Computations are performed using all three possible
C        C     values of the output frame evaluation locus REFLOC:
C        C
C        C        'TARGET'
C        C        'OBSERVER'
C        C        'CENTER'
C        C
C        C     Several unrelated computations are performed in
C        C     this program. In particular, computations
C        C     involving a surface point on Mars are included
C        C     simply to demonstrate use of the 'OBSERVER'
C        C     option.
C        C
C
C
C              PROGRAM EX1
C              IMPLICIT NONE
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      VDIST
C              DOUBLE PRECISION      VNORM
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         CAMERA
C              PARAMETER           ( CAMERA = 'MGS_MOC_NA' )
C
C              CHARACTER*(*)         FMT0
C              PARAMETER           ( FMT0   = '(1X,A,3F20.8)' )
C
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1   = '(1X,A, F20.8)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'spkcvt.tm' )
C
C              CHARACTER*(*)         TIMFMT
C              PARAMETER           ( TIMFMT =
C             .                    'YYYY MON DD HR:MN:SC.###### UTC' )
C
C              CHARACTER*(*)         TIMFM2
C              PARAMETER           ( TIMFM2 =
C             .              'YYYY MON DD HR:MN:SC.###### TDB ::TDB' )
C
C
C              INTEGER               BDNMLN
C              PARAMETER           ( BDNMLN = 36 )
C
C              INTEGER               CORLEN
C              PARAMETER           ( CORLEN = 10 )
C
C              INTEGER               LOCLEN
C              PARAMETER           ( LOCLEN = 25 )
C
C              INTEGER               FRNMLN
C              PARAMETER           ( FRNMLN = 32 )
C
C              INTEGER               MAXBND
C              PARAMETER           ( MAXBND = 10 )
C
C              INTEGER               SHPLEN
C              PARAMETER           ( SHPLEN = 80 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 40 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(FRNMLN)    CAMREF
C              CHARACTER*(TIMLEN)    EMITIM
C              CHARACTER*(LOCLEN)    REFLOC
C              CHARACTER*(BDNMLN)    OBSRVR
C              CHARACTER*(TIMLEN)    OBSTIM
C              CHARACTER*(FRNMLN)    OUTREF
C              CHARACTER*(SHPLEN)    SHAPE
C              CHARACTER*(BDNMLN)    TARGET
C              CHARACTER*(BDNMLN)    TRGCTR
C              CHARACTER*(FRNMLN)    TRGREF
C              CHARACTER*(TIMLEN)    TRGTIM
C
C              DOUBLE PRECISION      BOUNDS ( 3, MAXBND )
C              DOUBLE PRECISION      BSIGHT ( 3 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT0
C              DOUBLE PRECISION      LT1
C              DOUBLE PRECISION      LT2
C              DOUBLE PRECISION      LT3
C              DOUBLE PRECISION      SPOINT ( 3 )
C              DOUBLE PRECISION      SRFVEC ( 3 )
C              DOUBLE PRECISION      STATE0 ( 6 )
C              DOUBLE PRECISION      STATE1 ( 6 )
C              DOUBLE PRECISION      STATE2 ( 6 )
C              DOUBLE PRECISION      STATE3 ( 6 )
C              DOUBLE PRECISION      TRGEP2
C              DOUBLE PRECISION      TRGEPC
C              DOUBLE PRECISION      TRGST2 ( 6 )
C              DOUBLE PRECISION      TRGSTA ( 6 )
C
C              INTEGER               CAMID
C              INTEGER               I
C              INTEGER               N
C
C              LOGICAL               FOUND
C
C        C
C        C     Load SPICE kernels.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Convert the observation time to seconds past J2000 TDB.
C        C
C              OBSTIM = '2003 OCT 13 06:00:00.000000 UTC'
C
C              CALL STR2ET ( OBSTIM, ET )
C
C        C
C        C     Set the observer, target center, target frame, and
C        C     target state relative to its center.
C        C
C              OBSRVR = 'MGS'
C              TRGCTR = 'EARTH'
C              TRGREF = 'ITRF93'
C
C        C
C        C     Set the state of DSS-14 relative to the earth's
C        C     center at the J2000 epoch, expressed in the
C        C     ITRF93 reference frame. Values come from the
C        C     earth station SPK specified in the meta-kernel.
C        C
C        C     The velocity is non-zero due to tectonic
C        C     plate motion.
C        C
C              TRGEPC    =  0.D0
C
C              TRGSTA(1) =  -2353.6213656676991D0
C              TRGSTA(2) =  -4641.3414911499403D0
C              TRGSTA(3) =   3677.0523293197439D0
C              TRGSTA(4) =     -0.00000000000057086D0
C              TRGSTA(5) =      0.00000000000020549D0
C              TRGSTA(6) =     -0.00000000000012171D0
C
C        C
C        C     Find the apparent state of the station relative
C        C     to the spacecraft in the ITRF93 reference frame.
C        C     Evaluate the earth's orientation, that is the
C        C     orientation of the ITRF93 frame relative to the
C        C     J2000 frame, at the epoch obtained by correcting
C        C     the observation time for one-way light time. This
C        C     correction is obtained by setting REFLOC to 'TARGET'.
C        C
C              OUTREF = 'ITRF93'
C              ABCORR = 'CN+S'
C
C              REFLOC = 'TARGET'
C
C        C
C        C     Compute the observer-target state.
C        C
C              CALL SPKCVT ( TRGSTA, TRGEPC, TRGCTR, TRGREF,
C             .              ET,     OUTREF, REFLOC, ABCORR,
C             .              OBSRVR, STATE0, LT0            )
C
C        C
C        C     Display the computed state and light time.
C        C
C              CALL TIMOUT ( ET-LT0, TIMFMT, EMITIM )
C              CALL TIMOUT ( TRGEPC, TIMFM2, TRGTIM )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Frame evaluation locus:   ', REFLOC
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer:                 ', OBSRVR
C              WRITE (*,*) 'Observation time:         ', OBSTIM
C              WRITE (*,*) 'Target center:            ', TRGCTR
C              WRITE (*,*) 'Target-center state time: ', TRGTIM
C              WRITE (*,*) 'Target frame:             ', TRGREF
C              WRITE (*,*) 'Emission time:            ', EMITIM
C              WRITE (*,*) 'Output reference frame:   ', OUTREF
C              WRITE (*,*) 'Aberration correction:    ', ABCORR
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer-target position (km):   '
C              WRITE (*,FMT0) '   ', ( STATE0(I), I = 1, 3 )
C              WRITE (*,*) 'Observer-target velocity (km/s): '
C              WRITE (*,FMT0) '   ', ( STATE0(I), I = 4, 6 )
C              WRITE (*,FMT1) 'Light time (s):   ', LT0
C              WRITE (*,*) ' '
C
C        C
C        C     Repeat the computation, this time evaluating the
C        C     earth's orientation at the epoch obtained by
C        C     subtracting from the observation time the one way
C        C     light time from the earth's center.
C        C
C        C     This is equivalent to looking up the observer-target
C        C     state using SPKEZR.
C        C
C              REFLOC = 'CENTER'
C
C              CALL SPKCVT ( TRGSTA, TRGEPC, TRGCTR, TRGREF,
C             .              ET,     OUTREF, REFLOC, ABCORR,
C             .              OBSRVR, STATE1, LT1            )
C
C        C
C        C     Display the computed state and light time.
C        C
C              CALL TIMOUT ( ET-LT1, TIMFMT, EMITIM )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Frame evaluation locus:   ', REFLOC
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer:                 ', OBSRVR
C              WRITE (*,*) 'Observation time:         ', OBSTIM
C              WRITE (*,*) 'Target center:            ', TRGCTR
C              WRITE (*,*) 'Target-center state time: ', TRGTIM
C              WRITE (*,*) 'Target frame:             ', TRGREF
C              WRITE (*,*) 'Emission time:            ', EMITIM
C              WRITE (*,*) 'Output reference frame:   ', OUTREF
C              WRITE (*,*) 'Aberration correction:    ', ABCORR
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer-target position (km):   '
C              WRITE (*,FMT0) '   ', ( STATE1(I), I = 1, 3 )
C              WRITE (*,*) 'Observer-target velocity (km/s): '
C              WRITE (*,FMT0) '   ', ( STATE1(I), I = 4, 6 )
C              WRITE (*,FMT1) 'Light time (s):   ', LT1
C              WRITE (*,*) ' '
C
C              WRITE (*,FMT1) 'Distance between above positions '
C             .//             '(km):     ',   VDIST( STATE0, STATE1 )
C              WRITE (*,FMT1) 'Velocity difference magnitude '
C             .//             ' (km/s):     ',
C             .               VDIST( STATE0(4), STATE1(4) )
C
C        C
C        C     Check: compare the state computed directly above
C        C     to one produced by SPKEZR.
C        C
C              TARGET = 'DSS-14'
C
C              CALL SPKEZR ( TARGET, ET,     OUTREF, ABCORR,
C             .              OBSRVR, STATE2, LT2            )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) ' '
C              WRITE (*,*) 'State computed using SPKEZR: '
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer:               ', OBSRVR
C              WRITE (*,*) 'Observation time:       ', OBSTIM
C              WRITE (*,*) 'Target:                 ', TARGET
C              WRITE (*,*) 'Output reference frame: ', OUTREF
C              WRITE (*,*) 'Aberration correction:  ', ABCORR
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer-target position (km):   '
C              WRITE (*,FMT0) '   ', ( STATE2(I), I = 1, 3 )
C              WRITE (*,*) 'Observer-target velocity (km/s): '
C              WRITE (*,FMT0) '   ', ( STATE2(I), I = 4, 6 )
C              WRITE (*,FMT1) 'Light time (s): ', LT2
C              WRITE (*,*) ' '
C
C              WRITE (*,FMT1) 'Distance between last two '
C             .//             'positions (km):  ',
C             .               VDIST ( STATE1, STATE2 )
C              WRITE (*,FMT1) 'Velocity difference magnitude '
C             .//             '    (km/s):  ',
C             .               VDIST( STATE1(4), STATE2(4) )
C
C        C
C        C     Finally, compute an observer-target state in
C        C     a frame centered at the observer.
C        C     The reference frame will be that of the
C        C     MGS MOC NA camera.
C        C
C        C     In this case we'll use as the target the surface
C        C     intercept on Mars of the camera boresight. This
C        C     allows us to easily verify the correctness of
C        C     the results returned by SPKCVT.
C        C
C        C     Get camera frame and FOV parameters. We'll need
C        C     the camera ID code first.
C        C
C              CALL BODN2C ( CAMERA, CAMID, FOUND )
C
C              IF ( .NOT. FOUND ) THEN
C
C                 WRITE (*,*) 'Camera name could not be mapped '
C             .   //          'to an ID code.'
C                 STOP
C
C              END IF
C
C        C
C        C     GETFOV will return the name of the camera-fixed frame
C        C     in the string CAMREF, the camera boresight vector in
C        C     the array BSIGHT, and the FOV corner vectors in the
C        C     array BOUNDS. All we're going to use are the camera
C        C     frame name and camera boresight.
C        C
C              CALL GETFOV ( CAMID,  MAXBND, SHAPE,  CAMREF,
C             .              BSIGHT, N,      BOUNDS         )
C
C        C
C        C     Find the camera boresight surface intercept.
C        C
C              TRGCTR = 'MARS'
C              TRGREF = 'IAU_MARS'
C
C              CALL SINCPT ( 'ELLIPSOID', TRGCTR, ET,     TRGREF,
C             .              ABCORR,      OBSRVR, CAMREF, BSIGHT,
C             .              SPOINT,      TRGEP2, SRFVEC, FOUND  )
C
C        C
C        C     Set the position component of the state vector
C        C     TRGST2 to SPOINT.
C        C
C              CALL VEQU ( SPOINT, TRGST2 )
C
C        C
C        C     Set the velocity of the target state to zero.
C        C     Since the velocity is zero, we can pick any value
C        C     as the target epoch; we choose 0 seconds past
C        C     J2000 TDB.
C        C
C              CALL CLEARD ( 3, TRGST2(4) )
C
C              TRGEPC = 0.D0
C              OUTREF = CAMREF
C
C              REFLOC = 'OBSERVER'
C
C              CALL SPKCVT ( TRGST2, TRGEPC, TRGCTR, TRGREF,
C             .              ET,     OUTREF, REFLOC, ABCORR,
C             .              OBSRVR, STATE3, LT3             )
C
C        C
C        C     Convert the emission time and the target state
C        C     evaluation epoch to strings for output.
C        C
C              CALL TIMOUT ( ET - LT3, TIMFMT, EMITIM )
C              CALL TIMOUT ( TRGEPC,   TIMFM2, TRGTIM )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Frame evaluation locus:   ', REFLOC
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer:                 ', OBSRVR
C              WRITE (*,*) 'Observation time:         ', OBSTIM
C              WRITE (*,*) 'Target center:            ', TRGCTR
C              WRITE (*,*) 'Target-center state time: ', TRGTIM
C              WRITE (*,*) 'Target frame:             ', TRGREF
C              WRITE (*,*) 'Emission time:            ', EMITIM
C              WRITE (*,*) 'Output reference frame:   ', OUTREF
C              WRITE (*,*) 'Aberration correction:    ', ABCORR
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer-target position (km):   '
C              WRITE (*,FMT0) '   ', ( STATE3(I), I = 1, 3 )
C              WRITE (*,*) 'Observer-target velocity (km/s): '
C              WRITE (*,FMT0) '   ', ( STATE3(I), I = 4, 6 )
C              WRITE (*,FMT1) 'Light time (s): ', LT3
C              WRITE (*,FMT1) 'Target range from SINCPT (km): '
C             .//             '            ',   VNORM( SRFVEC )
C              WRITE (*,*) ' '
C              END
C
C
C        When this program was executed on a PC/Linux/gfortran
C        platform, the output was:
C
C
C        Frame evaluation locus:   TARGET
C
C        Observer:                 MGS
C        Observation time:         2003 OCT 13 06:00:00.000000 UTC
C        Target center:            EARTH
C        Target-center state time: 2000 JAN 01 12:00:00.000000 TDB
C        Target frame:             ITRF93
C        Emission time:            2003 OCT 13 05:55:44.232914 UTC
C        Output reference frame:   ITRF93
C        Aberration correction:    CN+S
C
C        Observer-target position (km):
C              52746468.84236781   52367725.79656220   18836142.68955782
C        Observer-target velocity (km/s):
C                  3823.39593314      -3840.60002121          2.21337692
C        Light time (s):           255.76708533
C
C
C        Frame evaluation locus:   CENTER
C
C        Observer:                 MGS
C        Observation time:         2003 OCT 13 06:00:00.000000 UTC
C        Target center:            EARTH
C        Target-center state time: 2000 JAN 01 12:00:00.000000 TDB
C        Target frame:             ITRF93
C        Emission time:            2003 OCT 13 05:55:44.232914 UTC
C        Output reference frame:   ITRF93
C        Aberration correction:    CN+S
C
C        Observer-target position (km):
C              52746419.34641990   52367775.65039122   18836142.68968301
C        Observer-target velocity (km/s):
C                  3823.40103499      -3840.59789000          2.21337692
C        Light time (s):           255.76708533
C
C        Distance between above positions (km):              70.25135676
C        Velocity difference magnitude  (km/s):               0.00552910
C
C
C        State computed using SPKEZR:
C
C        Observer:               MGS
C        Observation time:       2003 OCT 13 06:00:00.000000 UTC
C        Target:                 DSS-14
C        Output reference frame: ITRF93
C        Aberration correction:  CN+S
C
C        Observer-target position (km):
C              52746419.34641990   52367775.65039122   18836142.68968301
C        Observer-target velocity (km/s):
C                  3823.40103499      -3840.59789000          2.21337692
C        Light time (s):         255.76708533
C
C        Distance between last two positions (km):            0.00000000
C        Velocity difference magnitude     (km/s):            0.00000000
C
C
C        Frame evaluation locus:   OBSERVER
C
C        Observer:                 MGS
C        Observation time:         2003 OCT 13 06:00:00.000000 UTC
C        Target center:            MARS
C        Target-center state time: 2000 JAN 01 12:00:00.000000 TDB
C        Target frame:             IAU_MARS
C        Emission time:            2003 OCT 13 05:59:59.998702 UTC
C        Output reference frame:   MGS_MOC_NA
C        Aberration correction:    CN+S
C
C        Observer-target position (km):
C                     0.00000001         -0.00000001        388.97573572
C        Observer-target velocity (km/s):
C                     2.91968665          0.15140014          0.92363513
C        Light time (s):           0.00129748
C        Target range from SINCPT (km):                     388.97573572
C
C
C$ Restrictions
C
C     1)  This routine may not be suitable for work with stars or other
C         objects having large distances from the observer, due to loss
C         of precision in position vectors.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     S.C. Krening    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-MAR-2014 (NJB) (SCK) (BVS)
C
C-&
 
C$ Index_Entries
C
C     state of constant_velocity_target 
C     state of surface_point on extended_object
C     state of landmark on extended_object
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               ESRCHC
      
      LOGICAL               FAILED
      LOGICAL               RETURN

      EXTERNAL              ZZCVXSTA

C
C     Local parameters
C    
      INTEGER               NEVFLG
      PARAMETER           ( NEVFLG = 3 )

      INTEGER               EVLFLN
      PARAMETER           ( EVLFLN = 25 )

C
C     Codes for evaluation type flags. These codes are
C     indices of the flags within the EVLFLG array.
C
      INTEGER               EVLOBS
      PARAMETER           ( EVLOBS = 1 )

      INTEGER               EVLTRG
      PARAMETER           ( EVLTRG = 2 )

      INTEGER               EVLCTR
      PARAMETER           ( EVLCTR = 3 )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL   = 36 )

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )



C
C     Local variables
C
      CHARACTER*(EVLFLN)    EVLFLG ( NEVFLG )
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      J2STAT ( 6 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      TMPXFM ( 6, 6 )
      DOUBLE PRECISION      XTRANS ( 6, 6 )

      INTEGER               CTRCDE
      INTEGER               EVLTYP
      INTEGER               J2CODE
      INTEGER               OBSCDE
      INTEGER               ORFCDE

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      LOGICAL               FIRST
      LOGICAL               USELT
      LOGICAL               XMIT

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTCTR
      INTEGER               SVCCDE
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSR
      INTEGER               SVOBSC
      LOGICAL               SVFND2

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVOREF
      INTEGER               SVORFC


C
C     Saved variables
C
      SAVE                  EVLFLG
      SAVE                  J2CODE
      SAVE                  FIRST
      SAVE                  PRVCOR
      SAVE                  USELT
      SAVE                  XMIT

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVTCTR
      SAVE                  SVCCDE
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVOBSR
      SAVE                  SVOBSC
      SAVE                  SVFND2

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR3
      SAVE                  SVOREF
      SAVE                  SVORFC


C
C     Initial values
C     
C
C     Evaluation type flags. The order of these flags
C     must match that of the INTEGER parameters
C     corresponding to these flags.
C
      DATA                  EVLFLG / 'OBSERVER',
     .                               'TARGET',
     .                               'CENTER'    /

      DATA                  FIRST  / .TRUE.      /

      DATA                  PRVCOR / ' '         /


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKCVT' )

C
C     Counter initialization is done separately.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )
         CALL ZZCTRUIN( SVCTR3 )

      END IF

      IF (  FIRST .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
C
C        On the first pass, save the ID code of the J2000 frame.
C
         IF ( FIRST ) THEN
            CALL IRFNUM ( 'J2000', J2CODE )
         END IF

C
C        Analyze the aberration correction string; produce an attribute
C        block.
C
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKCVT')
            RETURN
         END IF

         USELT  = ATTBLK( LTIDX  )
         XMIT   = ATTBLK( XMTIDX )
         PRVCOR = ABCORR

         FIRST  = .FALSE.

      END IF
 
C
C     Convert the input name of the center of motion to 
C     an ID code.
C
      CALL ZZBODS2C ( SVCTR1, SVTCTR, SVCCDE, SVFND1,
     .                TRGCTR, CTRCDE, FOUND    )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Could not map body name # to an ID code.' )
         CALL ERRCH  ( '#',  TRGCTR                               )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SPKCVT'                                   )
         RETURN

      END IF

C
C     Convert the input name of the observer to an ID code.
C
      CALL ZZBODS2C ( SVCTR2, SVOBSR, SVOBSC, SVFND2,
     .                OBSRVR, OBSCDE, FOUND    )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Could not map body name # to an ID code.' )
         CALL ERRCH  ( '#',  OBSRVR                               )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SPKCVT'                                   )
         RETURN

      END IF

C
C     Look up the output frame's ID code.
C
      CALL ZZNAMFRM ( SVCTR3, SVOREF, SVORFC, OUTREF, ORFCDE )

      IF ( ORFCDE .EQ. 0 ) THEN
C
C        Only non-zero ID codes are legitimate.  Zero
C        indicates that the frame wasn't recognized.
C 
         CALL SETMSG ( 'The frame # was not recognized. Possible '
     .   //            'causes are that the frame name was misspelled '
     .   //            'or that a required frame kernel has not been '
     .   //            'loaded.'                                       )
         CALL ERRCH  ( '#', OUTREF                                     )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                           )
         CALL CHKOUT ( 'SPKCVT'                                        )
         RETURN
         
      END IF
      
C
C     Identify the frame evaluation locus.
C
      EVLTYP = ESRCHC ( REFLOC, NEVFLG, EVLFLG )

      IF ( EVLTYP .EQ. 0 ) THEN
C
C        REFLOC is not a valid evaluation choice.
C
         CALL SETMSG ( 'Output frame evaluation locus '
     .   //            '# was not recognized. Allowed values are '
     .   //            '''OBSERVER'', ''TARGET'', and ''CENTER''.' )
         CALL ERRCH  ( '#',  REFLOC                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                       )
         CALL CHKOUT ( 'SPKCVT'                                    )
         RETURN

      END IF

C
C     Store inputs required to evaluate the target's state relative to
C     its center of motion. 
C
      CALL ZZCVSSTA ( TRGSTA, CTRCDE, TRGEPC, TRGREF )

C
C     Below, the routine that uses the inputs stored by ZZCVSSTA
C     is ZZCVXSTA. ZZCVXSTA computes the geometric state of the 
C     target relative to its center of motion in the frame TRGREF.
C     
C
C     Get the state of the target relative to the observer at ET,
C     expressed in the frame OUTREF, using the specified aberration
C     corrections.

      IF ( .NOT. USELT )  THEN
C
C        We evaluate the target frame at ET, regardless of 
C        the setting of EVLTYP.
C         
         CALL ZZSPKFZT ( ZZCVXSTA, ET,     OUTREF,
     .                   ABCORR,   OBSCDE, STATE,  LT )

         CALL CHKOUT ( 'SPKCVT' )
         RETURN

      END IF

C
C     Getting to this point implies light time corrections are used.
C     The action to take depends on the frame evaluation locus.
C
      IF ( EVLTYP .EQ. EVLCTR ) THEN
C
C        We're going to use the traditional SPK method of specifying
C        the output frame evaluation epoch; this is what ZZSPKFZT
C        does.
C
         CALL ZZSPKFZT ( ZZCVXSTA, ET,     OUTREF,
     .                   ABCORR,   OBSCDE, STATE,  LT )


      ELSE IF ( EVLTYP .EQ. EVLOBS ) THEN
C
C        The output frame orientation is to be evaluated at the
C        observation epoch ET.        
C
C        Since the output frame is not necessarily centered at the
C        observer, we will first look up the state in an inertial
C        frame, then transform the state as necessary.
C
         CALL ZZSPKFZT ( ZZCVXSTA, ET,     'J2000', 
     .                   ABCORR,   OBSCDE, J2STAT,  LT )
                  
         IF ( ORFCDE .EQ. J2CODE ) THEN

            CALL MOVED ( J2STAT, 6, STATE )

         ELSE

            CALL FRMCHG ( J2CODE, ORFCDE, ET, XTRANS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SPKCVT')
               RETURN
            END IF

            CALL MXVG ( XTRANS,  J2STAT,  6,  6, STATE )

         END IF


      ELSE IF ( EVLTYP .EQ. EVLTRG ) THEN
C
C        We must evaluate the output frame at the epoch associated
C        with the target. This may well not be the epoch associated
C        with the output frame's center, so we will first look up the
C        state in an inertial frame, then transform the state as
C        necessary.
C
C        This process isn't parallel to that for the observer epoch,
C        because in this case we have to account for the rate of
C        change of light time. Start out by determining the sign
C        of the light time correction.
C
         IF ( XMIT ) THEN
C
C           We're doing transmission corrections.
C
            S =  1.D0
             
         ELSE
C
C           We're doing reception corrections.
C
            S = -1.D0

         END IF

C
C        Get the observer-target state in the J2000 frame, along
C        with the light time and light time rate.
C
         CALL ZZSPKFAT ( ZZCVXSTA, ET,     'J2000', 
     .                   ABCORR,   OBSCDE, J2STAT,  LT, DLT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKCVT')
            RETURN
         END IF


         IF (  ORFCDE .EQ. J2CODE ) THEN
C
C           The output frame is J2000. No frame transformation is
C           required.
C
            CALL MOVED ( J2STAT, 6, STATE )

         ELSE
C
C           Look up the state transformation from the J2000 frame to
C           OUTREF at the light time corrected epoch.
C
            CALL FRMCHG ( J2CODE,  ORFCDE,  ET + S*LT,  XTRANS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SPKCVT')
               RETURN
            END IF

C
C           Adjust the transformation to account for the rate of change
C           of light time.
C
            CALL ZZCORSXF ( XMIT, DLT, XTRANS, TMPXFM )

C
C           Map the output state to the frame OUTREF.
C        
            CALL MXVG ( TMPXFM, J2STAT, 6, 6, STATE )

         END IF

      ELSE
C
C        We've run out of valid evaluation choices. We were
C        supposed to have caught this problem earlier, so this
C        is the result of a coding error.
C
         CALL SETMSG ( 'Output frame evaluation locus # was '
     .   //            'not recognized. [Coding error].'      )
         CALL ERRCH  ( '#',  REFLOC                           )
         CALL SIGERR ( 'SPICE(BUG)'                           )
         CALL CHKOUT ( 'SPKCVT'                               )
         RETURN

      END IF

      CALL CHKOUT ( 'SPKCVT' )
      RETURN
      END


