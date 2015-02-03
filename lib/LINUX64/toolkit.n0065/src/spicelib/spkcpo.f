C$Procedure SPKCPO ( SPK, constant position observer state )
 
      SUBROUTINE SPKCPO ( TARGET, ET,     OUTREF, REFLOC, ABCORR,
     .                    OBSPOS, OBSCTR, OBSREF, STATE,  LT     )

C$ Abstract
C
C     Return the state of a specified target relative to an "observer,"
C     where the observer has constant position in a specified reference
C     frame. The observer's position is provided by the calling program
C     rather than by loaded SPK files.
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

      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         OUTREF
      CHARACTER*(*)         REFLOC
      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      OBSPOS ( 3 )
      CHARACTER*(*)         OBSCTR
      CHARACTER*(*)         OBSREF
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      LT


C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   Name of target ephemeris object.
C     ET         I   Observation epoch.
C     OUTREF     I   Reference frame of output state.
C     REFLOC     I   Output reference frame evaluation locus.
C     ABCORR     I   Aberration correction.
C     OBSPOS     I   Observer position relative to center of motion.
C     OBSCTR     I   Center of motion of observer.
C     OBSREF     I   Frame of observer position.
C     STATE      O   State of target with respect to observer.
C     LT         O   One way light time between target and
C                    observer.
C
C$ Detailed_Input
C
C     TARGET      is the name of a target body. Optionally, you may
C                 supply the ID code of the object as an integer
C                 string. For example, both 'EARTH' and '399' are
C                 legitimate strings to supply to indicate the target
C                 is Earth.
C
C                 Case and leading and trailing blanks are not
C                 significant in the string TARGET.
C
C
C     ET          is the ephemeris time at which the state of the
C                 target relative to the observer is to be computed. ET
C                 is expressed as seconds past J2000 TDB. ET refers to
C                 time at the observer's location.
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
C                                be selected when OUTREF is centered
C                                at the target object.
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
C                 the observer-target state to account for one-way
C                 light time and stellar aberration.
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
C     OBSPOS      is the fixed (constant) geometric position of an
C                 observer relative to its center of motion OBSCTR,
C                 expressed in the reference frame OBSREF.
C
C                 Units are always km.
C
C
C     OBSCTR      is the name of the center of motion of OBSPOS. The
C                 ephemeris of OBSCTR is provided by loaded SPK files.
C
C                 Optionally, you may supply the integer ID code for
C                 the object as an integer string. For example both
C                 'MOON' and '301' are legitimate strings that indicate
C                 the moon is the center of motion.
C
C                 Case and leading and trailing blanks are not
C                 significant in the string OBSCTR.
C
C
C     OBSREF      is the name of the reference frame relative to which
C                 the input position OBSPOS is expressed. The observer
C                 has constant position relative to its center of
C                 motion in this reference frame.
C
C                 Case and leading and trailing blanks are not
C                 significant in the string OBSREF.
C
C                                 
C$ Detailed_Output
C
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
C     1)  If either the name of the center of motion or the target
C         cannot be translated to its NAIF ID code, the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     2)  If the reference frame OUTREF is unrecognized, the error will
C         be diagnosed by a routine in the call tree of this routine.
C
C     3)  If the reference frame OBSREF is unrecognized, the error will
C         be diagnosed by a routine in the call tree of this routine.
C
C     4)  If the frame evaluation locus REFLOC is not recognized,
C         the error  will be diagnosed by a routine in the call tree of
C         this routine.
C
C     5)  If the loaded kernels provide insufficient data to compute
C         the requested state vector, the deficiency will be diagnosed
C         by a routine in the call tree of this routine.
C
C     6)  If an error occurs while reading an SPK or other kernel file,
C         the error  will be diagnosed by a routine in the call tree of
C         this routine.
C
C     7)  If the aberration correction ABCORR is not recognized, the
C         error will be diagnosed by a routine in the call tree of this
C         routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        -  SPK data: ephemeris data for the observer center and target
C           must be loaded. If aberration corrections are used, the
C           states of the observer center and target relative to the
C           solar system barycenter must be calculable from the
C           available ephemeris data. Typically ephemeris data are made
C           available by loading one or more SPK files using FURNSH.
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
C     This routine computes observer-target states for observers whose
C     trajectories are not provided by SPK files.
C
C     Observers supported by this routine must have constant position
C     with respect to a specified center of motion, expressed in a
C     caller-specified reference frame. The state of the center of
C     motion relative to the target must be computable using 
C     loaded SPK data.
C
C     For applications in which the observer has constant, non-zero
C     velocity relative to its center of motion, the SPICELIB routine
C
C        SPKCVO     { SPK, constant velocity observer state }
C
C     can be used. 
C     
C     This routine is suitable for computing states of target ephemeris
C     objects, as seen from landmarks on the surface of an extended
C     object, in cases where no SPK data are available for those
C     landmarks.
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
C        the target itself. A similar problem may occur when the 
C        observer is a surface point on an extended body and the
C        output frame is centered at the body center: the listed 
C        routines will correct the orientation of the output frame for
C        one-way light time between the frame center and the observer.
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
C
C     1) Compute apparent solar azimuth and elevation as seen from a
C        specified surface point on the earth.
C
C        Task Description
C        ================
C
C        In this example we'll use the location of the DSN station 
C        DSS-14 as our surface point.
C
C        We'll perform the solar azimuth and elevation computation two
C        ways:
C
C           - Using a station frame kernel to provide the 
C             specification of a topocentric reference frame
C             centered at DSS-14.
C
C           - Computing inline the transformation from the earth-fixed,
C             earth-centered frame ITRF93 to a topocentric frame
C             centered at DSS-14.
C             
C        Note that results of the two computations will differ
C        slightly. There are three sources of the differences:
C
C           1) The station position is time-dependent due to tectonic
C              plate motion, and epochs of the station positions used
C              to specify the axes of the topocentric frame are
C              different in the two cases. This gives rise to different
C              orientations of the frame's axes relative to the frame
C              ITRF93.
C
C           2) The two computations use different earth radii; this
C              results in computation of different geodetic latitudes 
C              of the station. This difference also affects the 
C              topocentric frame orientation relative to ITRF93.
C
C           3) The station movement between ET and the epoch at which
C              the DSS-14_TOPO frame is specified contributes a very
C              small offset---on the order of 10 cm---to the station-sun
C              position vector, expressed in the ITRF93 frame.
C           
C
C        Kernels
C        =======     
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C 
C
C        KPL/MK
C
C           File name: spkcpo.tm
C
C           This is the meta-kernel file for the header code example for
C           the subroutine SPKCPO. These kernel files can be found on
C           the NAIF website.
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
C              earth_topo_050714.tf             DSN station FK
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
C                               'earth_topo_050714.tf',
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
C        C     This program uses SPKCPO to compute solar azimuth
C        C     and elevation at a given surface point on the earth.
C        C
C
C              PROGRAM EX1
C              IMPLICIT NONE
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      VDIST
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         FMT0
C              PARAMETER           ( FMT0   = '(1X,A,3F20.8)' )
C
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1   = '(1X,A, F20.8)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'spkcvo.tm' )
C
C              CHARACTER*(*)         TIMFMT
C              PARAMETER           ( TIMFMT =
C             .                    'YYYY MON DD HR:MN:SC.###### UTC' )
C
C              CHARACTER*(*)         TIMFM2
C              PARAMETER           ( TIMFM2 =
C             .              'YYYY MON DD HR:MN:SC.###### TDB ::TDB' )
C
C              INTEGER               BDNMLN
C              PARAMETER           ( BDNMLN = 36 )
C
C              INTEGER               CORLEN
C              PARAMETER           ( CORLEN = 10 )
C
C              INTEGER               EVLLEN
C              PARAMETER           ( EVLLEN = 25 )
C
C              INTEGER               FRNMLN
C              PARAMETER           ( FRNMLN = 32 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 40 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(TIMLEN)    EMITIM
C              CHARACTER*(TIMLEN)    EPCSTR
C              CHARACTER*(EVLLEN)    REFLOC
C              CHARACTER*(BDNMLN)    OBSCTR
C              CHARACTER*(FRNMLN)    OBSREF
C              CHARACTER*(TIMLEN)    OBSTIM
C              CHARACTER*(FRNMLN)    OUTREF
C              CHARACTER*(BDNMLN)    TARGET
C
C              DOUBLE PRECISION      AZ
C              DOUBLE PRECISION      EL
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      F
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      LT0
C              DOUBLE PRECISION      LT1
C              DOUBLE PRECISION      NORMAL ( 3 )
C              DOUBLE PRECISION      OBSALT
C              DOUBLE PRECISION      OBSEPC
C              DOUBLE PRECISION      OBSLAT
C              DOUBLE PRECISION      OBSLON
C              DOUBLE PRECISION      OBSPOS ( 3 )
C              DOUBLE PRECISION      R
C              DOUBLE PRECISION      RADII  ( 3 )
C              DOUBLE PRECISION      RE
C              DOUBLE PRECISION      RP
C              DOUBLE PRECISION      STATE0 ( 6 )
C              DOUBLE PRECISION      STATE1 ( 6 )
C              DOUBLE PRECISION      TOPVEC ( 3 )
C              DOUBLE PRECISION      XFORM  ( 3, 3 )
C              DOUBLE PRECISION      Z      ( 3 )
C
C              INTEGER               I
C              INTEGER               N
C
C        C
C        C     Initial values
C        C
C              DATA                  Z / 0.D0, 0.D0, 1.D0 /
C
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
C        C     Set the target, observer center, observer frame, and
C        C     observer position relative to its center.
C        C
C              TARGET = 'SUN'
C              OBSCTR = 'EARTH'
C              OBSREF = 'ITRF93'
C
C        C
C        C     Set the position of DSS-14 relative to the earth's
C        C     center at the J2000 epoch, expressed in the
C        C     ITRF93 reference frame. Values come from the
C        C     earth station SPK specified in the meta-kernel.
C        C
C        C     The actual station velocity is non-zero due
C        C     to tectonic plate motion; we ignore the motion
C        C     in this example. See the routine SPKCVO for an
C        C     example in which the plate motion is accounted for.
C        C
C              OBSPOS(1) =  -2353.6213656676991D0
C              OBSPOS(2) =  -4641.3414911499403D0
C              OBSPOS(3) =   3677.0523293197439D0
C
C        C
C        C     Find the apparent state of the sun relative
C        C     to the station in the DSS-14_TOPO reference frame.
C        C     Evaluate the output frame's orientation, that is the
C        C     orientation of the DSS-14_TOPO frame relative to the
C        C     J2000 frame, at the observation epoch. This
C        C     correction is obtained by setting REFLOC to
C        C     'OBSERVER'.
C        C
C              OUTREF = 'DSS-14_TOPO'
C              ABCORR = 'CN+S'
C
C              REFLOC = 'OBSERVER'
C
C        C
C        C     Compute the observer-target state.
C        C
C              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC,
C             .              ABCORR, OBSPOS, OBSCTR,
C             .              OBSREF, STATE0, LT0            )
C
C        C
C        C     Compute planetocentric coordinates of the
C        C     observer-target position in the local
C        C     topocentric reference frame DSS-14_TOPO.
C        C
C              CALL RECLAT ( STATE0, R, LON, LAT )
C
C        C
C        C     Compute solar azimuth. The latitude we've
C        C     already computed is the elevation. Express
C        C     both angles in degrees.
C        C
C              EL =   LAT * DPR()
C              AZ = - LON * DPR()
C
C              IF ( AZ .LT. 0.D0 ) THEN
C                 AZ = AZ + 360.D0
C              END IF
C
C        C
C        C     Display the computed state, light time, and
C        C     angles.
C        C
C              CALL TIMOUT ( ET-LT0, TIMFMT, EMITIM )
C              CALL TIMOUT ( OBSEPC, TIMFM2, EPCSTR )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Frame evaluation locus:     ', REFLOC
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Target:                     ', TARGET
C              WRITE (*,*) 'Observation time:           ', OBSTIM
C              WRITE (*,*) 'Observer center:            ', OBSCTR
C              WRITE (*,*) 'Observer frame:             ', OBSREF
C              WRITE (*,*) 'Emission time:              ', EMITIM
C              WRITE (*,*) 'Output reference frame:     ', OUTREF
C              WRITE (*,*) 'Aberration correction:      ', ABCORR
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer-target position (km):   '
C              WRITE (*,FMT0) '   ', ( STATE0(I), I = 1, 3 )
C              WRITE (*,*) 'Observer-target velocity (km/s): '
C              WRITE (*,FMT0) '   ', ( STATE0(I), I = 4, 6 )
C              WRITE (*,FMT1) 'Light time (s):        ', LT0
C              WRITE (*,*) ' '
C              WRITE (*,FMT1) 'Solar azimuth (deg):   ', AZ
C              WRITE (*,FMT1) 'Solar elevation (deg): ', EL
C              WRITE (*,*) ' '
C
C        C
C        C     For an arbitrary surface point, we might not
C        C     have a frame kernel available. In this case
C        C     we can look up the state in the observer frame
C        C     using SPKCPO and then convert the state to
C        C     the local topocentric frame. We'll first
C        C     create the transformation matrix for converting
C        C     vectors in the observer frame to the topocentric
C        C     frame.
C        C
C        C     First step: find the geodetic (planetodetic)
C        C     coordinates of the observer. We need the
C        C     equatorial radius and flattening coefficient
C        C     of the reference ellipsoid.
C        C
C              CALL BODVRD ( 'EARTH', 'RADII', 3, N, RADII )
C
C              RE = RADII(1)
C              RP = RADII(3)
C
C              F  = ( RE - RP ) / RE
C
C              CALL RECGEO ( OBSPOS, RE, F, OBSLON, OBSLAT, OBSALT )
C
C        C
C        C     Find the outward surface normal on the reference
C        C     ellipsoid at the observer's longitude and latitude.
C        C
C              CALL LATREC ( 1.D0, OBSLON, OBSLAT, NORMAL )
C
C        C
C        C     The topocentric frame has its +Z axis aligned
C        C     with NORMAL and its +X axis pointed north.
C        C     The north direction is aligned with the component
C        C     of the ITRF93 +Z axis orthogonal to the topocentric
C        C     +Z axis.
C        C
C              CALL TWOVEC ( NORMAL, 3, Z, 1, XFORM )
C
C              OUTREF = 'ITRF93'
C              ABCORR = 'CN+S'
C
C              REFLOC = 'OBSERVER'
C
C        C
C        C     Compute the observer-target state.
C        C
C              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC,
C             .              ABCORR, OBSPOS, OBSCTR,
C             .              OBSREF, STATE1, LT1            )
C
C        C
C        C     Convert the position to the topocentric frame.
C        C
C              CALL MXV ( XFORM, STATE1, TOPVEC )
C
C        C
C        C     Compute azimuth and elevation.
C        C
C              CALL RECLAT ( TOPVEC, R, LON, LAT )
C
C              EL =   LAT * DPR()
C              AZ = - LON * DPR()
C
C              IF ( AZ .LT. 0.D0 ) THEN
C                 AZ = AZ + 360.D0
C              END IF
C
C              WRITE (*,*) ' '
C              WRITE (*,*) ' '
C              WRITE (*,*) 'AZ/EL computed without frame kernel: '
C              WRITE (*,*) ' '
C              WRITE (*,FMT1) 'Distance between last two '
C             .//             'positions (km):  ',
C             .               VDIST ( STATE0, TOPVEC )
C              WRITE (*,*) ' '
C              WRITE (*,FMT1) 'Solar azimuth (deg):   ', AZ
C              WRITE (*,FMT1) 'Solar elevation (deg): ', EL
C              WRITE (*,*) ' '
C
C              END
C
C
C        When this program was executed on a PC/Linux/gfortran
C        platform, the output was:
C
C
C        Frame evaluation locus:     OBSERVER
C
C        Target:                     SUN
C        Observation time:           2003 OCT 13 06:00:00.000000 UTC
C        Observer center:            EARTH
C        Observer frame:             ITRF93
C        Emission time:              2003 OCT 13 05:51:42.068322 UTC
C        Output reference frame:     DSS-14_TOPO
C        Aberration correction:      CN+S
C
C        Observer-target position (km):
C              62512272.82074844   58967494.42513601 -122059095.46751881
C        Observer-target velocity (km/s):
C                  2475.97326517      -9870.26706232      -3499.90809969
C        Light time (s):                497.93167797
C
C        Solar azimuth (deg):           316.67141599
C        Solar elevation (deg):         -54.85253168
C
C
C
C        AZ/EL computed without frame kernel:
C
C        Distance between last two positions (km):            3.07056971
C
C        Solar azimuth (deg):           316.67141786
C        Solar elevation (deg):         -54.85253216
C
C
C
C
C     2) Demonstrate applications of the output frame evaluation locus. 
C     
C        The following program is not necessarily realistic: for
C        brevity, it combines several unrelated computations.
C
C        Task Description
C        ================
C
C        Find the state of the Mars Global Surveyor spacecraft, as seen
C        from a given surface point on earth, corrected for light time
C        and stellar aberration, expressed in the earth fixed reference
C        frame ITRF93. The surface point is the position of the DSN
C        station DSS-14.
C
C        Contrast the states computed by setting the output frame
C        evaluation locus to 'OBSERVER' and to 'CENTER'. Show that the
C        latter choice produces results very close to those that
C        can be obtained using SPKEZR. 
C
C        Also compute the central meridian longitude on Mars of DSS-14.
C        This computation performs aberration corrections for the center
C        of Mars.
C
C        Note that in general, the routine SUBPNT should be used for
C        sub-observer point computations when high-accuracy aberration
C        corrections are desired. 
C 
C        The observation epoch is 2003 OCT 13 06:00:00 UTC.
C
C
C        Kernels
C        =======     
C
C        Use the meta-kernel of example 1 above.
C
C
C        Example code begins here.
C
C
C        C     Program: EX2
C        C
C        C     This program demonstrates the use of SPKCPO.
C        C     Computations are performed using all three possible
C        C     values of the output frame evaluation locus REFLOC:
C        C
C        C        'OBSERVER'
C        C        'CENTER'
C        C        'TARGET'
C        C
C        C     Several unrelated computations are performed in
C        C     this program. In particular, computation of the
C        C     central meridian longitude on Mars is included
C        C     simply to demonstrate use of the 'TARGET' option.
C        C
C
C              PROGRAM EX2
C              IMPLICIT NONE
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      DPR
C              DOUBLE PRECISION      VDIST
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         FMT0
C              PARAMETER           ( FMT0   = '(1X,A,3F20.8)' )
C
C              CHARACTER*(*)         FMT1
C              PARAMETER           ( FMT1   = '(1X,A, F20.8)' )
C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'spkcvo.tm' )
C
C              CHARACTER*(*)         TIMFMT
C              PARAMETER           ( TIMFMT =
C             .                    'YYYY MON DD HR:MN:SC.###### UTC' )
C
C              INTEGER               BDNMLN
C              PARAMETER           ( BDNMLN = 36 )
C
C              INTEGER               CORLEN
C              PARAMETER           ( CORLEN = 10 )
C
C              INTEGER               EVLLEN
C              PARAMETER           ( EVLLEN = 25 )
C
C              INTEGER               FRNMLN
C              PARAMETER           ( FRNMLN = 32 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 40 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CORLEN)    ABCORR
C              CHARACTER*(TIMLEN)    EMITIM
C              CHARACTER*(EVLLEN)    REFLOC
C              CHARACTER*(BDNMLN)    OBSRVR
C              CHARACTER*(TIMLEN)    OBSTIM
C              CHARACTER*(FRNMLN)    OUTREF
C              CHARACTER*(BDNMLN)    TARGET
C              CHARACTER*(BDNMLN)    OBSCTR
C              CHARACTER*(FRNMLN)    OBSREF
C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LAT
C              DOUBLE PRECISION      LON
C              DOUBLE PRECISION      LT0
C              DOUBLE PRECISION      LT1
C              DOUBLE PRECISION      LT2
C              DOUBLE PRECISION      LT3
C              DOUBLE PRECISION      R
C              DOUBLE PRECISION      STATE0 ( 6 )
C              DOUBLE PRECISION      STATE1 ( 6 )
C              DOUBLE PRECISION      STATE2 ( 6 )
C              DOUBLE PRECISION      STATE3 ( 6 )
C              DOUBLE PRECISION      OBSPOS ( 3 )
C              DOUBLE PRECISION      OBSVEC ( 3 )
C
C              INTEGER               I
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
C        C     Set the target, observer center, observer frame, and
C        C     observer position relative to its center.
C        C
C              TARGET = 'MGS'
C              OBSCTR = 'EARTH'
C              OBSREF = 'ITRF93'
C
C        C
C        C     Set the position of DSS-14 relative to the earth's
C        C     center at the J2000 epoch, expressed in the
C        C     ITRF93 reference frame. Values come from the
C        C     earth station SPK specified in the meta-kernel.
C        C
C        C     The actual station velocity is non-zero due
C        C     to tectonic plate motion; we ignore the motion
C        C     in this example. See the routine SPKCVT for an
C        C     example in which the plate motion is accounted for.
C        C
C              OBSPOS(1) =  -2353.6213656676991D0
C              OBSPOS(2) =  -4641.3414911499403D0
C              OBSPOS(3) =   3677.0523293197439D0
C
C        C
C        C     Find the apparent state of the spacecraft relative
C        C     to the station in the ITRF93 reference frame.
C        C     Evaluate the earth's orientation, that is the
C        C     orientation of the ITRF93 frame relative to the
C        C     J2000 frame, at the observation epoch. This
C        C     correction is obtained by setting REFLOC to
C        C     'OBSERVER'.
C        C
C              OUTREF = 'ITRF93'
C              ABCORR = 'CN+S'
C
C              REFLOC = 'OBSERVER'
C
C        C
C        C     Compute the observer-target state.
C        C
C              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC,
C             .              ABCORR, OBSPOS, OBSCTR,
C             .              OBSREF, STATE0, LT0            )
C
C        C
C        C     Display the computed state and light time.
C        C
C              CALL TIMOUT ( ET-LT0, TIMFMT, EMITIM )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Frame evaluation locus:     ', REFLOC
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Target:                     ', TARGET
C              WRITE (*,*) 'Observation time:           ', OBSTIM
C              WRITE (*,*) 'Observer center:            ', OBSCTR
C              WRITE (*,*) 'Observer frame:             ', OBSREF
C              WRITE (*,*) 'Emission time:              ', EMITIM
C              WRITE (*,*) 'Output reference frame:     ', OUTREF
C              WRITE (*,*) 'Aberration correction:      ', ABCORR
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
C              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC,
C             .              ABCORR, OBSPOS, OBSCTR,
C             .              OBSREF, STATE1, LT1            )
C
C        C
C        C     Display the computed state and light time.
C        C
C              CALL TIMOUT ( ET-LT1, TIMFMT, EMITIM )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Frame evaluation locus:     ', REFLOC
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Target:                     ', TARGET
C              WRITE (*,*) 'Observation time:           ', OBSTIM
C              WRITE (*,*) 'Observer center:            ', OBSCTR
C              WRITE (*,*) 'Observer frame:             ', OBSREF
C              WRITE (*,*) 'Emission time:              ', EMITIM
C              WRITE (*,*) 'Output reference frame:     ', OUTREF
C              WRITE (*,*) 'Aberration correction:      ', ABCORR
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
C              OBSRVR = 'DSS-14'
C
C              CALL SPKEZR ( TARGET, ET,     OUTREF, ABCORR,
C             .              OBSRVR, STATE2, LT2            )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) ' '
C              WRITE (*,*) 'State computed using SPKEZR: '
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Target:                 ', TARGET
C              WRITE (*,*) 'Observation time:       ', OBSTIM
C              WRITE (*,*) 'Output reference frame: ', OUTREF
C              WRITE (*,*) 'Aberration correction:  ', ABCORR
C              WRITE (*,*) 'Observer:               ', OBSRVR
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
C        C     a frame centered at the target. This state
C        C     can be used to compute the sub-observer longitude.
C        C     The reference frame is the Mars-fixed frame IAU_MARS.
C        C
C              TARGET = 'MARS'
C              OUTREF = 'IAU_MARS'
C
C              REFLOC = 'TARGET'
C
C              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC,
C             .              ABCORR, OBSPOS, OBSCTR,
C             .              OBSREF, STATE3, LT3            )
C
C        C
C        C     Central meridian longitude is the longitude of the
C        C     observer relative to the target center, so we must
C        C     negate the position portion of the state we just
C        C     computed.
C        C
C              CALL VMINUS ( STATE3, OBSVEC )
C
C              CALL RECLAT ( OBSVEC, R, LON, LAT )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Frame evaluation locus:     ', REFLOC
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Target:                     ', TARGET
C              WRITE (*,*) 'Observation time:           ', OBSTIM
C              WRITE (*,*) 'Observer center:            ', OBSCTR
C              WRITE (*,*) 'Observer frame:             ', OBSREF
C              WRITE (*,*) 'Emission time:              ', EMITIM
C              WRITE (*,*) 'Output reference frame:     ', OUTREF
C              WRITE (*,*) 'Aberration correction:      ', ABCORR
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Observer-target position (km):   '
C              WRITE (*,FMT0) '   ', ( STATE3(I), I = 1, 3 )
C              WRITE (*,*) 'Observer-target velocity (km/s): '
C              WRITE (*,FMT0) '   ', ( STATE3(I), I = 4, 6 )
C              WRITE (*,FMT1) 'Light time (s):   ', LT3
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Central meridian '
C              WRITE (*,FMT1) 'longitude (deg):  ', LON*DPR()
C              WRITE (*,*) ' '
C              END
C
C
C        When this program was executed on a PC/Linux/gfortran
C        platform, the output was:
C
C
C        Frame evaluation locus:     OBSERVER
C
C        Target:                     MGS
C        Observation time:           2003 OCT 13 06:00:00.000000 UTC
C        Observer center:            EARTH
C        Observer frame:             ITRF93
C        Emission time:              2003 OCT 13 05:55:44.201144 UTC
C        Output reference frame:     ITRF93
C        Aberration correction:      CN+S
C
C        Observer-target position (km):
C             -53720675.37947631  -51381249.05335969  -18838416.34718024
C        Observer-target velocity (km/s):
C                 -3751.69274754       3911.73417167         -2.17503628
C        Light time (s):           255.79885530
C
C
C        Frame evaluation locus:     CENTER
C
C        Target:                     MGS
C        Observation time:           2003 OCT 13 06:00:00.000000 UTC
C        Observer center:            EARTH
C        Observer frame:             ITRF93
C        Emission time:              2003 OCT 13 05:55:44.201144 UTC
C        Output reference frame:     ITRF93
C        Aberration correction:      CN+S
C
C        Observer-target position (km):
C             -53720595.74385086  -51381332.31464963  -18838416.34738571
C        Observer-target velocity (km/s):
C                 -3751.69880992       3911.72835653         -2.17503628
C        Light time (s):           255.79885530
C
C        Distance between above positions (km):             115.21404099
C        Velocity difference magnitude  (km/s):               0.00840050
C
C
C        State computed using SPKEZR:
C
C        Target:                 MGS
C        Observation time:       2003 OCT 13 06:00:00.000000 UTC
C        Output reference frame: ITRF93
C        Aberration correction:  CN+S
C        Observer:               DSS-14
C
C        Observer-target position (km):
C             -53720595.74378239  -51381332.31467460  -18838416.34737090
C        Observer-target velocity (km/s):
C                 -3751.69880992       3911.72835653         -2.17503628
C        Light time (s):         255.79885530
C
C        Distance between last two positions (km):            0.00007437
C        Velocity difference magnitude     (km/s):            0.00000000
C
C
C        Frame evaluation locus:     TARGET
C
C        Target:                     MARS
C        Observation time:           2003 OCT 13 06:00:00.000000 UTC
C        Observer center:            EARTH
C        Observer frame:             ITRF93
C        Emission time:              2003 OCT 13 05:55:44.201144 UTC
C        Output reference frame:     IAU_MARS
C        Aberration correction:      CN+S
C
C        Observer-target position (km):
C             -71445232.12770166    2312773.74175354   27766441.52048387
C        Observer-target velocity (km/s):
C                   155.65895286       5061.78618477          5.09447030
C        Light time (s):           255.79702283
C
C        Central meridian
C        longitude (deg):           -1.85409037
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
C-    SPICELIB Version 1.0.0, 27-MAR-2012 (NJB) (SCK) (BVS)
C
C-&
 
C$ Index_Entries
C
C     state relative to constant_position_observer
C     state relative to constant_position surface_point
C     state relative to surface_point on extended_object
C     state relative to landmark on extended_object
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
      LOGICAL               RETURN
 
C
C     Local variables
C      
      DOUBLE PRECISION      OBSEPC
      DOUBLE PRECISION      OBSSTA ( 6 )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKCPO' )

C
C     Create a state vector for the observer. The velocity
C     portion of the state is zero.
C
      CALL VEQU   ( OBSPOS, OBSSTA    )
      CALL CLEARD ( 3,      OBSSTA(4) )

C
C     Set the observation epoch; the value is arbitrary, since
C     the observer's velocity is zero.
C
      OBSEPC = 0.0D0

C
C     Compute the observer-target state vector.
C
      CALL SPKCVO ( TARGET, ET,     OUTREF, REFLOC, ABCORR,
     .              OBSSTA, OBSEPC, OBSCTR, OBSREF, STATE,  LT )

      CALL CHKOUT ( 'SPKCPO' )
      RETURN
      END


