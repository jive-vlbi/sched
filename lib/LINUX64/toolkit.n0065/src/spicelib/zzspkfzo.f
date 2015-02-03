C$Procedure ZZSPKFZO ( SPK function, easy reader, observer )
 
      SUBROUTINE ZZSPKFZO ( TARG, ET, REF, ABCORR, OBSSUB, STARG, LT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Return the state (position and velocity) of a target body
C     relative to an observing body, optionally corrected for light
C     time (planetary aberration) and stellar aberration. An input
C     subroutine provides the state of the observer relative to its
C     center of motion.
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
C     NAIF_IDS
C     FRAMES
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE
      INCLUDE              'frmtyp.inc'
      INCLUDE              'zzabcorr.inc'

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      EXTERNAL              OBSSUB
      DOUBLE PRECISION      STARG    ( 6 )
      DOUBLE PRECISION      LT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body.
C     ET         I   Observer epoch.
C     REF        I   Reference frame of output state vector.
C     ABCORR     I   Aberration correction flag.
C     OBSSUB     I   Observing body state subroutine.
C     STARG      O   State of target.
C     LT         O   One way light time between observer and target.
C
C$ Detailed_Input
C
C     TARG        is the NAIF ID code for a target body. The target and
C                 observer define a state vector having a position
C                 component that points from the observer to the
C                 target.
C
C     ET          is the ephemeris time, expressed as seconds past J2000
C                 TDB, at which the state of the target body relative to
C                 the observer is to be computed. ET refers to time at
C                 the observer's location.
C
C     REF         is the name of the reference frame relative to which
C                 the output state vector should be expressed. This may
C                 be any frame supported by the SPICE system, including
C                 built-in frames (documented in the Frames Required
C                 Reading) and frames defined by a loaded frame kernel
C                 (FK).
C
C                 When REF designates a non-inertial frame, the
C                 orientation of the frame is evaluated at an epoch 
C                 dependent on the selected aberration correction.
C                 See the description of the output state vector STARG
C                 for details.
C
C     ABCORR      indicates the aberration corrections to be applied
C                 to the state of the target body to account for one-way
C                 light time and stellar aberration. See the discussion
C                 in the Particulars section for recommendations on 
C                 how to choose aberration corrections.
C                  
C                 ABCORR may be any of the following:
C
C                    'NONE'     Apply no correction. Return the 
C                               geometric state of the target body 
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
C                               equation (see Particulars for details).
C                               The solution invoked by the 'LT' option
C                               uses one iteration.
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
C                               until the solution converges (three
C                               iterations on all supported platforms).
C                               Whether the 'CN+S' solution is
C                               substantially more accurate than the
C                               'LT' solution depends on the geometry
C                               of the participating objects and on the
C                               accuracy of the input data. In all
C                               cases this routine will execute more
C                               slowly when a converged solution is
C                               computed. See the Particulars section
C                               below for a discussion of precision of
C                               light time corrections.
C
C                    'CN+S'     Converged Newtonian light time
C                               correction and stellar aberration
C                               correction.
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
C                               Newtonian light time correction and
C                               stellar aberration correction.
C
C
C                 Neither special nor general relativistic effects are
C                 accounted for in the aberration corrections applied
C                 by this routine.
C
C                 Case and blanks are not significant in the string
C                 ABCORR.
C
C     OBSSUB      is the name of an external subroutine that returns
C                 the geometric state of the observer relative to a
C                 center of motion, expressed in the inertial reference
C                 frame REF, at the epoch ET.
C
C                 The calling sequence of OBSSSUB is
C
C                    SUBROUTINE OBSSUB ( ET, REF, OBSCTR, STATE )
C
C                    DOUBLE PRECISION      ET
C                    CHARACTER*(*)         REF
C                    INTEGER               OBSCTR
C                    DOUBLE PRECISION      STATE ( 6 )
C
C                    The inputs of OBSSUB are ET and REF; the outputs
C                    are OBSCTR and STATE. STATE is the geometric state
C                    of the observer relative to the returned center of
C                    motion at ET, expressed in the frame REF.
C
C                 The target designated by TARG and the observer whose
C                 ephemeris is given by OBSSUB define a state vector
C                 having a position component that points from the
C                 observer to the target.
C
C$ Detailed_Output
C
C     STARG       is a Cartesian state vector representing the position
C                 and velocity of the target body relative to the
C                 specified observer. STARG is corrected for the
C                 specified aberrations, and is expressed with respect
C                 to the reference frame specified by REF. The first
C                 three components of STARG represent the x-, y- and
C                 z-components of the target's position; the last three
C                 components form the corresponding velocity vector.
C
C                 The position component of STARG points from the
C                 observer's location at ET to the aberration-corrected
C                 location of the target. Note that the sense of the
C                 position vector is independent of the direction of
C                 radiation travel implied by the aberration
C                 correction.
C
C                 The velocity component of STARG is the derivative
C                 with respect to time of the position component of
C                 STARG.
C
C                 Units are always km and km/sec.
C
C                 Non-inertial frames are treated as follows: letting
C                 LTCENT be the one-way light time between the observer
C                 and the central body associated with the frame, the
C                 orientation of the frame is evaluated at ET-LTCENT,
C                 ET+LTCENT, or ET depending on whether the requested
C                 aberration correction is, respectively, for received
C                 radiation, transmitted radiation, or is omitted.
C                 LTCENT is computed using the method indicated by
C                 ABCORR.
C
C     LT          is the one-way light time between the observer and
C                 target in seconds. If the target state is corrected
C                 for aberrations, then LT is the one-way light time 
C                 between the observer and the light time corrected 
C                 target location.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the reference frame REF is not a recognized reference
C        frame the error 'SPICE(UNKNOWNFRAME)' is signaled.
C
C     2) If an invalid aberration correction is specified, the
C        error will be diagnosed by a routine in the call tree
C        of this routine.
C
C     3) If the loaded kernels provide insufficient data to 
C        compute the requested state vector, the deficiency will
C        be diagnosed by a routine in the call tree of this routine.
C
C     4) If an error occurs while reading an SPK or other kernel file,
C        the error will be diagnosed by a routine in the call tree 
C        of this routine.
C
C$ Files
C
C     This routine computes states using SPK files that have been
C     loaded into the SPICE system, normally via the kernel loading
C     interface routine FURNSH. See the routine FURNSH and the SPK
C     and KERNEL Required Reading for further information on loading
C     (and unloading) kernels.
C
C     If the output state STARG is to be expressed relative to a
C     non-inertial frame, or if any of the ephemeris data used to
C     compute STARG are expressed relative to a non-inertial frame in
C     the SPK files providing those data, additional kernels may be
C     needed to enable the reference frame transformations required to
C     compute the state. Normally these additional kernels are PCK
C     files or frame kernels. Any such kernels must already be loaded
C     at the time this routine is called.
C
C$ Particulars
C
C     This routine is part of the user interface to the SPICE ephemeris
C     system. It allows you to retrieve state information for any
C     ephemeris object relative to any other in a reference frame that
C     is convenient for further computations.
C
C
C     Aberration corrections
C     ======================
C
C     In space science or engineering applications one frequently
C     wishes to know where to point a remote sensing instrument, such
C     as an optical camera or radio antenna, in order to observe or
C     otherwise receive radiation from a target. This pointing problem
C     is complicated by the finite speed of light:  one needs to point
C     to where the target appears to be as opposed to where it actually
C     is at the epoch of observation. We use the adjectives
C     "geometric," "uncorrected," or "true" to refer to an actual
C     position or state of a target at a specified epoch. When a
C     geometric position or state vector is modified to reflect how it
C     appears to an observer, we describe that vector by any of the
C     terms "apparent," "corrected," "aberration corrected," or "light
C     time and stellar aberration corrected." The SPICE Toolkit can
C     correct for two phenomena affecting the apparent location of an
C     object:  one-way light time (also called "planetary aberration")
C     and stellar aberration.
C
C     One-way light time
C     ------------------
C
C     Correcting for one-way light time is done by computing, given an
C     observer and observation epoch, where a target was when the
C     observed photons departed the target's location. The vector from
C     the observer to this computed target location is called a "light
C     time corrected" vector. The light time correction depends on the
C     motion of the target relative to the solar system barycenter, but
C     it is independent of the velocity of the observer relative to the
C     solar system barycenter. Relativistic effects such as light
C     bending and gravitational delay are not accounted for in the
C     light time correction performed by this routine.
C
C     Stellar aberration
C     ------------------
C
C     The velocity of the observer also affects the apparent location
C     of a target:  photons arriving at the observer are subject to a
C     "raindrop effect" whereby their velocity relative to the observer
C     is, using a Newtonian approximation, the photons' velocity
C     relative to the solar system barycenter minus the velocity of the
C     observer relative to the solar system barycenter. This effect is
C     called "stellar aberration."  Stellar aberration is independent
C     of the velocity of the target. The stellar aberration formula
C     used by this routine does not include (the much smaller) 
C     relativistic effects.
C
C     Stellar aberration corrections are applied after light time
C     corrections:  the light time corrected target position vector is 
C     used as an input to the stellar aberration correction.
C
C     When light time and stellar aberration corrections are both
C     applied to a geometric position vector, the resulting position 
C     vector indicates where the target "appears to be" from the
C     observer's location. 
C
C     As opposed to computing the apparent position of a target, one
C     may wish to compute the pointing direction required for
C     transmission of photons to the target. This also requires
C     correction of the geometric target position for the effects of
C     light time and stellar aberration, but in this case the
C     corrections are computed for radiation traveling *from* the
C     observer to the target.
C
C     The "transmission" light time correction yields the target's
C     location as it will be when photons emitted from the observer's
C     location at ET arrive at the target. The transmission stellar
C     aberration correction is the inverse of the traditional stellar
C     aberration correction:  it indicates the direction in which
C     radiation should be emitted so that, using a Newtonian
C     approximation, the sum of the velocity of the radiation relative
C     to the observer and of the observer's velocity, relative to the 
C     solar system barycenter, yields a velocity vector that points in 
C     the direction of the light time corrected position of the target.
C   
C     One may object to using the term "observer" in the transmission
C     case, in which radiation is emitted from the observer's location.
C     The terminology was retained for consistency with earlier
C     documentation.
C
C     Below, we indicate the aberration corrections to use for some
C     common applications:
C
C        1) Find the apparent direction of a target for a remote-sensing
C           observation.
C
C              Use 'LT+S' or 'CN+S: apply both light time and stellar 
C              aberration corrections.
C
C           Note that using light time corrections alone ('LT' or 'CN')
C           is generally not a good way to obtain an approximation to
C           an apparent target vector: since light time and stellar
C           aberration corrections often partially cancel each other,
C           it may be more accurate to use no correction at all than to
C           use light time alone.
C
C
C        2) Find the corrected pointing direction to radiate a signal
C           to a target. This computation is often applicable for 
C           implementing communications sessions.
C
C              Use 'XLT+S' or 'XCN+S: apply both light time and stellar 
C              aberration corrections for transmission.
C
C
C        3) Compute the apparent position of a target body relative
C           to a star or other distant object.
C
C              Use 'LT', 'CN', 'LT+S', or 'CN+S' as needed to match the
C              correction applied to the position of the distant
C              object. For example, if a star position is obtained from
C              a catalog, the position vector may not be corrected for
C              stellar aberration. In this case, to find the angular
C              separation of the star and the limb of a planet, the
C              vector from the observer to the planet should be
C              corrected for light time but not stellar aberration.
C
C
C        4) Obtain an uncorrected state vector derived directly from 
C           data in an SPK file.
C
C              Use 'NONE'.
C
C
C        5) Use a geometric state vector as a low-accuracy estimate
C           of the apparent state for an application where execution 
C           speed is critical.
C
C              Use 'NONE'.
C
C
C        6) While this routine cannot perform the relativistic
C           aberration corrections required to compute states
C           with the highest possible accuracy, it can supply the
C           geometric states required as inputs to these computations.
C
C              Use 'NONE', then apply relativistic aberration
C              corrections (not available in the SPICE Toolkit).
C
C
C     Below, we discuss in more detail how the aberration corrections
C     applied by this routine are computed.     
C
C        Geometric case
C        ==============
C
C        ZZSPKFZO begins by computing the geometric position T(ET) of
C        the target body relative to the solar system barycenter (SSB).
C        Subtracting the geometric position of the observer O(ET) gives
C        the geometric position of the target body relative to the
C        observer. The one-way light time, LT, is given by
C
C                  | T(ET) - O(ET) |
C           LT = -------------------
C                          c
C
C        The geometric relationship between the observer, target, and
C        solar system barycenter is as shown:
C
C
C           SSB ---> O(ET)
C            |      /
C            |     /
C            |    /                           
C            |   /  T(ET) - O(ET)  
C            V  V                                  
C           T(ET)
C
C
C        The returned state consists of the position vector
C
C           T(ET) - O(ET)
C
C        and a velocity obtained by taking the difference of the
C        corresponding velocities. In the geometric case, the 
C        returned velocity is actually the time derivative of the 
C        position.
C
C
C        Reception case
C        ==============
C
C        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' is selected
C        for ABCORR, ZZSPKFZO computes the position of the target body
C        at epoch ET-LT, where LT is the one-way light time. Let T(t)
C        and O(t) represent the positions of the target and observer
C        relative to the solar system barycenter at time t; then LT is
C        the solution of the light-time equation
C
C                  | T(ET-LT) - O(ET) |
C           LT = ------------------------                            (1)
C                           c
C
C        The ratio 
C
C            | T(ET) - O(ET) |
C          ---------------------                                     (2)
C                    c
C
C        is used as a first approximation to LT; inserting (2) into the
C        right hand side of the light-time equation (1) yields the
C        "one-iteration" estimate of the one-way light time ("LT").
C        Repeating the process until the estimates of LT converge
C        yields the "converged Newtonian" light time estimate ("CN").
C
C        Subtracting the geometric position of the observer O(ET) gives
C        the position of the target body relative to the observer:
C        T(ET-LT) - O(ET).
C
C           SSB ---> O(ET)
C            | \     |
C            |  \    |
C            |   \   | T(ET-LT) - O(ET)
C            |    \  |
C            V     V V
C           T(ET)  T(ET-LT)
C        
C        The position component of the light time corrected state 
C        is the vector
C
C           T(ET-LT) - O(ET)
C
C        The velocity component of the light time corrected state 
C        is the difference
C
C           T_vel(ET-LT)*(1-dLT/dET) - O_vel(ET)
C            
C        where T_vel and O_vel are, respectively, the velocities of the
C        target and observer relative to the solar system barycenter at
C        the epochs ET-LT and ET. 
C
C        If correction for stellar aberration is requested, the target
C        position is rotated toward the solar system barycenter-
C        relative velocity vector of the observer. The rotation is
C        computed as follows:
C
C           Let r be the light time corrected vector from the observer
C           to the object, and v be the velocity of the observer with
C           respect to the solar system barycenter. Let w be the angle
C           between them. The aberration angle phi is given by
C
C              sin(phi) = v sin(w) / c
C
C           Let h be the vector given by the cross product
C
C              h = r X v
C
C           Rotate r by phi radians about h to obtain the apparent
C           position of the object.
C
C        When stellar aberration corrections are used, the rate of
C        change of the stellar aberration correction is accounted for
C        in the computation of the output velocity.
C
C
C        Transmission case
C        ==================
C
C        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' is
C        selected, ZZSPKFZO computes the position of the target body T
C        at epoch ET+LT, where LT is the one-way light time. LT is the
C        solution of the light-time equation
C
C                  | T(ET+LT) - O(ET) |
C           LT = ------------------------                            (3)
C                            c
C
C        Subtracting the geometric position of the observer, O(ET),
C        gives the position of the target body relative to the
C        observer: T(ET-LT) - O(ET).
C
C                   SSB --> O(ET)
C                  / |    * 
C                 /  |  *  T(ET+LT) - O(ET)  
C                /   |*     
C               /   *|    
C              V  V  V     
C          T(ET+LT)  T(ET)    
C
C        The position component of the light-time corrected state 
C        is the vector
C
C           T(ET+LT) - O(ET)
C
C        The velocity component of the light-time corrected state 
C        consists of the difference
C
C           T_vel(ET+LT)*(1+dLT/dET) - O_vel(ET)
C            
C        where T_vel and O_vel are, respectively, the velocities of the
C        target and observer relative to the solar system barycenter at
C        the epochs ET+LT and ET.
C
C        If correction for stellar aberration is requested, the target
C        position is rotated away from the solar system barycenter-
C        relative velocity vector of the observer. The rotation is
C        computed as in the reception case, but the sign of the
C        rotation angle is negated. Velocities are adjusted to account
C        for the rate of change of the stellar aberration correction.
C
C
C     Precision of light time corrections
C     ===================================
C
C        Corrections using one iteration of the light time solution
C        ----------------------------------------------------------
C
C        When the requested aberration correction is 'LT', 'LT+S',
C        'XLT', or 'XLT+S', only one iteration is performed in the
C        algorithm used to compute LT.
C
C        The relative error in this computation
C
C           | LT_ACTUAL - LT_COMPUTED |  /  LT_ACTUAL
C
C        is at most 
C
C            (V/C)**2
C           ----------
C            1 - (V/C)
C
C        which is well approximated by (V/C)**2, where V is the
C        velocity of the target relative to the solar system
C        barycenter, expressed in an inertial frame, and C is
C        the speed of light.
C
C
C        For nearly all objects in the solar system V is less than 60
C        km/sec. The value of C is ~300000 km/sec. Thus the
C        one-iteration solution for LT has a potential relative error
C        of not more than 4e-8. This is a potential light time error of
C        approximately 2e-5 seconds per astronomical unit of distance
C        separating the observer and target. Given the bound on V cited
C        above:
C
C           As long as the observer and target are separated by less
C           than 50 astronomical units, the error in the light time
C           returned using the one-iteration light time corrections is
C           less than 1 millisecond.
C
C           The magnitude of the corresponding position error, given
C           the above assumptions, may be as large as (V/C)**2 * the
C           distance between the observer and the uncorrected target
C           position: 300 km or equivalently 6 km/AU.
C
C        In practice, the difference between positions obtained using
C        one-iteration and converged light time is usually much smaller
C        than the value computed above and can be insignificant. For
C        example, for the spacecraft Mars Reconnaissance Orbiter and
C        Mars Express, the position error for the one-iteration light
C        time correction, applied to the spacecraft-to-Mars center
C        vector, is at the 1 cm level.
C        
C        Comparison of results obtained using the one-iteration and
C        converged light time solutions is recommended when adequacy of
C        the one-iteration solution is in doubt. 
C
C
C        Converged corrections 
C        ---------------------
C
C        When the requested aberration correction is 'CN', 'CN+S',
C        'XCN', or 'XCN+S', as many iterations as are required for
C        convergence are performed in the computation of LT. Usually
C        the solution is found after three iterations. The relative
C        error present in this case is at most
C
C            (V/C)**4
C           ----------
C            1 - (V/C)
C
C        which is well approximated by (V/C)**4. 
C
C           The precision of this computation (ignoring round-off
C           error) is better than 4e-11 seconds for any pair of objects
C           less than 50 AU apart, and having speed relative to the
C           solar system barycenter less than 60 km/s.
C
C           The magnitude of the corresponding position error, given
C           the above assumptions, may be as large as (V/C)**4 * the
C           distance between the observer and the uncorrected target
C           position: 1.2 cm at 50 AU or equivalently 0.24 mm/AU.
C
C        However, to very accurately model the light time between
C        target and observer one must take into account effects due to
C        general relativity. These may be as high as a few hundredths
C        of a millisecond for some objects.
C
C
C     Relativistic Corrections
C     =========================
C
C     This routine does not attempt to perform either general or
C     special relativistic corrections in computing the various
C     aberration corrections. For many applications relativistic
C     corrections are not worth the expense of added computation
C     cycles. If your application requires these additional corrections
C     we suggest you consult the astronomical almanac (page B36) for a
C     discussion of how to carry out these corrections.
C
C
C$ Examples
C
C     See usage in CVOSTA.
C
C$ Restrictions
C
C     1) This routine must not be called by routines of the SPICE
C        frame or SPK subsystems.
C
C     2) The input subroutine OBSSUB must not call this routine
C        or any of the supporting, private SPK routines
C
C           ZZSPKFAO
C           ZZSPKFAP
C           ZZSPKFAT
C           ZZSPKFLT
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     W.L. Taber      (JPL)
C     N.J. Bachman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 04-JUL-2014 (NJB)
C
C        Discussion of light time corrections was updated. Assertions
C        that converged light time corrections are unlikely to be
C        useful were removed.
C
C     Last update was 22-FEB-2012 (CHA) (NJB) (WLT) (IMU)
C-&
 
C$ Index_Entries
C
C     observer function easy reader for spk file
C
C-&
 
 
 
C$ Revisions
C
C     None.
C
C-&
 
C
C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VNORM

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C 
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  =  'ZZSPKFZO' )

C
C     Local variables
C 
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      CORXFM   ( 6, 6 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DLTCTR
      DOUBLE PRECISION      LTCENT
      DOUBLE PRECISION      STATE    ( 6    )
      DOUBLE PRECISION      STEMP    ( 6    )
      DOUBLE PRECISION      STOBS    ( 6    )
      DOUBLE PRECISION      STOCTR   ( 6    )
      DOUBLE PRECISION      XFORM    ( 6, 6 )
 
      INTEGER               CENTER
      INTEGER               FJ2000
      INTEGER               LTSIGN
      INTEGER               OBSCTR
      INTEGER               REQFRM
      INTEGER               TYPE
      INTEGER               TYPEID
 
      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND
      LOGICAL               PASS1
      LOGICAL               USEGEO
      LOGICAL               XMIT

C
C     Saved variables
C
      SAVE                  FJ2000
      SAVE                  PASS1
      SAVE                  PRVCOR
      SAVE                  USEGEO
      SAVE                  XMIT

C
C     Initial values
C
      DATA                  PASS1   /.TRUE./
      DATA                  PRVCOR  / ' ' /

 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )


      IF (  PASS1  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
C
C        The aberration correction flag differs from the value it
C        had on the previous call, if any. Analyze the new flag.
C
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        The aberration correction flag is recognized; save it.
C
         PRVCOR = ABCORR

C
C        Set logical flags indicating the attributes of the requested
C        correction:
C
C           XMIT is .TRUE. when the correction is for transmitted
C           radiation.
C
C           USEGEO indicates geometric state computation.
C
C        The above definitions are consistent with those used by
C        ZZVALCOR.
C 
         XMIT    =  ATTBLK ( XMTIDX )
         USEGEO  =  ATTBLK ( GEOIDX )

C
C        Get the frame ID for J2000 on the first call to this routine.
C
         IF ( PASS1 ) THEN

            CALL NAMFRM( 'J2000', FJ2000 )   

            PASS1 = .FALSE.

         END IF

      END IF

C
C     If we only want a geometric state, then use SPKGEO to compute
C     just that.
C
C     Otherwise, if REF is inertial, compute the state of the target
C     relative to the observer via SPKACS. If REF is non-inertial,
C     compute the requested state in the J2000 frame, then transform it
C     to the frame designated by REF.
C
      IF ( USEGEO ) THEN
C
C        Get the state of the target relative to its center at ET.
C
C        Start by getting the state of the observer with respect to its
C        center of motion; subtract this from the state of the target
C        with respect to this center.
C 
         CALL OBSSUB ( ET,    REF,    OBSCTR, STOCTR )

         CALL SPKGEO ( TARG,  ET,     REF,    OBSCTR, STEMP, LTCENT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL VSUBG  ( STEMP, STOCTR, 6,      STARG  )

         LT = VNORM ( STARG ) / CLIGHT()

      ELSE
C
C        Get the auxiliary information about the requested output
C        frame.
C
         CALL NAMFRM ( REF, REQFRM )
 
         IF ( REQFRM .EQ. 0 ) THEN
            CALL SETMSG ( 'The requested output frame ''#'' is '
     .      //            'not recognized by the reference frame '
     .      //            'subsystem. Please check that the '
     .      //            'appropriate kernels have been loaded '
     .      //            'and that you have correctly entered '
     .      //            'the name of the output frame. ' )
            CALL ERRCH  ( '#', REF )
            CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
            CALL CHKOUT ( RNAME )
            RETURN
 
         END IF
 
 
         CALL FRINFO ( REQFRM, CENTER, TYPE, TYPEID, FOUND )
 
C
C        If we are dealing with an inertial frame, we can simply
C        call ZZSPKFAO and return.
C
         IF ( TYPE .EQ. INERTL ) THEN
 
            CALL ZZSPKFAO ( TARG,   ET,    REF, ABCORR,
     .                      OBSSUB, STARG, LT,  DLT    )
            CALL CHKOUT ( RNAME )
            RETURN
 
         END IF

C
C        Still here?
C
C        We are dealing with a non-inertial frame. But we need to do
C        light time and stellar aberration corrections in an inertial
C        frame. Get the "apparent" state of TARG in the intermediary
C        inertial reference frame J2000.
C
C        We also need the light time to the center of the frame.
C        We compute that first so that we can re-use the temporary
C        variable STATE when we compute the inertial apparent state
C        of the target relative to the observer.
C
         CALL ZZSPKFAO ( TARG,   ET,    'J2000', ABCORR,
     .                   OBSSUB, STATE, LT,      DLT    )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        If the frame is centered at the target, the light time and
C        light time rate have both been computed already. If the frame
C        is centered elsewhere, we'll need to obtain the light time
C        between the observer and the frame center. Unlike SPKEZ, we
C        have no case here for the frame centered at the observer,
C        since the observer isn't an ephemeris object, as far as this
C        routine can determine.
C
         IF ( CENTER .EQ. TARG ) THEN
C
C           We already have the light time and light time rate
C           for the frame center as seen from the observer.
C
            LTCENT = LT
            DLTCTR = DLT

         ELSE

            CALL OBSSUB ( ET,      'J2000',  OBSCTR,  STOCTR )
            CALL SPKSSB ( OBSCTR,  ET,      'J2000',  STEMP  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL VADDG  ( STEMP,   STOCTR,  6,        STOBS  )

            CALL SPKLTC ( CENTER, ET,     'J2000', ABCORR, STOBS,
     .                    STEMP,  LTCENT, DLTCTR                 )
         END IF

C
C        If something went wrong (like we couldn't get the state of
C        the center relative to the observer) now it is time to quit.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
 
C
C        If the aberration corrections are for transmission, make the
C        sign of the light time positive, since we wish to compute the
C        orientation of the non-inertial frame at an epoch later than
C        ET by the one-way light time.
C
         IF ( XMIT ) THEN

            LTSIGN =  1
         ELSE
            LTSIGN = -1
         END IF

C
C        Get the state transformation from J2000 to the requested frame
C        and convert the state.
C
         CALL FRMCHG ( FJ2000, REQFRM, ET + LTSIGN*LTCENT, XFORM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        There's a tricky bit here:  since XFORM is evaluated
C        at time
C
C           ET + LTSIGN*LTCENT
C
C        XFORM is actually dependent on LTCENT. We need to account for
C        this dependency in our velocity transformation.
C
         CALL ZZCORSXF ( XMIT, DLTCTR, XFORM, CORXFM )

C
C        Now apply the frame transformation CORXFM to produce the
C        state expressed relative to the request frame REQFRM.
C
         CALL MXVG ( CORXFM, STATE, 6, 6, STARG )
 
      END IF
 
      CALL CHKOUT ( RNAME )
      RETURN
      END

