C$Procedure SPKPOS ( S/P Kernel, position )
 
      SUBROUTINE SPKPOS ( TARG, ET, REF, ABCORR, OBS, PTARG, LT )
 
C$ Abstract
C
C     Return the position of a target body relative to an observing
C     body, optionally corrected for light time (planetary aberration)
C     and stellar aberration.
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

      INCLUDE               'frmtyp.inc'
      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBS
      DOUBLE PRECISION      PTARG    ( 3 )
      DOUBLE PRECISION      LT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body name.
C     ET         I   Observer epoch.
C     REF        I   Reference frame of output position vector.
C     ABCORR     I   Aberration correction flag.
C     OBS        I   Observing body name.
C     PTARG      O   Position of target.
C     LT         O   One way light time between observer and target.
C
C$ Detailed_Input
C
C     TARG        is the name of a target body.  Optionally, you may
C                 supply the integer ID code for the object as an
C                 integer string.  For example both 'MOON' and '301'
C                 are legitimate strings that indicate the moon is the
C                 target body.
C
C                 The target and observer define a position vector
C                 which points from the observer to the target.
C
C     ET          is the ephemeris time, expressed as seconds past
C                 J2000 TDB, at which the position of the target body
C                 relative to the observer is to be computed.  ET
C                 refers to time at the observer's location.
C
C     REF         is the name of the reference frame relative to which
C                 the output position vector should be expressed. This
C                 may be any frame supported by the SPICE system,
C                 including built-in frames (documented in the Frames
C                 Required Reading) and frames defined by a loaded
C                 frame kernel (FK).
C
C                 When REF designates a non-inertial frame, the
C                 orientation of the frame is evaluated at an epoch
C                 dependent on the selected aberration correction. See
C                 the description of the output position vector PTARG
C                 for details.
C
C     ABCORR      indicates the aberration corrections to be applied to
C                 the position of the target body to account for
C                 one-way light time and stellar aberration.  See the
C                 discussion in the Particulars section for
C                 recommendations on how to choose aberration
C                 corrections.
C                  
C                 ABCORR may be any of the following:
C
C                    'NONE'     Apply no correction. Return the 
C                               geometric position of the target body 
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
C                               yields the position of the target at
C                               the moment it emitted photons arriving
C                               at the observer at ET.
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
C                               position obtained with the 'LT' option
C                               to account for the observer's velocity
C                               relative to the solar system
C                               barycenter. The result is the apparent
C                               position of the target---the position
C                               as seen by the observer.
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
C                               position of the target at the moment it
C                               receives photons emitted from the
C                               observer's location at ET.
C
C                    'XLT+S'    "Transmission" case:  correct for
C                               one-way light time and stellar
C                               aberration using a Newtonian
C                               formulation. This option modifies the
C                               position obtained with the 'XLT' option
C                               to account for the observer's velocity
C                               relative to the solar system
C                               barycenter. The computed target
C                               position indicates the direction that
C                               photons emitted from the observer's
C                               location must be "aimed" to hit the
C                               target.
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
C     OBS         is the name of an observing body.  Optionally, you
C                 may supply the ID code of the object as an integer
C                 string. For example, both 'EARTH' and '399' are
C                 legitimate strings to supply to indicate the
C                 observer is Earth.
C
C$ Detailed_Output
C
C     PTARG       is a Cartesian 3-vector representing the position of
C                 the target body relative to the specified observer.
C                 PTARG is corrected for the specified aberrations, and
C                 is expressed with respect to the reference frame
C                 specified by REF.  The three components of PTARG
C                 represent the x-, y- and z-components of the target's
C                 position.
C
C                 PTARG points from the observer's location at ET to
C                 the aberration-corrected location of the target.
C                 Note that the sense of this position vector is
C                 independent of the direction of radiation travel
C                 implied by the aberration correction.
C
C                 Units are always km.
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
C                 target in seconds. If the target position is 
C                 corrected for aberrations, then LT is the one-way
C                 light time between the observer and the light time
C                 corrected target location.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If name of target or observer cannot be translated to its
C        NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled.
C
C     2) If the reference frame REF is not a recognized reference
C        frame the error 'SPICE(UNKNOWNFRAME)' is signaled.
C
C     3) If the loaded kernels provide insufficient data to 
C        compute the requested position vector, the deficiency will
C        be diagnosed by a routine in the call tree of this routine.
C
C     4) If an error occurs while reading an SPK or other kernel file,
C        the error  will be diagnosed by a routine in the call tree 
C        of this routine.
C
C$ Files
C
C     This routine computes positions using SPK files that have been
C     loaded into the SPICE system, normally via the kernel loading
C     interface routine FURNSH. See the routine FURNSH and the SPK
C     and KERNEL Required Reading for further information on loading
C     (and unloading) kernels.
C
C     If the output position PTARG is to be expressed relative to a
C     non-inertial frame, or if any of the ephemeris data used to
C     compute PTARG are expressed relative to a non-inertial frame in
C     the SPK files providing those data, additional kernels may be
C     needed to enable the reference frame transformations required to
C     compute the position.  Normally these additional kernels are PCK
C     files or frame kernels.  Any such kernels must already be loaded
C     at the time this routine is called.
C
C$ Particulars
C
C     This routine is part of the user interface to the SPICE ephemeris
C     system.  It allows you to retrieve position information for any
C     ephemeris object relative to any other in a reference frame that
C     is convenient for further computations.
C
C     This routine is identical in function to the routine SPKEZP
C     except that it allows you to refer to ephemeris objects by name
C     (via a character string).
C
C
C     Aberration corrections
C     ======================
C
C     In space science or engineering applications one frequently
C     wishes to know where to point a remote sensing instrument, such
C     as an optical camera or radio antenna, in order to observe or
C     otherwise receive radiation from a target.  This pointing problem
C     is complicated by the finite speed of light:  one needs to point
C     to where the target appears to be as opposed to where it actually
C     is at the epoch of observation.  We use the adjectives
C     "geometric," "uncorrected," or "true" to refer to an actual
C     position or state of a target at a specified epoch.  When a
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
C     observed photons departed the target's location.  The vector from
C     the observer to this computed target location is called a "light
C     time corrected" vector.  The light time correction depends on the
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
C     observer relative to the solar system barycenter.  This effect is
C     called "stellar aberration."  Stellar aberration is independent
C     of the velocity of the target.  The stellar aberration formula
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
C     transmission of photons to the target.  This also requires
C     correction of the geometric target position for the effects of
C     light time and stellar aberration, but in this case the
C     corrections are computed for radiation traveling *from* the
C     observer to the target.
C
C     The "transmission" light time correction yields the target's
C     location as it will be when photons emitted from the observer's
C     location at ET arrive at the target.  The transmission stellar
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
C        4) Obtain an uncorrected position vector derived directly from 
C           data in an SPK file.
C
C              Use 'NONE'.
C
C
C        5) Use a geometric position vector as a low-accuracy estimate
C           of the apparent position for an application where execution 
C           speed is critical.
C
C              Use 'NONE'.
C
C
C        6) While this routine cannot perform the relativistic
C           aberration corrections required to compute positions
C           with the highest possible accuracy, it can supply the
C           geometric positions required as inputs to these 
C           computations.
C
C              Use 'NONE', then apply high-accuracy aberration
C              corrections (not available in the SPICE Toolkit).
C
C
C     Below, we discuss in more detail how the aberration corrections
C     applied by this routine are computed.     
C
C        Geometric case
C        ==============
C
C        SPKPOS begins by computing the geometric position T(ET) of the
C        target body relative to the solar system barycenter (SSB).
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
C        The returned position vector is
C
C           T(ET) - O(ET)
C
C
C
C        Reception case
C        ==============
C
C        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' is selected
C        for ABCORR, SPKPOS computes the position of the target body at
C        epoch ET-LT, where LT is the one-way light time.  Let T(t) and
C        O(t) represent the positions of the target and observer
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
C        The light time corrected position vector is
C
C           T(ET-LT) - O(ET)
C
C        If correction for stellar aberration is requested, the target
C        position is rotated toward the solar system barycenter-
C        relative velocity vector of the observer.  The rotation is
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
C
C        Transmission case
C        ==================
C
C        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' is
C        selected, SPKPOS computes the position of the target body T at
C        epoch ET+LT, where LT is the one-way light time.  LT is the
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
C        The light-time corrected position vector is
C
C           T(ET+LT) - O(ET)
C
C        If correction for stellar aberration is requested, the target
C        position is rotated away from the solar system barycenter-
C        relative velocity vector of the observer. The rotation is
C        computed as in the reception case, but the sign of the
C        rotation angle is negated.
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
C        velocity of the target relative to an inertial frame and C is
C        the speed of light.
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
C     aberration corrections.  For many applications relativistic
C     corrections are not worth the expense of added computation
C     cycles.  If however, your application requires these additional
C     corrections we suggest you consult the astronomical almanac (page
C     B36) for a discussion of how to carry out these corrections.
C
C
C$ Examples
C
C     1)  Load a planetary ephemeris SPK, then look up a series of
C         geometric positions of the moon relative to the earth,
C         referenced to the J2000 frame.
C
C
C               IMPLICIT NONE
C         C
C         C     Local constants
C         C
C               CHARACTER*(*)         FRAME
C               PARAMETER           ( FRAME  = 'J2000' )
C
C               CHARACTER*(*)         ABCORR
C               PARAMETER           ( ABCORR = 'NONE' )
C
C         C
C         C     The name of the SPK file shown here is fictitious;
C         C     you must supply the name of an SPK file available
C         C     on your own computer system.
C         C
C               CHARACTER*(*)         SPK
C               PARAMETER           ( SPK    = 'planet.bsp' )
C
C         C
C         C     ET0 represents the date 2000 Jan 1 12:00:00 TDB.
C         C 
C               DOUBLE PRECISION      ET0
C               PARAMETER           ( ET0    = 0.0D0 )
C
C         C
C         C     Use a time step of 1 hour; look up 100 positions.
C         C 
C               DOUBLE PRECISION      STEP
C               PARAMETER           ( STEP   = 3600.0D0 )
C
C               INTEGER               MAXITR
C               PARAMETER           ( MAXITR = 100 )
C                 
C               CHARACTER*(*)         OBSRVR
C               PARAMETER           ( OBSRVR = 'Earth' )
C         
C               CHARACTER*(*)         TARGET
C               PARAMETER           ( TARGET = 'Moon' )
C
C         C          
C         C     Local variables
C         C
C               DOUBLE PRECISION      ET
C               DOUBLE PRECISION      LT
C               DOUBLE PRECISION      POS ( 3 )
C
C               INTEGER               I
C
C         C
C         C     Load the SPK file.
C         C
C               CALL FURNSH ( SPK )
C
C         C
C         C     Step through a series of epochs, looking up a 
C         C     position vector at each one.
C         C
C               DO I = 1, MAXITR
C
C                  ET = ET0 + (I-1)*STEP
C
C                  CALL SPKPOS ( TARGET, ET, FRAME, ABCORR, OBSRVR, 
C              .                 POS,    LT                        )
C
C                  WRITE (*,*) 'ET = ', ET
C                  WRITE (*,*) 'J2000 x-position (km):   ', POS(1)
C                  WRITE (*,*) 'J2000 y-position (km):   ', POS(2)
C                  WRITE (*,*) 'J2000 z-position (km):   ', POS(3)
C                  WRITE (*,*) ' '
C
C               END DO
C
C               END
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     SPK Required Reading.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     B.V. Semenov    (JPL)
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 03-JUL-2014 (NJB) (BVS)
C
C        Discussion of light time corrections was updated. Assertions
C        that converged light time corrections are unlikely to be
C        useful were removed.
C
C     Last update was 19-SEP-2013 (BVS)
C
C        Updated to save the input body names and ZZBODTRN state
C        counters and to do name-ID conversions only if the counters
C        have changed.
C
C-    SPICELIB Version 3.0.3, 04-APR-2008 (NJB)
C
C        Corrected minor error in description of XLT+S aberration
C        correction.
C
C-    SPICELIB Version 3.0.2, 20-OCT-2003 (EDW)
C
C        Added mention that LT returns in seconds.
C
C-    SPICELIB Version 3.0.1, 29-JUL-2003 (NJB) (CHA)
C
C        Various minor header changes were made to improve clarity.
C
C-    SPICELIB Version 3.0.0, 31-DEC-2001 (NJB)
C
C        Updated to handle aberration corrections for transmission
C        of radiation.  Formerly, only the reception case was 
C        supported.  The header was revised and expanded to explain 
C        the functionality of this routine in more detail.
C
C-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
C
C-&
 
C$ Index_Entries
C
C     using body names get position relative to an observer
C     get position relative observer corrected for aberrations
C     read ephemeris data
C     read trajectory data
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
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )

C
C     Local variables
C
      INTEGER               TARGID
      INTEGER               OBSID
 
      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTGID
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSN
      INTEGER               SVOBSI
      LOGICAL               SVFND2

      LOGICAL               FIRST

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTGID
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVOBSN
      SAVE                  SVOBSI
      SAVE                  SVFND2

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKPOS' )
      END IF
 
C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )

         FIRST = .FALSE.

      END IF
 
C
C     Starting from translation of target name to its code
C
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTGID, SVFND1,
     .                TARG, TARGID, FOUND    )
  
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The target, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '
     .   //            'Alternatively you may call SPKEZP '
     .   //            'directly if you know the SPICE id-codes '
     .   //            'for both ''#'' and ''#'' '                )
         CALL ERRCH  ( '#', TARG                                  )
         CALL ERRCH  ( '#', TARG                                  )
         CALL ERRCH  ( '#', OBS                                   )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SPKPOS'                                   )
         RETURN
 
      END IF
 
C
C     Now do the same for observer.
C
      CALL ZZBODS2C ( SVCTR2, SVOBSN, SVOBSI, SVFND2,
     .                OBS, OBSID, FOUND    )
  
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '
     .   //            'Alternatively you may call SPKEZP '
     .   //            'directly if you know the SPICE id-codes '
     .   //            'for both ''#'' and ''#'' '                )
         CALL ERRCH  ( '#', OBS                                   )
         CALL ERRCH  ( '#', TARG                                  )
         CALL ERRCH  ( '#', OBS                                   )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SPKPOS'                                   )
         RETURN
 
      END IF 
 
C
C     After all translations are done we can call SPKEZP.
C
      CALL SPKEZP ( TARGID, ET, REF, ABCORR, OBSID, PTARG, LT )
 

      CALL CHKOUT ( 'SPKPOS' )
      RETURN
      END
