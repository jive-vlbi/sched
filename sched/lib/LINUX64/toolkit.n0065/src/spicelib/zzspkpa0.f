C$Procedure ZZSPKPA0 ( S/P Kernel, apparent position only )
 
      SUBROUTINE ZZSPKPA0 ( TARG, ET, REF, SOBS, ABCORR, PTARG, LT )
 
C$ Abstract
C
C     Return the position of a target body relative to an observer,
C     optionally corrected for light time and stellar aberration.
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
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      DOUBLE PRECISION      SOBS     ( 6 )
      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      PTARG    ( 3 )
      DOUBLE PRECISION      LT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body.
C     ET         I   Observer epoch.
C     REF        I   Inertial reference frame of observer's state.
C     SOBS       I   State of observer wrt. solar system barycenter.
C     ABCORR     I   Aberration correction flag.
C     PTARG      O   Position of target.
C     LT         O   One way light time between observer and target.
C
C$ Detailed_Input
C
C     TARG        is the NAIF ID code for a target body.  The target
C                 and observer define a position vector which points
C                 from the observer to the target.
C
C     ET          is the ephemeris time, expressed as seconds past
C                 J2000 TDB, at which the position of the target body
C                 relative to the observer is to be computed.  ET
C                 refers to time at the observer's location.
C
C     REF         is the inertial reference frame with respect to which
C                 the observer's state SOBS is expressed. REF must be
C                 recognized by the SPICE Toolkit.  The acceptable
C                 frames are listed in the Frames Required Reading, as
C                 well as in the SPICELIB routine CHGIRF.
C
C                 Case and blanks are not significant in the string REF.
C
C     SOBS        is the geometric (uncorrected) state of the observer
C                 relative to the solar system barycenter at epoch ET.
C                 SOBS is a 6-vector:  the first three components of
C                 SOBS represent a Cartesian position vector; the last
C                 three components represent the corresponding velocity
C                 vector.  SOBS is expressed relative to the inertial
C                 reference frame designated by REF.
C
C                 Units are always km and km/sec.
C
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
C                               yields the position of the target at the
C                               moment it emitted photons arriving at
C                               the observer at ET.
C 
C                               The light time correction involves
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
C                               of the target as seen by the observer.
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
C                               of SPKEZR for a discussion of precision
C                               of light time corrections.
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
C                               formulation  This option modifies the
C                               position obtained with the 'XLT' option
C                               to account for the observer's velocity
C                               relative to the solar system
C                               barycenter. The target position
C                               indicates the direction that photons
C                               emitted from the observer's location
C                               must be "aimed" to hit the target.
C
C                    'XCN'      "Transmission" case:  converged 
C                               Newtonian light time correction.
C
C                    'XCN+S'    "Transmission" case:  converged
C                               Newtonian light time correction and
C                               stellar aberration correction.
C
C                 Neither special nor general relativistic effects are
C                 accounted for in the aberration corrections applied
C                 by this routine.
C
C                 Case and blanks are not significant in the string
C                 ABCORR.
C
C$ Detailed_Output
C
C     PTARG       is a Cartesian 3-vector representing the position of
C                 the target body relative to the specified observer.
C                 PTARG is corrected for the specified aberrations, and
C                 is expressed with respect to the specified inertial
C                 reference frame.  The components of PTARG represent
C                 the x-, y- and z-components of the target's position.
C
C                 The vector PTARG points from the observer's position
C                 at ET to the aberration-corrected location of the
C                 target. Note that the sense of the position vector is
C                 independent of the direction of radiation travel
C                 implied by the aberration correction.
C
C                 Units are always km.
C
C     LT          is the one-way light time between the observer and
C                 target in seconds.  If the target position is 
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
C     1) If the value of ABCORR is not recognized, the error
C        'SPICE(SPKINVALIDOPTION)' is signaled.
C
C     2) If the reference frame requested is not a recognized
C        inertial reference frame the error 'SPICE(BADFRAME)' is 
C        signaled.
C
C     3) If the position of the target relative to the solar system
C        barycenter cannot be computed, the error will be diagnosed 
C        by routines in the call tree of this routine.
C
C$ Files
C
C
C     This routine computes positions using SPK files that have been
C     loaded into the SPICE system, normally via the kernel loading
C     interface routine FURNSH.  Application programs typically load
C     kernels once before this routine is called, for example during
C     program initialization; kernels need not be loaded repeatedly.
C     See the routine FURNSH and the SPK and KERNEL Required Reading
C     for further information on loading (and unloading) kernels.
C
C     If any of the ephemeris data used to compute PTARG are expressed
C     relative to a non-inertial frame in the SPK files providing those
C     data, additional kernels may be needed to enable the reference
C     frame transformations required to compute PTARG.  Normally
C     these additional kernels are PCK files or frame kernels.  Any
C     such kernels must already be loaded at the time this routine is
C     called.
C
C$ Particulars
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
C     time and stellar aberration corrected."
C
C     The SPICE Toolkit can correct for two phenomena affecting the
C     apparent location of an object:  one-way light time (also called
C     "planetary aberration") and stellar aberration.  Correcting for
C     one-way light time is done by computing, given an observer and
C     observation epoch, where a target was when the observed photons
C     departed the target's location.  The vector from the observer to
C     this computed target location is called a "light time corrected"
C     vector.  The light time correction depends on the motion of the
C     target, but it is independent of the velocity of the observer
C     relative to the solar system barycenter. Relativistic effects
C     such as light bending and gravitational delay are not accounted
C     for in the light time correction performed by this routine.
C
C     The velocity of the observer also affects the apparent location
C     of a target:  photons arriving at the observer are subject to a
C     "raindrop effect" whereby their velocity relative to the observer
C     is, using a Newtonian approximation, the photons' velocity
C     relative to the solar system barycenter minus the velocity of the
C     observer relative to the solar system barycenter.  This effect is
C     called "stellar aberration."  Stellar aberration is independent
C     of the motion of the target.  The stellar aberration formula used
C     by this routine is non- relativistic.
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
C     transmission of photons to the target.  This requires correction
C     of the geometric target position for the effects of light time and
C     stellar aberration, but in this case the corrections are computed
C     for radiation traveling from the observer to the target. 
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
C     The traditional aberration corrections applicable to observation 
C     and those applicable to transmission are related in a simple way:
C     one may picture the geometry of the "transmission" case by 
C     imagining the "observation" case running in reverse time order,
C     and vice versa.  
C
C     One may reasonably object to using the term "observer" in the
C     transmission case, in which radiation is emitted from the
C     observer's location.  The terminology was retained for
C     consistency with earlier documentation.
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
C        5) Use a geometric position vector as a low-accuracy estimate
C           of the apparent position for an application where execution
C           speed is critical:
C
C              Use 'NONE'.
C
C
C        6) While this routine cannot perform the relativistic
C           aberration corrections required to compute positions
C           with the highest possible accuracy, it can supply the
C           geometric positions required as inputs to these 
C           computations:
C
C              Use 'NONE', then apply high-accuracy aberration
C              corrections (not available in the SPICE Toolkit).
C
C
C     Below, we discuss in more detail how the aberration corrections
C     applied by this routine are computed.     
C
C
C     Geometric case
C     ==============
C
C        SPKAPO begins by computing the geometric position T(ET) of the
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
C     Reception case
C     ==============
C
C        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' are
C        selected, SPKAPO computes the position of the target body at
C        epoch ET-LT, where LT is the one-way light time.  Let T(t)
C        and O(t) represent the positions of the target and observer
C        relative to the solar system barycenter at time t; then LT 
C        is the solution of the
C        light-time equation
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
C        RHS of the light-time equation (1) yields the "one-iteration"
C        estimate of the one-way light time. Repeating the process
C        until the estimates of LT converge yields the "converged
C        Newtonian" light time estimate.
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
C        
C        The light-time corrected position is the vector
C
C           T(ET-LT) - O(ET)
C
C        If correction for stellar aberration is requested, the target
C        position is rotated toward the solar system barycenter-relative
C        velocity vector of the observer. The magnitude of the rotation
C        depends on the magnitude of the observer's velocity relative
C        to the solar system barycenter and the angle between 
C        this velocity and the observer-target vector.  The rotation
C        is computed as follows:
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
C
C     Transmission case
C     ==================
C
C        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' are
C        selected, SPKAPO computes the position of the target body T at
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
C
C        The light-time corrected position is 
C
C           T(ET+LT) - O(ET)
C
C        If correction for stellar aberration is requested, the target
C        position is rotated away from the solar system barycenter-
C        relative velocity vector of the observer.  The magnitude of the
C        rotation depends on the magnitude of the velocity and the
C        angle between the velocity and the observer-target vector.
C        The rotation is computed as in the reception case, but the
C        sign of the rotation angle is negated.
C
C     Neither special nor general relativistic effects are accounted 
C     for in the aberration corrections performed by this routine.
C
C$ Examples
C
C     In the following code fragment, SPKSSB and SPKAPO are used
C     to display the position of Io (body 501) as seen from the 
C     Voyager 2 spacecraft (Body -32) at a series of epochs.
C
C     Normally, one would call the high-level reader SPKPOS to obtain
C     position vectors.  The example below illustrates the interface
C     of this routine, but is not intended as a recommendation on
C     how to use the SPICE SPK subsystem.
C
C     The use of integer ID codes is necessitated by the low-level
C     interface of this routine.
C
C        IO    = 501
C        VGR2  = -32
C
C        DO WHILE ( EPOCH .LE. END )
C
C           CALL SPKSSB (  VGR2,  EPOCH, 'J2000', STVGR2  )
C           CALL SPKAPO (  IO,    EPOCH, 'J2000', STVGR2,
C       .                 'LT+S', STIO,   LT              )
C
C           CALL RECRAD (  STIO,  RANGE,  RA,     DEC     )
C           WRITE (*,*)  RA * DPR(),  DEC * DPR()
C
C           EPOCH = EPOCH + DELTA
C
C        END DO
C
C$ Restrictions
C
C     1) The ephemeris files to be used by SPKAPO must be loaded
C        (normally by the SPICELIB kernel loader FURNSH) before 
C        this routine is called.
C
C     2) Unlike most other SPK position computation routines, this
C        routine requires that the input state be relative to an
C        inertial reference frame.  Non-inertial frames are not
C        supported by this routine.
C
C     3) In a future version of this routine, the implementation 
C        of the aberration corrections may be enhanced to improve
C        accuracy.    
C 
C$ Literature_References
C
C     SPK Required Reading.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.0, 03-JUL-2014 (NJB) (BVS) 
C
C        Discussion of light time corrections was updated. Assertions
C        that converged light time corrections are unlikely to be
C        useful were removed.
C
C     Last update was 21-SEP-2013 (BVS) 
C
C        Updated to call LJUCRS instead of CMPRSS/UCASE. 
C
C-    SPICELIB Version 2.2.0, 17-MAY-2010 (NJB)
C
C        Bug fix: routine now returns immediately after
C        state lookup failure.
C
C-    SPICELIB Version 2.1.0, 31-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSUB call.
C
C-    SPICELIB Version 2.0.1, 20-OCT-2003 (EDW)
C
C        Added mention that LT returns in seconds.
C        Corrected spelling errors. 
C
C-    SPICELIB Version 2.0.0, 18-DEC-2001 (NJB)
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
C     apparent position from spk file
C     get apparent position
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSUB call.
C
C-& 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VNORM

      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 5 )

      INTEGER               NFLAGS
      PARAMETER           ( NFLAGS = 9 )
C
C     Indices of flags in the FLAGS array:
C
      INTEGER               IXNONE
      PARAMETER           ( IXNONE = 1 )
      
      INTEGER               IXLT
      PARAMETER           ( IXLT   = IXNONE + 1 )

      INTEGER               IXLTS
      PARAMETER           ( IXLTS  = IXLT   + 1 )

      INTEGER               IXCN
      PARAMETER           ( IXCN   = IXLTS  + 1 )

      INTEGER               IXCNS
      PARAMETER           ( IXCNS  = IXCN   + 1 )

      INTEGER               IXXLT
      PARAMETER           ( IXXLT  = IXCNS  + 1 )

      INTEGER               IXXLTS
      PARAMETER           ( IXXLTS = IXXLT  + 1 )

      INTEGER               IXXCN
      PARAMETER           ( IXXCN  = IXXLTS + 1 )

      INTEGER               IXXCNS
      PARAMETER           ( IXXCNS = IXXCN  + 1 )

C
C     NAIF ID code for the solar system barycenter:
C      
      INTEGER               SSB
      PARAMETER           ( SSB    = 0 )
 
C
C     Local variables
C
      CHARACTER*(CORLEN)    CORR
      CHARACTER*(CORLEN)    FLAGS  ( NFLAGS )
      CHARACTER*(CORLEN)    PRVCOR
 
      DOUBLE PRECISION      TPOS   ( 3 )

      INTEGER               I
      INTEGER               LTSIGN
      INTEGER               MAXITR
      INTEGER               REFID
 
      LOGICAL               FIRST
      LOGICAL               USECN
      LOGICAL               USESTL
      LOGICAL               USELT
      LOGICAL               XMIT

C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  FLAGS
      SAVE                  PRVCOR
      SAVE                  USECN
      SAVE                  USESTL
      SAVE                  USELT
      SAVE                  XMIT
 
C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /

      DATA                  FLAGS   / 'NONE', 'LT',  'LT+S',
     .                                        'CN',  'CN+S',
     .                                        'XLT', 'XLT+S',
     .                                        'XCN', 'XCN+S'  /

      DATA                  PRVCOR  / ' ' /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZSPKPA0' )
      END IF
 

      IF (  FIRST  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
C
C        The aberration correction flag differs from the value it
C        had on the previous call, if any.  Analyze the new flag.
C
C        Remove leading and embedded white space from the aberration
C        correction flag, then convert to upper case.
C 
         CALL LJUCRS ( 0, ABCORR, CORR )
  
C
C        Locate the flag in our list of flags.
C
         I = ISRCHC ( CORR, NFLAGS, FLAGS )

         IF ( I .EQ. 0 ) THEN

            CALL SETMSG ( 'Requested aberration correction was #.' )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(SPKINVALIDOPTION)'                )
            CALL CHKOUT ( 'ZZSPKPA0'                                 )
            RETURN

         END IF

C
C        The aberration correction flag is recognized; save it.
C
         PRVCOR = ABCORR

C
C        Set logical flags indicating the attributes of the requested
C        correction.
C 
         XMIT    =  I .GT. IXCNS

         USELT   =       ( I .EQ. IXLT  ) .OR. ( I .EQ. IXLTS  ) 
     .              .OR. ( I .EQ. IXXLT ) .OR. ( I .EQ. IXXLTS )

         USESTL  =  ( I .GT. 1 )  .AND.  ODD(I)

         USECN   =       ( I .EQ. IXCN  ) .OR. ( I .EQ. IXCNS  ) 
     .              .OR. ( I .EQ. IXXCN ) .OR. ( I .EQ. IXXCNS )

         FIRST   = .FALSE.

      END IF

C
C     See if the reference frame is a recognized inertial frame.
C
      CALL IRFNUM ( REF, REFID )
 
      IF ( REFID .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The requested frame ''#'' is not a '
     .   //            'recognized inertial frame. '        )
         CALL ERRCH  ( '#', REF                             )
         CALL SIGERR ( 'SPICE(BADFRAME)'                    )
         CALL CHKOUT ( 'ZZSPKPA0'                             )
         RETURN
 
      END IF
 
C
C     Determine the sign of the light time offset.
C
      IF ( XMIT ) THEN
         LTSIGN = 1
      ELSE
         LTSIGN = -1
      END IF

C
C     Find the geometric position of the target body with respect to the
C     solar system barycenter. Subtract the position of the observer
C     to get the relative position. Use this to compute the one-way
C     light time.
C
      CALL ZZSPKGP0 ( TARG,   ET,   REF, SSB, PTARG, LT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSPKPA0' )
         RETURN
      END IF

      CALL VSUB   ( PTARG,  SOBS,           TPOS      )
      CALL VEQU   ( TPOS,   PTARG                     )
      LT = VNORM  ( PTARG ) / CLIGHT()
 
C
C     To correct for light time, find the position of the target body
C     at the current epoch minus the one-way light time. Note that
C     the observer remains where he is.
C
      IF ( USELT ) THEN
         MAXITR = 1
      ELSE IF ( USECN ) THEN
         MAXITR = 3
      ELSE
         MAXITR = 0
      END IF

 
      DO I = 1, MAXITR

         CALL ZZSPKGP0 ( TARG,   ET + LTSIGN*LT, REF, SSB, PTARG, LT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSPKPA0' )
            RETURN
         END IF

         CALL VSUB   ( PTARG,  SOBS,     TPOS )
         CALL VEQU   ( TPOS,   PTARG )
         LT = VNORM  ( PTARG ) / CLIGHT()

      END DO

C
C     At this point, PTARG contains the geometric or light-time 
C     corrected position of the target relative to the observer,
C     depending on the specified correction.
C
C     If stellar aberration correction is requested, perform it now.
C
      IF ( USESTL ) THEN

         IF ( XMIT ) THEN
C
C           This is the transmission case.  
C
C           Compute the position vector obtained by applying 
C           "reception" stellar aberration to PTARG.  
C
            CALL STLABX ( PTARG, SOBS(4), TPOS )
            CALL VEQU   ( TPOS,  PTARG )

         ELSE
C
C           This is the reception case.  
C
C           Compute the position vector obtained by applying 
C           "reception" stellar aberration to PTARG.  
C
            CALL STELAB ( PTARG, SOBS(4), TPOS )
            CALL VEQU   ( TPOS,  PTARG )

         END IF

      END IF
 
      CALL CHKOUT ( 'ZZSPKPA0' ) 
      RETURN
      END
