C$Procedure ZZSPKAS1 ( SPK, apparent state )
 
      SUBROUTINE ZZSPKAS1 ( TARG,  ET,     REF,   ABCORR, 
     .                      STOBS, ACCOBS, STARG, LT,     DLT )
 
C$ Abstract
C
C     Given the state and acceleration of an observer relative to the
C     solar system barycenter, return the state (position and velocity)
C     of a target body relative to the observer, optionally corrected
C     for light time and stellar aberration. All input and output
C     vectors are expressed relative to an inertial reference frame.
C
C     This routine supersedes SPKAPP.
C
C     SPICE users normally should call the high-level API routines
C     SPKEZR or SPKEZ rather than this routine.
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

      INCLUDE 'zzabcorr.inc'
 
      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      ACCOBS ( 3 )
      DOUBLE PRECISION      STARG  ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DLT

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body.
C     ET         I   Observer epoch.
C     REF        I   Inertial reference frame of output state.
C     ABCORR     I   Aberration correction flag.
C     STOBS      I   State of the observer relative to the SSB.
C     ACCOBS     I   Acceleration of the observer relative to the SSB.
C     STARG      O   State of target.
C     LT         O   One way light time between observer and target.
C     DLT        O   Derivative of light time with respect to time.
C
C$ Detailed_Input
C
C     TARG        is the NAIF ID code for a target body.  The target
C                 and observer define a state vector whose position
C                 component points from the observer to the target.
C
C     ET          is the ephemeris time, expressed as seconds past
C                 J2000 TDB, at which the state of the target body
C                 relative to the observer is to be computed.  ET
C                 refers to time at the observer's location.
C
C     REF         is the inertial reference frame with respect to which
C                 the input state STOBS, the input acceleration ACCOBS,
C                 and the output state STARG are expressed. REF must be
C                 recognized by the SPICE Toolkit.  The acceptable
C                 frames are listed in the Frames Required Reading, as
C                 well as in the SPICELIB routine CHGIRF.
C
C                 Case and blanks are not significant in the string
C                 REF.
C 
C     ABCORR      indicates the aberration corrections to be applied
C                 to the state of the target body to account for one-way
C                 light time and stellar aberration. See the discussion
C                 in the header of SPKEZR for recommendations on 
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
C                               computed. See the Particulars section of
C                               SPKEZR for a discussion of precision of
C                               light time corrections.
C
C                    'CN+S'     Converged Newtonian light time
C                               correction and stellar aberration
C                               correction.
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
C                                 
C     STOBS       is the geometric state of the observer relative to
C                 the solar system barycenter at ET. STOBS is expressed
C                 relative to the reference frame designated by REF.
C                 The target and observer define a state vector whose
C                 position component points from the observer to the
C                 target.
C
C     ACCOBS      is the geometric acceleration of the observer
C                 relative to the solar system barycenter at ET. This
C                 is the derivative with respect to time of the
C                 velocity portion of STOBS. ACCOBS is expressed
C                 relative to the reference frame designated by REF.
C
C                 ACCOBS is used for computing stellar aberration
C                 corrected velocity. If stellar aberration corrections
C                 are not specified by ABCORR, ACCOBS is ignored; the
C                 caller need not provide a valid input value in this
C                 case.
C                   
C$ Detailed_Output
C
C     STARG       is a Cartesian state vector representing the position
C                 and velocity of the target body relative to the
C                 specified observer. STARG is corrected for the
C                 specified aberrations, and is expressed with respect
C                 to the inertial reference frame designated by REF.
C                 The first three components of STARG represent the x-,
C                 y- and z-components of the target's position; last
C                 three components form the corresponding velocity
C                 vector.
C
C                 The position component of STARG points from the
C                 observer's location at ET to the aberration-corrected
C                 location of the target. Note that the sense of the
C                 position vector is independent of the direction of
C                 radiation travel implied by the aberration
C                 correction.
C
C                 Units are always km and km/sec.
C
C     LT          is the one-way light time between the observer and
C                 target in seconds.  If the target state is corrected
C                 for light time, then LT is the one-way light time 
C                 between the observer and the light time-corrected 
C                 target location.
C
C     DLT         is the derivative with respect to barycentric
C                 dynamical time of the one way light time between
C                 target and observer:
C
C                    DLT = d(LT)/d(ET)
C
C                 DLT can also be described as the rate of change of 
C                 one way light time. DLT is unitless, since LT and
C                 ET both have units of TDB seconds.
C
C                 If the observer and target are at the same position,
C                 then DLT is set to zero.
C 
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the value of ABCORR is not recognized, the error
C        is diagnosed by a routine in the call tree of this
C        routine.
C
C     2) If ABCORR calls for stellar aberration but not light
C        time corrections, the error SPICE(NOTSUPPORTED) is
C        signaled.
C
C     3) If ABCORR calls for relativistic light time corrections, the
C        error SPICE(NOTSUPPORTED) is signaled.
C
C     4) If the reference frame requested is not a recognized
C        inertial reference frame, the error SPICE(BADFRAME)
C        is signaled.
C
C     5) If the state of the target relative to the solar system
C        barycenter cannot be computed, the error will be diagnosed 
C        by routines in the call tree of this routine.
C
C     6) If the observer and target are at the same position,
C        then DLT is set to zero. This situation could arise,
C        for example, when the observer is Mars and the target
C        is the Mars barycenter.
C
C$ Files
C
C     This routine computes states using SPK files that have been
C     loaded into the SPICE system, normally via the kernel loading
C     interface routine FURNSH.  Application programs typically load
C     kernels once before this routine is called, for example during
C     program initialization; kernels need not be loaded repeatedly.
C     See the routine FURNSH and the SPK and KERNEL Required Reading
C     for further information on loading (and unloading) kernels.
C
C     If any of the ephemeris data used to compute STARG are expressed
C     relative to a non-inertial frame in the SPK files providing those
C     data, additional kernels may be needed to enable the reference
C     frame transformations required to compute the state. Normally
C     these additional kernels are PCK files or frame kernels. Any such
C     kernels must already be loaded at the time this routine is
C     called.
C
C$ Particulars
C
C     This routine supports higher-level SPK API routines that can
C     perform both light time and stellar aberration corrections.
C
C     User applications normally will not need to call this routine
C     directly. However, this routine can improve run-time efficiency
C     in situations where many targets are observed from the same
C     location at the same time. In such cases, the state and
C     acceleration of the observer relative to the solar system
C     barycenter need be computed only once per look-up epoch.
C
C     When apparent positions, rather than apparent states, are
C     required, consider using the high-level position-only API
C     routines
C
C        SPKPOS
C        SPKEZP
C
C     or the low-level, position-only analog of this routine
C
C        SPKAPO
C
C     In general, the position-only routines are more efficient.
C
C     See the header of the routine SPKEZR for a detailed discussion
C     of aberration corrections.
C
C$ Examples
C
C    1) Look up a sequence of states of the Moon as seen from the
C       Earth. Use light time and stellar aberration corrections.
C       Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB;
C       compute subsequent states at intervals of 1 hour. For each
C       epoch, display the states, the one way light time between
C       target and observer, and the rate of change of the one way
C       light time.
C
C       Use the following meta-kernel to specify the kernels to 
C       load:
C
C          KPL/MK
C
C          This meta-kernel is intended to support operation of SPICE
C          example programs. The kernels shown here should not be
C          assumed to contain adequate or correct versions of data
C          required by SPICE-based user applications.
C
C          In order for an application to use this meta-kernel, the
C          kernels referenced here must be present in the user's
C          current working directory.
C
C
C          \begindata
C
C             KERNELS_TO_LOAD = ( 'de418.bsp', 
C                                 'pck00008.tpc',
C                                 'naif0008.tls'  )
C
C          \begintext
C
C
C       The code example follows:
C
C           PROGRAM EX1
C           IMPLICIT NONE
C     C
C     C     Local constants
C     C
C     C     The meta-kernel name shown here refers to a file whose 
C     C     contents are those shown above. This file and the kernels 
C     C     it references must exist in your current working directory.
C     C
C           CHARACTER*(*)         META
C           PARAMETER           ( META   = 'example.mk' )
C     C
C     C     Use a time step of 1 hour; look up 5 states.
C     C
C           DOUBLE PRECISION      STEP
C           PARAMETER           ( STEP   = 3600.0D0 )
C
C           INTEGER               MAXITR
C           PARAMETER           ( MAXITR = 5 )
C     C
C     C     Local variables
C     C
C           DOUBLE PRECISION      ACC    ( 3 )
C           DOUBLE PRECISION      DLT
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      STATE  ( 6 )
C           DOUBLE PRECISION      STATE0 ( 6 )
C           DOUBLE PRECISION      STATE2 ( 6 )
C           DOUBLE PRECISION      STOBS  ( 6 )
C           DOUBLE PRECISION      TDELTA
C           INTEGER               I
C
C     C
C     C     Load the SPK and LSK kernels via the meta-kernel.
C     C
C           CALL FURNSH ( META )
C     C
C     C     Convert the start time to seconds past J2000 TDB.
C     C
C           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 )
C     C
C     C     Step through a series of epochs, looking up a
C     C     state vector at each one.
C     C
C           DO I = 1, MAXITR
C
C              ET = ET0 + (I-1)*STEP
C
C     C
C     C        Look up a state vector at epoch ET using the
C     C        following inputs:
C     C
C     C           Target:                 Moon (NAIF ID code 301)
C     C           Reference frame:        J2000
C     C           Aberration correction:  Light time and stellar
C     C                                   aberration ('LT+S')
C     C           Observer:               Earth (NAIF ID code 399)
C     C
C     C        Before we can execute this computation, we'll need the
C     C        geometric state and accleration of the observer relative
C     C        to the solar system barycenter at ET, expressed
C     C        relative to the J2000 reference frame. First find 
C     C        the state:
C     C
C              CALL SPKSSB ( 399, ET, 'J2000', STOBS )
C     C
C     C        Next compute the acceleration. We numerically
C     C        differentiate the velocity using a quadratic 
C     C        approximation:
C     C
C              TDELTA = 1.D0
C
C              CALL SPKSSB ( 399, ET-TDELTA, 'J2000', STATE0 )
C              CALL SPKSSB ( 399, ET+TDELTA, 'J2000', STATE2 )
C
C              CALL QDERIV ( 3, STATE0(4), STATE2(4), TDELTA, ACC )
C     C
C     C        Now compute the desired state vector:
C     C
C              CALL SPKAPS ( 301,   ET,  'J2000', 'LT+S',
C          .                 STOBS, ACC, STATE,    LT,   DLT )
C
C              WRITE (*,*) 'ET = ', ET
C              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
C              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
C              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
C              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
C              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
C              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
C              WRITE (*,*) 'One-way light time (s):  ', LT
C              WRITE (*,*) 'Light time rate:         ', DLT
C              WRITE (*,*) ' '
C
C           END DO
C
C           END
C
C
C     The output produced by this program will vary somewhat as 
C     a function of the platform on which the program is built and
C     executed. On a PC/Linux/g77 platform, the following output
C     was produced:
C
C        ET =   0.
C        J2000 x-position (km):    -291584.614
C        J2000 y-position (km):    -266693.406
C        J2000 z-position (km):    -76095.6532
C        J2000 x-velocity (km/s):   0.643439157
C        J2000 y-velocity (km/s):  -0.666065874
C        J2000 z-velocity (km/s):  -0.301310063
C        One-way light time (s):    1.34231061
C        Light time rate:           1.07316909E-07
C
C        ET =   3600.
C        J2000 x-position (km):    -289256.459
C        J2000 y-position (km):    -269080.605
C        J2000 z-position (km):    -77177.3528
C        J2000 x-velocity (km/s):   0.64997032
C        J2000 y-velocity (km/s):  -0.660148253
C        J2000 z-velocity (km/s):  -0.299630418
C        One-way light time (s):    1.34269395
C        Light time rate:           1.05652599E-07
C
C        ET =   7200.
C        J2000 x-position (km):    -286904.897
C        J2000 y-position (km):    -271446.417
C        J2000 z-position (km):    -78252.9655
C        J2000 x-velocity (km/s):   0.656443883
C        J2000 y-velocity (km/s):  -0.654183552
C        J2000 z-velocity (km/s):  -0.297928533
C        One-way light time (s):    1.34307131
C        Light time rate:           1.03990457E-07
C
C        ET =   10800.
C        J2000 x-position (km):    -284530.133
C        J2000 y-position (km):    -273790.671
C        J2000 z-position (km):    -79322.4117
C        J2000 x-velocity (km/s):   0.662859505
C        J2000 y-velocity (km/s):  -0.648172247
C        J2000 z-velocity (km/s):  -0.296204558
C        One-way light time (s):    1.34344269
C        Light time rate:           1.02330665E-07
C
C        ET =   14400.
C        J2000 x-position (km):    -282132.378
C        J2000 y-position (km):    -276113.202
C        J2000 z-position (km):    -80385.612
C        J2000 x-velocity (km/s):   0.669216846
C        J2000 y-velocity (km/s):  -0.642114815
C        J2000 z-velocity (km/s):  -0.294458645
C        One-way light time (s):    1.3438081
C        Light time rate:           1.00673404E-07
C
C
C$ Restrictions
C
C     1) This routine should not be used to compute geometric states.
C        Instead, use SPKEZR, SPKEZ, or SPKGEO. SPKGEO, which is called
C        by SPKEZR and SPKEZ, introduces less round-off error when the
C        observer and target have a common center that is closer to
C        both objects than is the solar system barycenter.
C
C     2) The kernel files to be used by SPKAPS must be loaded
C        (normally by the SPICELIB kernel loader FURNSH) before 
C        this routine is called.
C
C     3) Unlike most other SPK state computation routines, this
C        routine requires that the output state be relative to an
C        inertial reference frame. 
C
C$ Literature_References
C
C     SPK Required Reading.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 04-JUL-2014 (NJB)
C
C        Discussion of light time corrections was updated. Assertions
C        that converged light time corrections are unlikely to be
C        useful were removed.
C
C     Last update was 15-APR-2014 (NJB)
C
C        Added a FAILED() call to prevent numeric problems
C        resulting from uninitialized values.
C
C-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
C
C-&
 
C$ Index_Entries
C
C     low-level aberration-corrected state computation
C     low-level light time and stellar aberration correction
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
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'ZZSPKAS1' )

C
C     Local variables
C
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      CORPOS ( 3 )
      DOUBLE PRECISION      CORVEL ( 3 )
      DOUBLE PRECISION      DPCORR ( 3 )
      DOUBLE PRECISION      PCORR  ( 3 )
 
      INTEGER               REFID

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FIRST
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT
      
C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  PRVCOR
      SAVE                  USELT
      SAVE                  USESTL
      SAVE                  XMIT

C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /
      DATA                  PRVCOR  / ' ' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )


      IF (  FIRST  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
C
C        The aberration correction flag differs from the value it
C        had on the previous call, if any.  Analyze the new flag.
C
         CALL ZZPRSCOR ( ABCORR, ATTBLK )

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
C           USELT is .TRUE. when any type of light time correction
C           (normal or converged Newtonian) is specified.
C
C           USECN indicates converged Newtonian light time correction.
C
C        The above definitions are consistent with those used by
C        ZZPRSCOR.
C 
         XMIT    =  ATTBLK ( XMTIDX )
         USELT   =  ATTBLK ( LTIDX  )
         USESTL  =  ATTBLK ( STLIDX )

         IF (  USESTL  .AND.  ( .NOT. USELT )  ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for '
     .      //            'stellar aberration but not light time '
     .      //            'corrections. This combination is not '
     .      //            'expected.'                              )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         ELSE IF ( ATTBLK(RELIDX) ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for '
     .      //            'relativistic light time correction.'    )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         END IF

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
         CALL CHKOUT ( RNAME                                )
         RETURN
 
      END IF

C
C     Get the state of the target relative to the observer,
C     optionally corrected for light time.
C
      CALL ZZSPKLT1 ( TARG,  ET,    REF, ABCORR, 
     .                STOBS, STARG, LT,  DLT     )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C
C     If stellar aberration corrections are not needed, we're
C     already done.
C
      IF ( .NOT. USESTL ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C
C     Get the stellar aberration correction and its time derivative.
C
      CALL ZZSTELAB ( XMIT, ACCOBS, STOBS(4), STARG, PCORR, DPCORR )

C
C     Adding the stellar aberration correction to the light
C     time-corrected target position yields the position corrected for
C     both light time and stellar aberration.
C
      CALL VADD ( PCORR,  STARG, CORPOS )
      CALL VEQU ( CORPOS, STARG         )

C
C     Velocity is treated in an analogous manner.
C
      CALL VADD ( DPCORR, STARG(4), CORVEL )
      CALL VEQU ( CORVEL, STARG(4)         )

      CALL CHKOUT ( RNAME )
      RETURN
      END
