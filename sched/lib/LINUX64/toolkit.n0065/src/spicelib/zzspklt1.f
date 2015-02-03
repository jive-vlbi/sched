C$Procedure ZZSPKLT1 ( S/P Kernel, light time corrected state )
 
      SUBROUTINE ZZSPKLT1 ( TARG,  ET,    REF, ABCORR, 
     .                      STOBS, STARG, LT,  DLT    )
 
C$ Abstract
C
C     Return the state (position and velocity) of a target body
C     relative to an observer, optionally corrected for light time,
C     expressed relative to an inertial reference frame.
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
C                 relative to the observer is to be computed. ET
C                 refers to time at the observer's location.
C
C     REF         is the inertial reference frame with respect to which
C                 the input state STOBS and the output state STARG are
C                 expressed. REF must be recognized by the SPICE
C                 Toolkit. The acceptable frames are listed in the
C                 Frames Required Reading, as well as in the SPICELIB
C                 routine CHGIRF.
C
C                 Case and blanks are not significant in the string
C                 REF.
C 
C
C     ABCORR      indicates the aberration corrections to be applied to
C                 the state of the target body to account for one-way
C                 light time. See the discussion in the Particulars
C                 section for recommendations on how to choose
C                 aberration corrections.
C                  
C                 If ABCORR includes the stellar aberration correction
C                 symbol '+S', this flag is simply ignored. Aside from
C                 the possible presence of this symbol, ABCORR may be
C                 any of the following:
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
C                               The light time correction involves
C                               iterative solution of the light time
C                               equation (see Particulars for details).
C                               The solution invoked by the 'LT' option
C                               uses one iteration.
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
C                    'XCN'      "Transmission" case:  converged 
C                               Newtonian light time correction.
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
C     STOBS       is the geometric (uncorrected) state of the observer
C                 relative to the solar system barycenter at epoch ET.
C                 STOBS is a 6-vector: the first three components of
C                 STOBS represent a Cartesian position vector; the last
C                 three components represent the corresponding velocity
C                 vector. STOBS is expressed relative to the inertial
C                 reference frame designated by REF.
C
C                 Units are always km and km/sec.
C                                 
C$ Detailed_Output
C
C     STARG       is a Cartesian state vector representing the position
C                 and velocity of the target body relative to the
C                 specified observer. STARG is corrected for the
C                 specified aberration, and is expressed with respect
C                 to the specified inertial reference frame.  The first
C                 three components of STARG represent the x-, y- and
C                 z-components of the target's position; last three
C                 components form the corresponding velocity vector.
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
C     1) For the convenience of the caller, the input aberration
C        correction flag can call for stellar aberration correction via
C        inclusion of the '+S' suffix. This portion of the aberration
C        correction flag is ignored if present.
C
C     2) If the value of ABCORR is not recognized, the error
C        is diagnosed by a routine in the call tree of this
C        routine.
C
C     3) If the reference frame requested is not a recognized
C        inertial reference frame, the error SPICE(BADFRAME)
C        is signaled.
C
C     4) If the state of the target relative to the solar system
C        barycenter cannot be computed, the error will be diagnosed 
C        by routines in the call tree of this routine.
C
C     5) If the observer and target are at the same position,
C        then DLT is set to zero. This situation could arise,
C        for example, when the observer is Mars and the target
C        is the Mars barycenter.
C
C     6) If a division by zero error would occur in the computation
C        of DLT, the error SPICE(DIVIDEBYZERO) is signaled.
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
C     these additional kernels are PCK files or frame kernels. Any
C     such kernels must already be loaded at the time this routine is
C     called.
C
C$ Particulars
C
C     This routine supports higher-level SPK API routines that can
C     perform both light time and stellar aberration corrections.
C     User applications normally will not need to call this routine
C     directly.
C
C     See the header of the routine SPKEZR for a detailed discussion
C     of aberration corrections.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C    1) Look up a sequence of states of the Moon as seen from the
C       Earth. Use light time corrections. Compute the first state for
C       the epoch 2000 JAN 1 12:00:00 TDB; compute subsequent states at
C       intervals of 1 hour. For each epoch, display the states, the
C       one way light time between target and observer, and the rate of
C       change of the one way light time.
C
C       Use the following meta-kernel to specify the kernels to 
C       load:
C
C          KPL/MK
C
C          File name: spkltc.tm
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
C             KERNELS_TO_LOAD = ( 'de421.bsp', 
C                                 'pck00010.tpc',
C                                 'naif0010.tls'  )
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
C           PARAMETER           ( META   = 'spkltc.tm' )
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
C           DOUBLE PRECISION      DLT
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      STATE ( 6 )
C           DOUBLE PRECISION      STOBS ( 6 )
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
C     C           Aberration correction:  Light time ('LT')
C     C           Observer:               Earth (NAIF ID code 399)
C     C
C     C        Before we can execute this computation, we'll need the
C     C        geometric state of the observer relative to the solar
C     C        system barycenter at ET, expressed relative to the
C     C        J2000 reference frame:
C     C
C              CALL SPKSSB ( 399, ET,    'J2000', STOBS )
C     C
C     C        Now compute the desired state vector:
C     C
C              CALL SPKLTC ( 301,   ET,    'J2000', 'LT',
C          .                 STOBS, STATE, LT,      DLT     )
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
C     On a PC/Linux/gfortran platform, the following output was
C     produced:
C
C
C        ET =    0.0000000000000000
C        J2000 x-position (km):     -291569.26541282982
C        J2000 y-position (km):     -266709.18647825718
C        J2000 z-position (km):     -76099.155118763447
C        J2000 x-velocity (km/s):   0.64353061322177041
C        J2000 y-velocity (km/s):  -0.66608181700820079
C        J2000 z-velocity (km/s):  -0.30132283179625752
C        One-way light time (s):     1.3423106103251679
C        Light time rate:           1.07316908698977495E-007
C
C        ET =    3600.0000000000000
C        J2000 x-position (km):     -289240.78128184378
C        J2000 y-position (km):     -269096.44087958336
C        J2000 z-position (km):     -77180.899725757539
C        J2000 x-velocity (km/s):   0.65006211520087476
C        J2000 y-velocity (km/s):  -0.66016273921695667
C        J2000 z-velocity (km/s):  -0.29964267390571342
C        One-way light time (s):     1.3426939548635302
C        Light time rate:           1.05652598952224259E-007
C
C        ET =    7200.0000000000000
C        J2000 x-position (km):     -286888.88736709207
C        J2000 y-position (km):     -271462.30170547962
C        J2000 z-position (km):     -78256.555682137609
C        J2000 x-velocity (km/s):   0.65653599154284592
C        J2000 y-velocity (km/s):  -0.65419657680401588
C        J2000 z-velocity (km/s):  -0.29794027307420823
C        One-way light time (s):     1.3430713117337547
C        Light time rate:           1.03990456898758609E-007
C
C        ET =    10800.000000000000
C        J2000 x-position (km):     -284513.79173691198
C        J2000 y-position (km):     -273806.60031034052
C        J2000 z-position (km):     -79326.043183274567
C        J2000 x-velocity (km/s):   0.66295190054599118
C        J2000 y-velocity (km/s):  -0.64818380709706158
C        J2000 z-velocity (km/s):  -0.29621577937090349
C        One-way light time (s):     1.3434426890693671
C        Light time rate:           1.02330665243423737E-007
C
C        ET =    14400.000000000000
C        J2000 x-position (km):     -282115.70368389413
C        J2000 y-position (km):     -276129.16976799071
C        J2000 z-position (km):     -80389.282965712249
C        J2000 x-velocity (km/s):   0.66930950377548726
C        J2000 y-velocity (km/s):  -0.64212490805688027
C        J2000 z-velocity (km/s):  -0.29446934336246899
C        One-way light time (s):     1.3438080956559786
C        Light time rate:           1.00673403630050830E-007
C
C
C$ Restrictions
C
C     1) The routine SPKGEO should be used instead of this routine
C        to compute geometric states. SPKGEO introduces less
C        round-off error when the observer and target have common 
C        center that is closer to both objects than is the solar
C        system barycenter.
C
C     2) The kernel files to be used by SPKLTC must be loaded
C        (normally by the SPICELIB kernel loader FURNSH) before 
C        this routine is called.
C
C     3) Unlike most other SPK state computation routines, this
C        routine requires that the output state be relative to an
C        inertial reference frame. 
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 04-JUL-2014 (NJB)
C
C        Discussion of light time corrections was updated. Assertions
C        that converged light time corrections are unlikely to be
C        useful were removed.
C
C     Last update was 02-MAY-2012 (NJB)
C
C        Updated to ensure convergence when CN or XCN light time
C        corrections are used. The new algorithm also terminates early
C        (after fewer than three iterations) when convergence is
C        attained.
C
C        Call to ZZPRSCOR was replaced by a call to ZZVALCOR.
C
C-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
C
C-&
 
C$ Index_Entries
C
C     low-level light time correction
C     light-time corrected state from spk file
C     get light-time corrected state
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
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM


      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'ZZSPKLT1' )

C
C     TOL is the tolerance used for a division-by-zero test
C     performed prior to computation of DLT.
C
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL    = 1.D-10 )

C
C     Convergence limit:
C
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-17 )

C
C     Maximum number of light time iterations for any
C     aberration correction:
C
      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 5 )
      
      INTEGER               SSB
      PARAMETER           ( SSB   = 0 )

C
C     Local variables
C
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      LTERR
      DOUBLE PRECISION      PRVLT
      DOUBLE PRECISION      SSBLT
      DOUBLE PRECISION      SSBTRG ( 6 )
 
      INTEGER               I
      INTEGER               LTSIGN
      INTEGER               NUMITR
      INTEGER               REFID
 
      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               PASS1
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT
      
C
C     Saved variables
C
      SAVE                  PASS1
      SAVE                  PRVCOR
      SAVE                  USECN
      SAVE                  USELT
      SAVE                  XMIT

C
C     Initial values
C
      DATA                  PASS1   / .TRUE. /
      DATA                  PRVCOR  / ' ' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( RNAME )
      END IF


      IF (  PASS1  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
C
C        The aberration correction flag differs from the value it
C        had on the previous call, if any.  Analyze the new flag.
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
C           USELT is .TRUE. when any type of light time correction
C           (normal or converged Newtonian) is specified.
C
C           USECN indicates converged Newtonian light time correction.
C
C        The above definitions are consistent with those used by
C        ZZVALCOR.
C 
         XMIT    =  ATTBLK ( XMTIDX )
         USELT   =  ATTBLK ( LTIDX )
         USECN   =  ATTBLK ( CNVIDX )
         USESTL  =  ATTBLK ( STLIDX )

         PASS1   = .FALSE.

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
C     Find the geometric state of the target body with respect to
C     the solar system barycenter. Subtract the state of the
C     observer to get the relative state. Use this to compute the
C     one-way light time.
C  
      CALL ZZSPKGO1 ( TARG,   ET,    REF, SSB,  SSBTRG, SSBLT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      CALL VSUBG  ( SSBTRG, STOBS, 6,   STARG  )

      DIST = VNORM(STARG)
      LT   = DIST / CLIGHT()

      IF ( LT .EQ. 0.D0 ) THEN
C
C        This can happen only if the observer and target are at the
C        same position. We don't consider this an error, but we're not
C        going to compute the light time derivative.
C
         DLT = 0

         CALL CHKOUT ( RNAME ) 
         RETURN

      END IF


      IF ( .NOT. USELT ) THEN
C
C        This is a special case: we're not using light time
C        corrections, so the derivative
C        of light time is just
C
C           (1/c) * d(VNORM(STARG))/dt
C
         DLT = VDOT( STARG, STARG(4) ) / ( DIST * CLIGHT() )

C
C        LT and DLT are both set, so we can return.
C  
         CALL CHKOUT ( RNAME )
         RETURN

      END IF

C
C     To correct for light time, find the state of the target body
C     at the current epoch minus the one-way light time. Note that
C     the observer remains where it is.
C
C     Determine the sign of the light time offset.
C
      IF ( XMIT ) THEN
         LTSIGN =  1
      ELSE
         LTSIGN = -1
      END IF

C
C     Let NUMITR be the number of iterations we'll perform to
C     compute the light time.
C
      IF ( USECN ) THEN
         NUMITR = MAXITR
      ELSE
         NUMITR = 1
      END IF
 
      I     = 0
      LTERR = 1.D0

      DO WHILE (  ( I .LT. NUMITR ) .AND. ( LTERR .GT. CNVLIM )  )
C
C        LT was set either prior to this loop or
C        during the previous loop iteration.
C
         EPOCH  =  ET + ( LTSIGN * LT )

         CALL ZZSPKGO1 ( TARG, EPOCH, REF, SSB, SSBTRG, SSBLT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL VSUBG  ( SSBTRG, STOBS, 6, STARG )

         PRVLT = LT
         LT    = TOUCHD(  VNORM( STARG ) / CLIGHT()  )

C        LTERR is the magnitude of the change between the current
C        estimate of light time and the previous estimate, relative to
C        the previous light time corrected epoch.
C 
         LTERR = TOUCHD(    ABS( LT  - PRVLT      ) 
     .                    / MAX( 1.D0, ABS(EPOCH) )  )
         I     = I + 1

      END DO

C
C     At this point, STARG contains the light time corrected 
C     state of the target relative to the observer.
C
C     Compute the derivative of light time with respect
C     to time: dLT/dt.  Below we derive the formula for 
C     this quantity for the reception case. Let
C
C        POBS be the position of the observer relative to the
C        solar system barycenter.
C
C        VOBS be the velocity of the observer relative to the
C        solar system barycenter.
C
C        PTARG be the position of the target relative to the
C        solar system barycenter.
C
C        VTARG be the velocity of the target relative to the
C        solar system barycenter.
C
C        S be the sign of the light time correction. S is 
C        negative for the reception case.
C
C     The light-time corrected position of the target relative to
C     the observer at observation time ET, given the one-way
C     light time LT is:
C
C         PTARG(ET+S*LT) - POBS(ET)
C
C     The light-time corrected velocity of the target relative to
C     the observer at observation time ET is
C
C         VTARG(ET+S*LT)*( 1 + S*d(LT)/d(ET) ) - VOBS(ET)
C
C     We need to compute dLT/dt. Below, we use the facts that,
C     for a time-dependent vector X(t),
C
C          ||X||     = <X,X> ** (1/2)
C
C        d(||X||)/dt = (1/2)<X,X>**(-1/2) * 2 * <X,dX/dt>
C
C                    = <X,X>**(-1/2) *  <X,dX/dt>
C
C                    = <X,dX/dt> / ||X||
C
C     Newtonian light time equation:
C
C        LT     =   (1/c) * || PTARG(ET+S*LT) - POBS(ET)||
C
C     Differentiate both sides:
C
C        dLT/dt =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
C
C                  * < PTARG(ET+S*LT) - POBS(ET),
C                      VTARG(ET+S*LT)*(1+S*d(LT)/d(ET)) - VOBS(ET) >
C
C
C               = (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
C
C                 * (  < PTARG(ET+S*LT) - POBS(ET),
C                        VTARG(ET+S*LT) - VOBS(ET) >
C
C                   +  < PTARG(ET+S*LT) - POBS(ET),
C                        VTARG(ET+S*LT)           > * (S*d(LT)/d(ET))  )
C
C     Let
C
C        A =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
C
C        B =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) - VOBS(ET) >
C
C        C =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) >
C
C     Then
C
C        d(LT)/d(ET) =  A * ( B  +  C * S*d(LT)/d(ET) )
C
C     which implies
C
C        d(LT)/d(ET) =  A*B / ( 1 - S*C*A )
C
C
C     
      A   =  1.D0 / ( CLIGHT()*VNORM(STARG) )

      B   =  VDOT ( STARG, STARG(4)  )

      C   =  VDOT ( STARG, SSBTRG(4) )

C
C     For physically realistic target velocities, S*C*A cannot equal 1.
C     We'll check for this case anyway.
C
      IF (  LTSIGN*C*A  .GT.  1.D0-TOL ) THEN

         CALL SETMSG ( 'Target range rate magnitude is '
     .//               'approximately the speed of light. The '
     .//               'light time derivative cannot be '
     .//               'computed.'                            )
         CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                  )
         CALL CHKOUT ( RNAME                                  )
         RETURN
         
      END IF

C
C     Compute DLT: the rate of change of light time.
C
      DLT = (A*B)/( 1.D0 - LTSIGN*C*A )

C
C     Overwrite the velocity portion of the output state
C     with the light-time corrected velocity.
C   
      CALL VLCOM ( 1.D0 + LTSIGN*DLT, SSBTRG(4), 
     .            -1.D0,              STOBS(4),   STARG(4) )


      CALL CHKOUT ( RNAME )
      RETURN
      END
