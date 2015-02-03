C$Procedure ZZSPKFAP ( SPK function, apparent inertial state )
 
      SUBROUTINE ZZSPKFAP ( TRGSUB, ET,     REF,   ABCORR, 
     .                      STOBS,  ACCOBS, STARG, LT,     DLT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Given the state and acceleration of an observer relative to the
C     solar system barycenter, return the state (position and velocity)
C     of a target body relative to the observer, optionally corrected
C     for light time and stellar aberration. All input and output
C     vectors are expressed relative to an inertial reference frame. An
C     input subroutine provides the state of the target relative to its
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
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'
 
      EXTERNAL              TRGSUB
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
C     TRGSUB     I   Target body state subroutine.
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
C     TRGSUB      is the name of an external subroutine that returns
C                 the geometric state of the target body relative to a
C                 center of motion, expressed in the inertial reference
C                 frame REF, at the epoch ET.
C
C                 The calling sequence of TRGSUB is
C
C                    SUBROUTINE TRGSUB ( ET, REF, TRGCTR, STATE )
C
C                    DOUBLE PRECISION      ET
C                    CHARACTER*(*)         REF
C                    INTEGER               TRGCTR
C                    DOUBLE PRECISION      STATE ( 6 )
C
C
C                    The inputs of TRGSUB are ET and REF; the outputs
C                    are TRGCTR and STATE. STATE is the geometric state
C                    of the target relative to the returned center of
C                    motion at ET, expressed in the frame REF.
C
C                 The target and observer define a state vector whose
C                 position component points from the observer to the
C                 target.
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
C     2) If the reference frame requested is not a recognized
C        inertial reference frame, the error SPICE(BADFRAME)
C        is signaled.
C
C     3) If the state of the target relative to the solar system
C        barycenter cannot be computed, the error will be diagnosed 
C        by routines in the call tree of this routine.
C
C     4) If the observer and target are at the same position,
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
C     This routine supports higher-level routines that can
C     perform both light time and stellar aberration corrections
C     and that use target states provided by subroutines rather
C     than by the conventional, public SPK APIs. For example, this
C     routine can be used for objects having fixed positions
C     on the surfaces of planets.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) This routine must not be called by routines of the SPICE
C        frame subsystem. It must not be called by any portion of
C        the SPK subsystem other than the private SPK function-based
C        component.
C
C     2) The input subroutine TRGSUB must not call this routine
C        or any of the supporting, private SPK routines
C
C           ZZSPKFAT
C           ZZSPKFAO
C           ZZSPKFZO
C           ZZSPKFZT
C
C     3)  When possible, the routine SPKGEO should be used instead of
C         this routine to compute geometric states. SPKGEO introduces
C         less round-off error when the observer and target have common
C         center that is closer to both objects than is the solar
C         system barycenter.
C
C     4)  Unlike most other SPK state computation routines, this
C         routine requires that the output state be relative to an
C         inertial reference frame.
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
C-    SPICELIB Version 1.0.0, 04-JUL-2014 (NJB)
C
C        Discussion of light time corrections was updated. Assertions
C        that converged light time corrections are unlikely to be
C        useful were removed.
C
C-    SPICELIB Version 1.0.0, 09-JAN-2011 (NJB)
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
      PARAMETER           ( RNAME  = 'ZZSPKFAP' )

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
      LOGICAL               PASS1
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT
      
C
C     Saved variables
C
      SAVE                  PASS1
      SAVE                  PRVCOR
      SAVE                  USELT
      SAVE                  USESTL
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
      END IF

      CALL CHKIN ( RNAME )


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
         USELT   =  ATTBLK ( LTIDX  )
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
C     Get the state of the target relative to the observer,
C     optionally corrected for light time.
C
      CALL ZZSPKFLT ( TRGSUB, ET,    REF, ABCORR, 
     .                STOBS,  STARG, LT,  DLT     )

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
