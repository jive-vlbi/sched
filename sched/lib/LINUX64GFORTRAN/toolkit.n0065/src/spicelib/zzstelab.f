C$Procedure ZZSTELAB ( Private --- stellar aberration correction )
 
      SUBROUTINE ZZSTELAB ( XMIT, ACCOBS, VOBS, STARG, SCORR, DSCORR )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the state (position and velocity) of a target body
C     relative to an observing body, optionally corrected for light
C     time (planetary aberration) and stellar aberration.
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

      LOGICAL               XMIT
      DOUBLE PRECISION      ACCOBS ( 3 )
      DOUBLE PRECISION      VOBS   ( 3 )
      DOUBLE PRECISION      STARG  ( 6 )
      DOUBLE PRECISION      SCORR  ( 3 )
      DOUBLE PRECISION      DSCORR ( 3 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     XMIT       I   Reception/transmission flag.
C     ACCOBS     I   Observer acceleration relative to SSB.
C     VOBS       I   Observer velocity relative to to SSB.
C     STARG      I   State of target relative to observer.
C     SCORR      O   Stellar aberration correction for position.
C     DSCORR     O   Stellar aberration correction for velocity.
C
C$ Detailed_Input
C
C     XMIT        is a logical flag which is set to .TRUE. for the
C                 "transmission" case in which photons *depart* from
C                 the observer's location at an observation epoch ET
C                 and arrive at the target's location at the light-time
C                 corrected epoch ET+LT, where LT is the one-way light
C                 time between observer and target; XMIT is set to
C                 .FALSE. for "reception" case in which photons depart
C                 from the target's location at the light-time
C                 corrected epoch ET-LT and *arrive* at the observer's
C                 location at ET.
C
C                 Note that the observation epoch is not used in this
C                 routine.
C
C                 XMIT must be consistent with any light time
C                 corrections used for the input state STARG: if that
C                 state has been corrected for "reception" light time;
C                 XMIT must be .FALSE.; otherwise XMIT must be .TRUE.
C
C     ACCOBS      is the geometric acceleration of the observer
C                 relative to the solar system barycenter. Units are
C                 km/sec**2. ACCOBS must be expressed relative to
C                 an inertial reference frame.
C
C     VOBS        is the geometric velocity of the observer relative to
C                 the solar system barycenter. VOBS must be expressed
C                 relative to the same inertial reference frame as
C                 ACCOBS. Units are km/sec.
C
C     STARG       is the Cartesian state of the target relative to the
C                 observer. Normally STARG has been corrected for
C                 one-way light time, but this is not required. STARG
C                 must be expressed relative to the same inertial
C                 reference frame as ACCOBS. Components are
C                 (x, y, z, dx, dy, dz). Units are km and km/sec.
C                 
C$ Detailed_Output
C
C     SCORR       is the stellar aberration correction for the position
C                 component of STARG. Adding SCORR to this position
C                 vector produces the input observer-target position,
C                 corrected for stellar aberration.
C
C                 The reference frame of SCORR is the common frame
C                 relative to which the inputs ACCOBS, VOBS, and STARG
C                 are expressed. Units are km.
C
C     DSCORR      is the stellar aberration correction for the velocity
C                 component of STARG. Adding DSCORR to this velocity
C                 vector produces the input observer-target velocity,
C                 corrected for stellar aberration.
C
C                 The reference frame of DSCORR is the common frame
C                 relative to which the inputs ACCOBS, VOBS, and STARG
C                 are expressed. Units are km/s.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If attempt to divide by zero occurs, the error 
C        SPICE(DIVIDEBYZERO) will be signaled. This case may occur
C        due to uninitialized inputs.
C
C     2) Loss of precision will occur for geometric cases in which
C        VOBS is nearly parallel to the position component of STARG.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes a Newtonian estimate of the stellar
C     aberration correction of an input state. Normally the input state
C     has already been corrected for one-way light time.
C
C     Since stellar aberration corrections are typically "small"
C     relative to the magnitude of the input observer-target position
C     and velocity, this routine avoids loss of precision by returning
C     the corrections themselves rather than the corrected state
C     vector. This allows the caller to manipulate (for example,
C     interpolate) the corrections with greater accuracy.
C
C$ Examples
C
C     See SPICELIB routine SPKACS.
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 15-APR-2014 (NJB) 
C
C        Added RETURN test and discovery check-in.
C        Check for division by zero was added. This
C        case might occur due to uninitialized inputs.
C
C-    SPICELIB Version 1.0.1, 12-FEB-2009 (NJB) 
C
C        Minor updates were made to the inline documentation.
C     
C-    SPICELIB Version 1.0.0, 17-JAN-2008 (NJB) 
C
C-&
 
C
C     Note for the maintenance programmer
C     ===================================
C
C     The source code of the test utility T_ZZSTLABN must be 
C     kept in sync with the source code of this routine. That
C     routine uses a value of SEPLIM that forces the numeric
C     branch of the velocity computation to be taken in all
C     cases. See the documentation of that routine for details.
C

C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C
C
C     Let PHI be the (non-negative) rotation angle of the stellar
C     aberration correction; then SEPLIM is a limit on how close PHI
C     may be to zero radians while stellar aberration velocity is
C     computed analytically. When sin(PHI) is less than SEPLIM, the
C     velocity must be computed numerically.
C
      DOUBLE PRECISION      SEPLIM
      PARAMETER           ( SEPLIM = 1.D-6 )

C
C     Let TDELTA be the time interval, measured in seconds,
C     used for numerical differentiation of the stellar 
C     aberration correction, when this is necessary.
C
      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 1.D0 )

C
C     Local variables
C
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DPHI
      DOUBLE PRECISION      DPTMAG
      DOUBLE PRECISION      DRHAT  ( 3 )
      DOUBLE PRECISION      DVP    ( 3 )
      DOUBLE PRECISION      DVPHAT ( 3 )
      DOUBLE PRECISION      EPTARG ( 3 )
      DOUBLE PRECISION      EVOBS  ( 3 )
      DOUBLE PRECISION      LCACC  ( 3 )
      DOUBLE PRECISION      LCVOBS ( 3 )
      DOUBLE PRECISION      PTARG  ( 3 )
      DOUBLE PRECISION      PTGMAG
      DOUBLE PRECISION      RHAT   ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SAOFF  ( 3, 2 )
      DOUBLE PRECISION      SGN
      DOUBLE PRECISION      SRHAT  ( 6 )
      DOUBLE PRECISION      SVP    ( 6 )
      DOUBLE PRECISION      SVPHAT ( 6 )
      DOUBLE PRECISION      TERM1  ( 3 )
      DOUBLE PRECISION      TERM2  ( 3 )
      DOUBLE PRECISION      TERM3  ( 3 )
      DOUBLE PRECISION      VP     ( 3 )
      DOUBLE PRECISION      VPHAT  ( 3 )
      DOUBLE PRECISION      VTARG  ( 3 )

      INTEGER               I

C
C     Use discovery check-in.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     In the discussion below, the dot product of vectors X and Y
C     is denoted by 
C
C        <X,Y>
C
C     The speed of light is denoted by the lower case letter "c." BTW,
C     variable names used here are case-sensitive: upper case "C" 
C     represents a different quantity which is unrelated to the speed
C     of light.
C
C     Variable names ending in "HAT" denote unit vectors. Variable
C     names starting with "D" denote derivatives with respect to time.
C
C     We'll compute the correction SCORR and its derivative with
C     respect to time DSCORR for the reception case. In the
C     transmission case, we perform the same computation with the
C     negatives of the observer velocity and acceleration.
C     
C     In the code below, we'll store the position and velocity portions
C     of the input observer-target state STARG in the variables PTARG
C     and VTARG, respectively.
C
C     Let VP be the component of VOBS orthogonal to PTARG. VP
C     is defined as
C
C         VOBS - < VOBS, RHAT > RHAT                                 (1)
C
C     where RHAT is the unit vector
C
C         PTARG/||PTARG||
C
C     Then
C
C        ||VP||/c                                                    (2)
C
C     is the magnitude of 
C
C        s = sin( phi )                                              (3)
C
C     where phi is the stellar aberration correction angle. We'll
C     need the derivative with respect to time of (2).
C
C     Differentiating (1) with respect to time yields the 
C     velocity DVP, where, letting 
C
C        DRHAT  =  d(RHAT) / dt 
C        VPHAT  =  VP      / ||VP||
C        DVPMAG =  d( ||VP|| ) / dt
C
C     we have
C
C        DVP = d(VP)/dt 
C
C            = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT
C                        +   <VOBS,RHAT>  * DRHAT                 )  (4)
C
C     and
C
C        DVPMAG = < DVP, VPHAT >                                     (5)
C
C     Now we can find the derivative with respect to time of 
C     the stellar aberration angle phi:
C
C        ds/dt = d(sin(phi))/dt = d(phi)/dt * cos(phi)               (6)
C
C     Using (2) and (5), we have for positive phi,
C
C        ds/dt = (1/c)*DVPMAG = (1/c)*<DVP, VPHAT>                   (7)
C
C     Then for positive phi
C
C        d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>             (8)
C
C     Equation (8) is well-defined as along as VP is non-zero:
C     if VP is the zero vector, VPHAT is undefined. We'll treat
C     the singular and near-singular cases separately.
C
C     The aberration correction itself is a rotation by angle phi
C     from RHAT towards VP, so the corrected vector is
C
C        ( sin(phi)*VPHAT + cos(phi)*RHAT ) * ||PTARG||
C
C     and  we can express the offset of the corrected vector from 
C     PTARG, which is the output SCORR, as
C
C        SCORR = 
C
C        ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||          (9)
C
C     Let DPTMAG be defined as
C
C        DPTMAG  =  d ( ||PTARG|| ) / dt                            (10)
C
C     Then the derivative with respect to time of SCORR is
C
C        DSCORR =
C
C             (      sin(phi)*DVPHAT  
C
C                +   cos(phi)*d(phi)/dt * VPHAT
C
C                +  (cos(phi) - 1) * DRHAT
C
C                +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG||
C
C           + ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG       (11)
C
C
C     Computations begin here:
C
C     Split STARG into position and velocity components. Compute 
C
C        RHAT
C        DRHAT
C        VP
C        DPTMAG
C
      IF ( XMIT ) THEN
         CALL VMINUS ( VOBS,   LCVOBS )
         CALL VMINUS ( ACCOBS, LCACC  )
      ELSE
         CALL VEQU   ( VOBS,   LCVOBS )
         CALL VEQU   ( ACCOBS, LCACC  )
      END IF

      CALL VEQU  ( STARG,      PTARG )
      CALL VEQU  ( STARG(4),   VTARG )

      CALL DVHAT ( STARG,      SRHAT )
      CALL VEQU  ( SRHAT,      RHAT  )
      CALL VEQU  ( SRHAT(4),   DRHAT )
 
      CALL VPERP ( LCVOBS, RHAT, VP    )
     
      DPTMAG = VDOT ( VTARG, RHAT )

C
C     Compute sin(phi) and cos(phi), which we'll call S and C
C     respectively. Note that phi is always close to zero for 
C     realistic inputs (for which ||VOBS|| << CLIGHT), so the 
C     cosine term is positive.
C
      S = VNORM(VP) / CLIGHT()

      C = SQRT (  MAX( 0.D0,  1 - S*S )  )

      IF ( C .EQ. 0.D0 ) THEN
C
C        C will be used as a divisor later (in the computation
C        of DPHI), so we'll put a stop to the problem here.
C
         CALL CHKIN  ( 'ZZSTELAB'                               )
         CALL SETMSG ( 'Cosine of the aberration angle is 0; '
     .   //            'this cannot occur for realistic '
     .   //            'observer velocities. This case can '
     .   //            'arise due to uninitialized inputs. '
     .   //            'This cosine value is used as a divisor '
     .   //            'in a later computation, so it must not '
     .   //            'be equal to zero.'                      )
        CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                     )
        CALL CHKOUT ( 'ZZSTELAB'                                )
        RETURN

      END IF

C
C     Compute the unit vector VPHAT and the stellar
C     aberration correction. We avoid relying on 
C     VHAT's exception handling for the zero vector.
C
      IF ( VZERO(VP) ) THEN
         CALL CLEARD ( 3,  VPHAT )
      ELSE
         CALL VHAT   ( VP, VPHAT )
      END IF

C
C     Now we can use equation (9) to obtain the stellar
C     aberration correction SCORR:
C
C        SCORR = 
C
C           ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||       
C
C
      PTGMAG = VNORM( PTARG )

      CALL VLCOM ( PTGMAG * S,           VPHAT, 
     .             PTGMAG * (C - 1.D0),  RHAT,   SCORR )

   
C
C     Now we use S as an estimate of PHI to decide if we're
C     going to differentiate the stellar aberration correction
C     analytically or numerically. 
C
C     Note that S is non-negative by construction, so we don't
C     need to use the absolute value of S here.
C
      IF ( S .GE. SEPLIM ) THEN
C
C        This is the analytic case.
C
C        Compute DVP---the derivative of VP with respect to time.
C        Recall equation (4):
C
C        DVP = d(VP)/dt 
C
C            = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT
C                        +   <VOBS,RHAT>  * DRHAT                 )  
C
         CALL VLCOM3 ( 1.D0,                                      LCACC,
     .                 -VDOT(LCVOBS, DRHAT) - VDOT(LCACC, RHAT),  RHAT,
     .                 -VDOT(LCVOBS, RHAT ),                      DRHAT,
     .                 DVP                                             )
        
         CALL VHAT ( VP, VPHAT )

C
C        Now we can compute DVPHAT, the derivative of VPHAT:
C
         CALL VEQU  ( VP,        SVP    )
         CALL VEQU  ( DVP,       SVP(4) )

         CALL DVHAT ( SVP,       SVPHAT )
         CALL VEQU  ( SVPHAT(4), DVPHAT )

C
C        Compute the DPHI, the time derivative of PHI, using equation 8:
C    
C           d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>             
C
C
         DPHI = ( 1.D0 / ( C * CLIGHT() ) ) * VDOT( DVP, VPHAT )

C
C        At long last we've assembled all of the "ingredients" required
C        to compute DSCORR:
C
C           DSCORR =
C
C             (     sin(phi)*DVPHAT  
C
C                +  cos(phi)*d(phi)/dt * VPHAT
C
C                +  (cos(phi) - 1) * DRHAT
C
C                +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG||
C
C                +  ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG  
C
C
         CALL VLCOM  ( S,             DVPHAT,
     .                 C * DPHI,      VPHAT,  TERM1 )

         CALL VLCOM  ( C - 1.D0,      DRHAT, 
     .                 (-S) * DPHI,   RHAT,   TERM2 )

         CALL VADD   ( TERM1,  TERM2, TERM3 )

         CALL VLCOM3 ( PTGMAG,                TERM3,
     .                 DPTMAG * S,            VPHAT,
     .                 DPTMAG * (C - 1.D0),   RHAT,    DSCORR )


      ELSE
C
C        This is the numeric case. We're going to differentiate
C        the stellar aberration correction offset vector using
C        a quadratic estimate.
C
         DO I = 1, 2 
C
C           Set the sign of the time offset.
C           
            IF ( I .EQ. 1 ) THEN
               SGN = -1.D0
            ELSE
               SGN =  1.D0
            END IF

C
C           Estimate the observer's velocity relative to the
C           solar system barycenter at the current epoch. We use
C           the local copies of the input velocity and acceleration 
C           to make a linear estimate.
C            
            CALL VLCOM ( 1.D0,        LCVOBS, 
     .                   SGN*TDELTA,  LCACC,     EVOBS )

C
C           Estimate the observer-target vector. We use the
C           observer-target state velocity to make a linear estimate.
C
            CALL VLCOM ( 1.D0,        STARG, 
     .                   SGN*TDELTA,  STARG(4),  EPTARG )

C
C           Let RHAT be the unit observer-target position.
C           Compute the component of the observer's velocity
C           that is perpendicular to the target position; call
C           this vector VP. Also compute the unit vector in
C           the direction of VP.
C
            CALL VHAT  ( EPTARG,  RHAT     )
            CALL VPERP ( EVOBS,   RHAT, VP )

            IF ( VZERO(VP) ) THEN
               CALL CLEARD ( 3,  VPHAT )
            ELSE
               CALL VHAT   ( VP, VPHAT )
            END IF

C
C           Compute the sine and cosine of the correction
C           angle.
C
            S = VNORM(VP) / CLIGHT()

            C = SQRT (  MAX( 0.D0,  1 - S*S )  )     

C
C           Compute the vector offset of the correction.
C
            PTGMAG = VNORM( EPTARG )

            CALL VLCOM ( PTGMAG * S,           VPHAT, 
     .                   PTGMAG * (C - 1.D0),  RHAT,   SAOFF(1,I) )
         END DO

C
C        Now compute the derivative.
C
         CALL QDERIV ( 3, SAOFF(1,1), SAOFF(1,2), TDELTA, DSCORR ) 

      END IF

C
C     At this point the correction offset SCORR and its derivative
C     with respect to time DSCORR are both set.
C
      END

