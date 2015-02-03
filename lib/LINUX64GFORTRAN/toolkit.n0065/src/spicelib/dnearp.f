C$Procedure      DNEARP ( Derivative of near point )
 
      SUBROUTINE DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )
 
C$ Abstract
C
C     Compute the ellipsoid surface point nearest to a specified
C     position; also compute the velocity of this point.
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
C     None.
C
C$ Keywords
C
C     ELLIPSOID, GEOMETRY, DERIVATIVE
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      STATE ( 6 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DNEAR ( 6 )
      DOUBLE PRECISION      DALT  ( 2 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STATE      I   State of an object in body-fixed coordinates.
C     A          I   Length of semi-axis parallel to x-axis.
C     B          I   Length of semi-axis parallel to y-axis.
C     C          I   Length on semi-axis parallel to z-axis.
C     DNEAR      O   State of the nearest point on the ellipsoid.
C     DALT       O   Altitude and derivative of altitude.
C     FOUND      O   Tells whether DNEAR is degenerate.
C
C$ Detailed_Input
C
C     STATE      is a 6-vector giving the position and velocity of
C                some object in the body-fixed coordinates of the
C                ellipsoid.
C
C                In body-fixed coordinates, the semi-axes of the
C                ellipsoid are aligned with the x, y, and z-axes of the
C                coordinate system.
C
C     A          is the length of the semi-axis of the ellipsoid
C                that is parallel to the x-axis of the body-fixed
C                coordinate system.
C
C     B          is the length of the semi-axis of the ellipsoid
C                that is parallel to the y-axis of the body-fixed
C                coordinate system.
C
C     C          is the length of the semi-axis of the ellipsoid
C                that is parallel to the z-axis of the body-fixed
C                coordinate system.
C
C$ Detailed_Output
C
C
C     DNEAR      is the 6-vector giving the position and velocity
C                in body-fixed coordinates of the point on the
C                ellipsoid, closest to the object whose position
C                and velocity are represented by STATE.
C
C                While the position component of DNEAR is always
C                meaningful, the velocity component of DNEAR will be
C                meaningless if FOUND if .FALSE.  (See the discussion
C                of the meaning of FOUND below.)
C
C
C     DALT       is an array of two double precision numbers.  The
C                first gives the altitude of STATE with respect to
C                the ellipsoid.  The second gives the rate of
C                change of the altitude.
C
C                Note that the rate of change of altitude is meaningful
C                if and only if FOUND is .TRUE.  (See the discussion of
C                the meaning of FOUND below.)
C
C     FOUND      is a logical flag indicating whether or not the
C                velocity portion of DNEAR is meaningful.
C                If the velocity portion of DNEAR is meaningful
C                FOUND will be returned with a value of .TRUE.
C                Under very rare circumstance the velocity of the
C                near point is undefined.  Under these circumstances
C                FOUND will be returned with the value .FALSE.
C
C                FOUND can be .FALSE. only for states whose position
C                components are inside the ellipsoid and then only at
C                points on a special surface contained inside the
C                ellipsoid called the focal set of the ellipsoid.
C
C                A point in the interior is on this special surface
C                only if there are two or more points on the ellipsoid
C                that are closest to it.  The origin is such a point
C                and the only such point if the ellipsoid is a
C                sphere.  For non-spheroidal ellipsoids the focal
C                set contains small portions of the planes of
C                symmetry of the ellipsoid.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C
C     1) If the axes are non-positive, a routine in the call tree
C        of this routine will diagnose the error.
C
C     2) If an object is passing through the interior of an ellipsoid
C        there are points at which there is more than 1 point on
C        the ellipsoid that is closest to the object.  At these
C        points the velocity of the near point is undefined. (See
C        the description of the output variable FOUND).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If an object is moving relative to some triaxial body along
C     a trajectory C(t) then there is a companion trajectory N(t)
C     that gives the point on the ellipsoid that is closest to
C     C(t) as a function of t.  The instantaneous position and
C     velocity of C(t) (STATE) are sufficient to compute the
C     instantaneous position and velocity of N(t) (DNEAR).
C
C     This routine computes DNEAR from STATE.  In addition it returns
C     the altitude and rate of change of altitude.
C
C     Note that this routine can compute DNEAR for STATES outside,
C     on, or inside the ellipsoid.  However, the velocity of DNEAR
C     and derivative of altitude do not exist for a "small" set
C     of STATES  in the interior of the ellipsoid. See the
C     discussion of FOUND above for a description of this set of
C     points.
C
C$ Examples
C
C     Example 1.  Speed of a ground track.
C     =======================================
C
C     Suppose you wish to compute the velocity of the ground track
C     of a satellite as it passes over a location on the earth
C     and that the moment of passage (ET) has been previously
C     determined.  (We assume that the spacecraft is close enough
C     to the surface that light time corrections do not matter.)
C
C     We let
C
C        BODY    be the idcode for the body
C        FRAME   be the string representing the body's body-fixed frame
C        SCID    be the idcode of the spacecraft
C
C     First get the axes of the body.
C
C        CALL BODVCD ( BODY, 'RADII', 3, DIM, ABC  )
C
C        A = ABC(1)
C        B = ABC(2)
C        C = ABC(3)
C
C        CALL SPKEZ  ( SCID,  ET,   FRAME,   'NONE', BODY, STATE, LT )
C        CALL DNEARP ( STATE, A, B, C, DNEAR, DALT )
C
C     DNEAR contains the state of the subspacecraft point.
C
C
C     Example 2. Doppler shift of an altimeter.
C     =========================================
C
C     Suppose you wish to compute the one-way doppler shift of a radar
C     altimeter mounted on board a spacecraft as it passes
C     over some region.  Moreover, assume that for your
C     purposes it is sufficient to neglect effects of atmosphere,
C     topography and antenna pattern for the sake of this
C     computation.  We use the same notation as in the previous example.
C
C     First get the axes of the body.
C
C        CALL BODVCD ( BODY, 'RADII', 3, DIM, ABC  )
C
C        A = ABC(1)
C        B = ABC(2)
C        C = ABC(3)
C
C        CALL SPKEZ  ( SCID,  ET,   FRAME,   'NONE', BODY, STATE, LT )
C        CALL DNEARP ( STATE, A, B, C, DNEAR, DALT )
C
C
C     The change in frequency is given by multiplying SHIFT times the
C     carrier frequency
C
C        SHIFT = ( DALT(2) / CLIGHT() )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 26-JUN-2008 (NJB)
C
C        Corrected spelling error in abstract; re-wrote
C        abstract text.
C
C-    SPICELIB Version 1.1.1, 24-OCT-2005 (NJB)
C
C        Header update:  changed references to BODVAR to references
C        to BODVCD.
C
C-    SPICELIB Version 1.1.0, 05-MAR-1998 (WLT)
C
C        In the previous version of the routine FOUND could be
C        returned without being set to TRUE when the velocity
C        of the near point and rate of change of altitude
C        could be determined.  This error has been corrected.
C
C-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
C
C
C-&
 
 
 
C$ Index_Entries
C
C     Velocity of the nearest point on an ellipsoid
C     Rate of change of the altitude over an ellipsoid
C     Derivative of altitude over an ellipoid
C     Velocity of a ground track
C
C-&
C
C     Spicelib functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
 
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VTMV
 
C
C     Local Variables
C
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DTERM  ( 3 )
      DOUBLE PRECISION      GRAD   ( 3 )
      DOUBLE PRECISION      L
      DOUBLE PRECISION      LENGTH
      DOUBLE PRECISION      LPRIME
      DOUBLE PRECISION      NORML  ( 3 )
      DOUBLE PRECISION      TEMP   ( 3 )
      DOUBLE PRECISION      ZENITH ( 3 )
 
 
      DOUBLE PRECISION      GRADM  ( 3, 3 )
      DOUBLE PRECISION      M      ( 3, 3 )
 
      INTEGER               I
C
C     Saved Variables
C
      SAVE                  GRADM
      SAVE                  M
 
C
C     Initial Values
C
      DATA   GRADM / 1.0D0, 0.0D0, 0.0D0,
     .               0.0D0, 1.0D0, 0.0D0,
     .               0.0D0, 0.0D0, 1.0D0  /
 
      DATA   M     / 1.0D0, 0.0D0, 0.0D0,
     .               0.0D0, 1.0D0, 0.0D0,
     .               0.0D0, 0.0D0, 1.0D0  /
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DNEARP' )
C
C     Until we have reason to believe otherwise, we set FOUND to TRUE.
C
      FOUND = .TRUE.
 
C
C     First we need to compute the near point.
C
      CALL NEARPT ( STATE, A, B, C, DNEAR, DALT )
 
C
C     Make sure nothing went bump in the dark innards of NEARPT.
C
      IF ( FAILED() ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'DNEARP' )
         RETURN
      END IF
 
C
C     Now for the work of this routine.  We need to compute the
C     velocity component of DNEAR.
C
C     In all of the discussions below we let <,> stand for the
C     dot product.
C
C     Let P be the position (first three components) of STATE
C     and let N be the position (first three components) of DNEAR.
C
C     The surface of the ellipsoid is described as the level set
C     f(x,y,z) = 1 for the function f defined by
C
C         f(x,y,z) = x**2/a**2 + y**2/b**2 + z**2/c**2
C
C     Let GRAD be the "half" gradiant of f. Then for some L
C
C
C           N + L * GRAD = P                         ( 1 )
C
C
C     So that
C                < P - N, GRAD >
C           L =  --------------
C                < GRAD , GRAD >
C
C                          GRAD
C             =  < P - N, ------ >  /  | GRAD |
C                         |GRAD|
C
C     Since GRAD is computed at a point on the level set f(x,y,z) = 1
C     we don't have to worry about the magnitude of |GRAD| being
C     so small that underflow can occur.
C
C     Note that the half gradiant of f  can be computed by simple
C     vector multiplication
C
C                       [ 1/A**2    0       0    ] [ x ]
C        GRAD(x,y,z)  = |   0     1/B**2    0    | | y |
C                       [   0       0     1/C**2 ] [ z ]
C
C     We call the matrix above GRADM.  The correct off
C     diagonal values have been established in the data statement
C     following the declaration section of this routine.
C
 
      GRADM(1,1) = 1.0D0/(A*A)
      GRADM(2,2) = 1.0D0/(B*B)
      GRADM(3,3) = 1.0D0/(C*C)
 
      CALL VSUB  ( STATE,  DNEAR, ZENITH )
 
      CALL MXV   ( GRADM,  DNEAR,    GRAD   )
      CALL UNORM ( GRAD,   NORML,    LENGTH )
 
      L  = VDOT  ( ZENITH, NORML ) / LENGTH
 
 
C
C     We can rewrite equation (1) as
C
C        P = N + L * GRADM * N
C
C     from this it follows that
C
C        P' =  N' + L' * GRADM * N
C                 + L  * GRADM * N'
C
C           = ( IDENT + L*GRADM ) * N'   + L' * GRADM * N
C
C           = ( IDENT + L*GRADM ) * N'   + L' * GRAD
C
C     where IDENT is the 3x3 identity matrix.
C
C     Let M be the inverse of the matrix IDENT + L*GRADM. (Provided
C     of course that all of the diagonal entries are non-zero).
C
C     If we multiply both sides of the equation above by M
C     we have
C
C
C        M*P'  = N'  + L'* M * GRAD                      ( 2 )
C
C
C     Recall now that N' is orthogonal to GRAD (N' lies in the
C     tangent plane to the ellipsoid at N and GRAD is normal
C     to this tangent plane).  Thus
C
C        < GRAD, M*P' > = L' < GRAD, M * GRAD >
C
C     and
C
C                 < GRAD, M*P'   >
C        L'   =   -----------------
C                 < GRAD, M*GRAD >
C
C
C             =   VTMV ( GRAD, M, P' ) / VTMV ( GRAD, M, GRAD )
C
C     Let's pause now to compute M and L'.
C
C        This is where things could go bad.  M might not exist (which
C        indicates STATE is on the focal set of the ellipsoid).  In
C        addition it is conceivable that VTMV ( GRAD, M, GRAD ) is
C        zero.  This turns out not to be possible.  However, the
C        demonstration of this fact requires delving into the details
C        of how N was computed by NEARPT.  Rather than spending a
C        lot of time explaining the details we will make an
C        unnecessary but inexpensive check that we don't divide by
C        zero when computing L'.
C
      DO I = 1, 3
         DTERM(I) = 1.0D0 + L*GRADM(I,I)
      END DO
 
      DO I = 1, 3
 
         IF ( DTERM(I) .NE. 0.0D0 ) THEN
            M(I,I) = 1.0D0 / DTERM(I)
         ELSE
            FOUND = .FALSE.
            CALL CHKOUT ( 'DNEARP' )
            RETURN
         END IF
 
      END DO
 
      DENOM = VTMV ( GRAD, M, GRAD )
 
      IF ( DENOM .EQ. 0.0D0 ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'DNEARP' )
         RETURN
      END IF
 
      LPRIME = VTMV( GRAD, M, STATE(4) ) / DENOM
 
C
C     Now that we have L' we can easily compute N'. Rewriting
C     equation (2) from above we have.
C
C        N'  = M * ( P' - L'*GRAD )
C
      CALL VLCOM ( 1.0D0, STATE(4), -LPRIME, GRAD, TEMP     )
      CALL MXV   ( M,     TEMP,                    DNEAR(4) )
 
 
C
C     Only one thing left to do.  Compute the derivative
C     of the altitude ALT.  Recall that
C
C                              GRAD
C        ALT     = < P  -  N, ------ >
C                             |GRAD|
C
C                             GRAD
C        dALT/dt = < P' - N', ------ >
C                             |GRAD|
C
C                                        GRAD
C                 + < P  -  N, Deriv of{------} >
C                                       |GRAD|
C
C     The second term is zero.  To see this note that P - N is parallel
C     to GRAD.  Moreover, since GRAD/|GRAD| is a unit vector its
C     derivative is necessarily orthogonal to it.  Hence it is
C     orthogonal to GRAD and P-N.
C
C     Thus
C                              GRAD
C        dALT/dt = < P' - N', ------ >
C                             |GRAD|
C
C     But as we discussed earlier N' is orthogonal to GRAD.  Thus
C
C                          GRAD
C        dALT/dt = < P' , ------ >
C                         |GRAD|
C
C     We've already computed GRAD/|GRAD| (NORML). Hence
C
C        dALT/dt = < P', NORML >
C
      DALT(2) = VDOT ( STATE(4), NORML )
 
      CALL CHKOUT ( 'DNEARP' )
      RETURN
      END
