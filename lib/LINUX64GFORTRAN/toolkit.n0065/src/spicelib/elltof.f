 
C$Procedure      ELLTOF ( Elliptic time of flight )
 
      SUBROUTINE ELLTOF ( MA, ECC, E )
 
C$ Abstract
C
C     Solve the time of flight equation MA = E - e sin(E) for the
C     elliptic eccentric anomaly E, given mean anomaly the MA and
C     the eccentricity ECC.
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
C     CONIC
C
C$ Declarations
 
      DOUBLE PRECISION      MA
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      E
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MA         I   Mean anomaly at epoch.
C     ECC        I   Eccentricity.
C     E          O   Elliptic eccentric anomaly.
C
C$ Detailed_Input
C
C     MA          is the elliptic mean anomaly of an orbiting body at
C                 some epoch t,
C
C                                      3 1/2
C                       MA = (t-T)(mu/a )
C
C                 where T is the time of periapsis passage, a is
C                 the semi-major axis of the orbit, and mu is the
C                 gravitational parameter of the primary body.
C
C     ECC         is the eccentricity of the orbit.
C
C$ Detailed_Output
C
C     E           is the corresponding eccentric anomaly. This is the
C                 solution to the time of flight equation
C
C                       MA = E - e sin(E)
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the eccentricity (ECC) is outside the range [0,1),
C        the error 'SPICE(WRONGCONIC)' is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Iterate to solve
C
C           f(E,MA,e) = E - e sin(E) - MA = 0
C
C$ Examples
C
C     ELLTOF, HYPTOF, and PARTOF are used by CONICS.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1] Roger Bate, Fundamentals of Astrodynamics, Dover, 1971.
C
C     [2] Ed Ng, "A General Algorithm for the Solution of Kepler's
C         Equation for Elliptic Orbits", Cel. Mech. 20, 243, 1979.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     elliptic time of flight
C
C-&
 
C$ Revisions
C
C-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
C
C        The routine now verifies that the eccentricity is in the
C        proper range---[0,1)---before proceeding.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      DCBRT
      DOUBLE PRECISION      HALFPI
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
 
C
C     Local parameters
C
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL = 1.D-15 )
 
C
C     Local variables
C
      DOUBLE PRECISION      M
      DOUBLE PRECISION      MPRIME
      INTEGER               N
 
      DOUBLE PRECISION      M0
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      R
      DOUBLE PRECISION      QR
      DOUBLE PRECISION      Y
 
      DOUBLE PRECISION      FN
      DOUBLE PRECISION      DERIV
      DOUBLE PRECISION      DERIV2
      DOUBLE PRECISION      CHANGE
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ELLTOF' )
      END IF
 
      IF ( ECC .LT. 0.D0  .OR.  ECC .GE. 1.D0 ) THEN
         CALL SIGERR ( 'SPICE(WRONGCONIC)' )
         CALL CHKOUT ( 'ELLTOF'            )
         RETURN
      END IF
 
 
C
C     For reasons of numerical stability, we would like to restrict
C     our solution to the interval [0,pi]. Because E, M, and sin E
C     are always positive or negative together, we can pretend that M
C     is positive and adjust the sign of the result. And for M, E > pi,
C     we can define
C
C           M = 2n pi + M'     and    E = 2n pi + E'
C
C     where M' and E' are in the interval [-pi,pi]. Solving for E'
C     gives us E.
C
C     So, we begin by reducing the input mean anomaly to [0,pi].
C
      M = ABS ( MA )
 
      IF ( M .GT. PI() ) THEN
         N      = INT ( (M-PI()) / TWOPI() ) + 1
         MPRIME = M - N * TWOPI()
      ELSE
         N      = 0
         MPRIME = M
      END IF
 
      M = ABS ( MPRIME )
 
C
C     The convergence of the iterative scheme below depends on a good
C     initial estimate for E.
C
C     For small eccentricity, the initial estimate E = M is sufficient.
C     However, as the eccentricity increases, so does the number of
C     iterations required for convergence. For sufficiently large
C     eccentricity, this estimate leads to divergence.
C
C     Ng [2] notes that the function y(M,e)
C
C           E - M
C          -------  =  sin(e y + M)
C             e
C
C     increases and decreases monotonically when M is in the ranges
C     [0,M0] and [m0,pi], respectively.
C
C     When M0 < M < pi, where M0 = (pi/2) - e, the cubic
C             -   -
C
C                              pi - M  2        pi - M    pi - M
C           B(M,e) = 1 - (1 -  -------)  (1 + 2 ------- - -------)
C                              pi - M0          pi - M0    1 + e
C
C     provides a good initial estimate of y for all values of e.
C
C
      M0 = HALFPI() - ECC
 
      IF ( M .GE. M0 ) THEN
 
         A = PI() - M
         B = PI() - M0
 
         Y = 1.D0 - (1.D0 - A/B)**2 * (1.D0 + 2.D0*A/B - A/(1.D0+ECC))
         E = ECC * SIN(ECC*Y + M) + M
 
C
C     The situation is a little more troublesome, however, when M < M0.
C     For small eccentricity, the cubic
C
C                                  2
C           A(M,e) = 1 - (1 - M/M0)  (1 + 2M/M0 - M/(1-e) )
C
C     gives a reasonable first estimate of y. However, as e -> 1,
C     successive approximations of the form
C
C                             k           k
C           C (M,e) = 1 - (-1)  (1 - M/M0)
C            k
C
C     are used, where k = 4 for e > 0.7, and k = 8 for e > 0.85.
C
C     For high eccentricity (e > 0.96) and low mean anomaly (M < 0.05),
C     these successive approximations eventually fail. Fortunately, in
C     just these cases, the cubic
C
C                           3    2  1/3           3    2  1/3
C           D(M,e) = [r + (q  + r )]     + [r - (q  + r )]
C
C     where
C
C           r = 3M/e,   q = (2/e)(1 - e)
C
C     provides a reasonable estimate of E directly.
C
C
      ELSE IF ( ECC .LE. 0.7D0 ) THEN
 
         Y = 1.D0 - (1.D0 - M/M0)**2 * (1.D0 + 2.D0*M/M0 - M/(1.D0-ECC))
         E = ECC * SIN(ECC*Y + M) + M
 
      ELSE IF ( ECC .LE. 0.85D0 ) THEN
 
         Y = 1.D0 - (1.D0 - M/M0)**4
         E = ECC * SIN(ECC*Y + M) + M
 
      ELSE IF ( ECC .LE. 0.96D0 .OR. M .GT. 0.05D0 ) THEN
 
         Y = 1.D0 - (1.D0 - M/M0)**8
         E = ECC * SIN(ECC*Y + M) + M
 
      ELSE
 
         Q  = (2.D0/ECC) * (1.D0 - ECC)
         R  = 3.D0 * (M/ECC)
         QR = SQRT ( Q**3 + R**2 )
 
         E  = DCBRT ( R + QR ) + DCBRT ( R - QR )
 
      END IF
 
 
C
C     Use the Newton second-order method,
C
C                                           2
C          E    = E  - (f/f')*(1 + f*f''/2f' )
C           i+1    i
C
C     where
C
C          f   = E - e sin(E) - M
C          f'  = 1 - e cos(E)
C          f'' =     e sin(E)
C
      CHANGE = 1.D0
 
      DO WHILE ( ABS ( CHANGE ) .GT. TOL )
 
         FN     = E    - ECC * SIN(E) - M
         DERIV  = 1.D0 - ECC * COS(E)
         DERIV2 =        ECC * SIN(E)
 
         CHANGE = (FN/DERIV) * ( 1.D0 + (FN*DERIV2) / (2.D0*DERIV**2) )
         E      = E - CHANGE
 
      END DO
 
C
C     "Unwrap" E' into the actual value of E.
C
      IF ( MPRIME .LT. 0 ) THEN
         E = -E
      END IF
 
      IF ( N .GT. 0 ) THEN
         E = E + N * TWOPI()
      END IF
 
      IF ( MA .LT. 0 ) THEN
         E = -E
      END IF
 
 
      CALL CHKOUT ( 'ELLTOF' )
      RETURN
      END
