C$Procedure      KPSOLV ( Solve Keplers Equation --- Vector Form )
 
      DOUBLE PRECISION FUNCTION KPSOLV ( EVEC )
 
C$ Abstract
C
C    This routine solves the equation X = < EVEC, U(X) > where
C    U(X) is the unit vector [ Cos(X), SIN(X) ] and  < , > denotes
C    the two-dimensional dot product.
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
C     ROOTS
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      EVEC ( 2 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     EVEC       I   A 2-vector whose magnitude is less than 1.
C
C     The function returns the solution to X = < EVEC, U(X) >
C
C$ Detailed_Input
C
C     EVEC       is any two dimensional vector whose magnitude is
C                less than 1.
C
C$ Detailed_Output
C
C     The function returns the value X such that the equation
C
C        X = EVEC(1)COS(X) + EVEC(2)SIN(X).
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the magnitude of EVEC is greater than or equal to 1
C        the error SPICE(EVECOUTOFRANGE) is signalled.
C
C$ Particulars
C
C     This routine uses bisection and Newton's method to find
C     the root of the equation
C
C        X = EVEC(1)COS(X) + EVEC(2)SIN(X).
C
C     This equation is just a "vector form" of Kepler's equation.
C
C
C$ Examples
C
C     Suppose you need to solve the equation
C
C         M = E - e SIN(E)                           [ 1 ]
C
C     for E. If we let X = E - M the equation is transformed to
C
C        0 = X - e SIN( X + M )
C
C          = X - e SIN( M ) COS(X) - e COS(M) SIN ( X )
C
C     Thus if we solve the equation
C
C        X = e SIN(M) COS(X) + e COS(M) SIN(X)
C
C     we can find the value of X we can compute E.
C
C     The code fragment below illustrates how this routine can
C     be used to solve equation [1].
C
C         EVEC(1) = ECC * DSIN(M)
C         EVEC(2) = ECC * DCOS(M)
C         E       = M   + KPSOLV( EVEC )
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-AUG-1997 (WLT)
C
C        KPSOLV is now given an initial value of zero so that
C        if an error condition is detected, KPSOLV will have
C        a return value.
C
C-    SPICELIB Version 1.0.0, 03-JAN-1997 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Solve the vector form of the Kepler equation
C
C-&
 
C
C     MXNEWT is the number of iterations we will perform
C     in the Newtons method for finding the solution to
C     the vector form of Kepler's equation.  It has been
C     empirically determined that 5 iterations is always
C     sufficient on computers have 64 bit double precision
C     numbers.
C
      INTEGER               MXNEWT
      PARAMETER           ( MXNEWT = 5 )
 
 
      DOUBLE PRECISION      COSX
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      ECC2
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
      DOUBLE PRECISION      SINX
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XL
      DOUBLE PRECISION      XM
      DOUBLE PRECISION      XU
      DOUBLE PRECISION      Y0
      DOUBLE PRECISION      YPX
      DOUBLE PRECISION      YX
      DOUBLE PRECISION      YXM
 
      INTEGER               I
      INTEGER               MAXIT
 
C
C     We give the function an initial value, just in case
C     we exit without solving Kepler's equation.
C
      KPSOLV = 0.0D0
 
      H      = EVEC(1)
      K      = EVEC(2)
 
      ECC2   = H*H + K*K
 
      IF ( ECC2 .GE. 1.0D0 ) THEN
 
         CALL CHKIN  ( 'KPSOLV' )
         CALL SETMSG ( 'The magnitude of the vector EVEC = ( #, '
     .   //            '# ) must be less than 1.  However, the '
     .   //            'magnitude of this vector is #.' )
 
         CALL ERRDP  ( '#', H           )
         CALL ERRDP  ( '#', K           )
         CALL ERRDP  ( '#', DSQRT(ECC2) )
 
         CALL SIGERR ( 'SPICE(EVECOUTOFRANGE)'  )
         CALL CHKOUT ( 'KPSOLV' )
         RETURN
 
      END IF
 
C
C     We first approximate the equation 0 = X - H * COS(X) - K * SIN(X)
C     using bisection.  If we let Y(X) = X - H * COS(X) - K * SIN(X)
C
C        Y( ECC) =  ECC - <EVEC,U(X)>  =   ECC - ECC*COS(ANGLE_X) > 0
C        Y(-ECC) = -ECC - <EVEC,U(X)>  =  -ECC - ECC*COS(ANGLE_X) < 0
C
C     where ANGLE_X is the angle between U(X) and EVEC. Thus -ECC
C     and ECC necessarily bracket the root of the equation Y(X) = 0.
C
C     Also note that Y'(X) = 1 - < EVEC, V(X) > where V(X) is the
C     unit vector given by U'(X).  Thus Y is an increasing function
C     over the interval from -ECC to ECC.
C
C     The mid point of ECC and -ECC is 0 and Y(0) = -H.  Thus
C     we can do the first bisection step without doing
C     much in the way of computations.
C
 
      Y0  = -H
      XM  =  0.0D0
      ECC =  DSQRT(ECC2)
 
      IF      ( Y0 .GT. 0.0D0 ) THEN
 
         XU  =  0.0D0
         XL  = -ECC
 
      ELSE IF ( Y0 .LT. 0.0D0 ) THEN
 
         XU =  ECC
         XL =  0.0D0
 
      ELSE
 
         KPSOLV = 0.0D0
         RETURN
 
      END IF
C
C     Iterate until we are assured of being in a region where
C     Newton's method will converge quickly.  The formula
C     below was empirically determined to give good results.
C
      MAXIT = MIN(32, MAX( 1, NINT(1.0D0/(1.0D0-ECC)) ) )
 
      DO I = 1, MAXIT
 
C
C        Compute the next midpoint.  We bracket XM by XL and XU just in
C        case some kind of strange rounding occurs in the computation
C        of the midpoint.
C
         XM     = MAX( XL, MIN( XU, 0.5D0*(XL + XU) ) )
 
C
C        Compute Y at the midpoint of XU and XL
C
         YXM = XM - H*COS(XM) - K*SIN(XM)
C
C        Determine the new upper and lower bounds.
C
         IF ( YXM .GT. 0.0D0 ) THEN
            XU = XM
         ELSE
            XL = XM
         END IF
 
 
      END DO
 
C
C     We've bisected into a region where we can now get rapid
C     convergence using Newton's method.
C
      X   = XM
 
      DO I = 1, MXNEWT
 
         COSX = DCOS(X)
         SINX = DSIN(X)
C
C        Compute Y and Y' at X.  Use these to get the next
C        iteration for X.
C
C        For those of you who might be wondering, "Why not put
C        in a check for YX .EQ. 0 and return early if we get
C        an exact solution?"  Here's why.  An empirical check
C        of those cases where you can actually escape from the
C        Do-loop  showed that the test YX .EQ. 0 is true
C        only about once in every 10000 case of random inputs
C        of EVEC.  Thus on average the check is a waste of
C        time and we don't bother with it.
C
         YX   = X     - H*COSX - K*SINX
         YPX  = 1.0D0 + H*SINX - K*COSX
         X    = X     - YX/YPX
 
      END DO
 
      KPSOLV = X
 
      RETURN
      END
