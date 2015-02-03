C$Procedure  CHBIGR ( Chebyshev expansion integral )

      SUBROUTINE CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
 
C$ Abstract
C
C     Evaluate an indefinite integral of a Chebyshev expansion at a
C     specified point X. The constant of integration is selected to
C     make the integral zero when X equals the abscissa value X2S(1).
C     Return the value of the input expansion at X as well.
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
C     CHEBYSHEV
C     EPHEMERIS
C     INTEGRAL
C     MATH
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               DEGP
      DOUBLE PRECISION      CP     ( * )
      DOUBLE PRECISION      X2S    ( 2 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      P      
      DOUBLE PRECISION      ITGRLP 

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     DEGP       I   Degree of input Chebyshev expansion.
C     CP         I   Chebyshev coefficients of input expansion.
C     X2S        I   Transformation parameters.
C     X          I   Abscissa value of evaluation.
C     P          O   Input expansion evaluated at X.
C     ITGRLP     O   Integral evaluated at X.
C
C$ Detailed_Input
C
C     DEGP       is the degree of the input Chebyshev expansion.
C     
C     CP         is an array containing the coefficients of the input
C                Chebyshev expansion. The coefficient of the Ith
C                Chebyshev polynomial is contained in element CP(I+1),
C                for I = 1 : DEGP+1.
C
C     X2S        is an array containing the "transformation parameters"
C                of the domain of the expansion. Element X2S(1)
C                contains the midpoint of the interval on which the
C                input expansion is defined; X2S(2) is one-half of the
C                length of this interval; this value is called the
C                interval's "radius."
C
C                The input expansion defines a function f(X) on the
C                interval 
C
C                   [ X2S(1)-X2S(2),  X2S(1)+X2S(2) ]
C
C                as follows:
C
C                   Define S = ( X - X2S(1) ) / X2S(2) 
C 
C
C                                  DEGP+1
C                                  __
C                                  \
C                   f(X) = g(S)  = /  CP(k)  T   (S)
C                                  --         k-1
C                                  k=1
C
C
C     X          is the abscissa value at which the function defined by
C                the input expansion and its integral are to be
C                evaluated. Normally X should lie in the closed
C                interval
C
C                   [ X2S(1)-X2S(2),  X2S(1)+X2S(2) ]
C
C                See the Restrictions section below.
C
C$ Detailed_Output
C     
C     P,
C     ITGRLP     Define S and f(X) as above in the description of the
C                input argument X2S. Then P is f(X), and ITGRLP is
C                an indefinite integral of f(X), evaluated at X.
C                
C                The indefinite integral satisfies
C
C                   d(ITGRLP)/dX     = f(X)
C
C                and
C
C                   ITGRLP( X2S(1) ) = 0
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input degree is negative, the error
C         SPICE(INVALDDEGREE) is signaled.
C
C     2)  If the input interval radius is non-positive, the error
C         SPICE(INVALIDRADIUS) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Let 
C
C        T ,  n = 0, ...
C         n
C
C     represent the nth Chebyshev polynomial of the first kind:
C
C        T (x) = cos( n*arccos(x) )
C         n
C
C     The input coefficients represent the Chebyshev expansion
C
C                       DEGP+1
C                       __
C                       \
C        f(X) = g(S)  = /  CP(k)  T   (S)
C                       --         k-1
C                       k=1
C
C     where
C
C        S = ( X - X2S(1) ) / X2S(2) 
C
C     This routine evaluates and returns the value at X of an
C     indefinite integral F(X), where
C
C        dF(X)/dX    = f(X)  for all X in 
C                            [X2S(1)-X2S(2), X2S(1)+X2S(2)]
C
C        F( X2S(1) ) = 0
C
C     The value at X of the input expansion 
C
C        f(X)
C
C     is returned as well.
C
C     Note that numerical problems may result from applying this
C     routine to abscissa values outside of the interval defined
C     by the input parameters X2S(*). See the Restrictions section.
C
C     To evaluate Chebyshev expansions and their derivatives, use the
C     SPICELIB routines CHBINT or CHBDER.
C
C     This routine supports the SPICELIB SPK type 20 and PCK type 20
C     evaluators SPKE20 and PCKE20.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     1)  Let the domain of a polynomial to be evaluated be the
C         closed interval
C
C            [20, 30]
C
C         Let the input expansion represent the polynomial
C
C                             6
C            f(X)  = g(S) = 5S
C
C         where
C
C            S     = (X - 20)/10
C
C         Let F(X) be an indefinite integral of f(X) such that
C
C            F(20) = 0
C
C         Evaluate 
C
C            f(30) and F(30)
C
C
C        Example code begins here.
C
C              PROGRAM EX1
C              IMPLICIT NONE
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      CP    ( 6 )
C              DOUBLE PRECISION      X
C              DOUBLE PRECISION      X2S   ( 2 )
C              DOUBLE PRECISION      P
C              DOUBLE PRECISION      ITGRLP
C
C              INTEGER               DEGP
C
C        C
C        C     Let our domain be the interval [10, 30].
C        C
C              X2S(1) = 20.D0
C              X2S(2) = 10.D0
C        C
C        C     Assign the expansion coefficients.
C        C
C              DEGP  = 5
C
C              CP(1) = 0.D0
C              CP(2) = 3.75D0
C              CP(3) = 0D0
C              CP(4) = 1.875D0
C              CP(5) = 0.D0
C              CP(6) = 0.375D0
C
C        C
C        C     Evaluate the function and its integral at X = 30.
C        C
C              X = 30.D0
C
C              CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
C
C        C
C        C     We make the change of variables
C        C
C        C        S(X) = (1/10) * ( X - 20 )
C        C
C        C     The expansion represents the polynomial
C        C
C        C                         5
C        C        f(X) = g(S) = 6*S
C        C
C        C     An indefinite integral of the expansion is
C        C
C        C                                    6
C        C        F(X) = G(S) * dX/dS = 10 * S   
C        C
C        C     where G is defined on the interval [-1, 1]. The result
C        C     should be (due to the change of variables)
C        C
C        C          (G(1)  - G(0) ) * dX/dS
C        C
C        C        = (F(30) - F(20)) * 10
C        C
C        C        = 10
C        C
C        C     The value of the expansion at X should be
C        C
C        C        f(20) = g(1) = 6
C        C
C              WRITE (*,*) 'ITGRLP = ', ITGRLP
C              WRITE (*,*) 'P      = ', P
C
C              END
C
C
C     When this program was executed on a PC/Linux/gfortran platform,
C     the output was:
C
C        ITGRLP =   10.0000000000000000
C        P      =    6.0000000000000000
C
C
C$ Restrictions
C
C     1)  The value (X-X2S(1)) / X2S(2) normally should lie within the
C         interval -1:1 inclusive, that is, the closed interval 
C         [-1, 1]. Chebyshev polynomials increase rapidly in magnitude
C         as a function of distance of abscissa values from this
C         interval.
C
C         In typical SPICE applications, where the input expansion
C         represents position, velocity, or orientation, abscissa
C         values that map to points outside of [-1, 1] due to round-off
C         error will not cause numeric exceptions.
C
C     2)  No checks for floating point overflow are performed.
C
C     3)  Significant accumulated round-off error can occur for input
C         expansions of excessively high degree. This routine imposes
C         no limits on the degree of the input expansion; users must
C         verify that the requested computation provides appropriate
C         accuracy.
C
C$ Literature_References
C
C     1)  "Numerical Recipes -- The Art of Scientific Computing" by
C         William H. Press, Brian P. Flannery, Saul A. Teukolsky,
C         Willam T. Vetterling. (See Clenshaw's Recurrence Formula.)
C
C     2)  Chebyshev polynomials. (2013, September 28). In Wikipedia,
C         The Free Encyclopedia. Retrieved 01:23, November 23, 2013, 
C         from http://en.wikipedia.org/w/index.php?title=
C         Chebyshev_polynomials&oldid=574881046
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 03-DEC-2013 (NJB)
C
C-&

C$ Index_Entries
C
C     integral of chebyshev_polynomial_expansion
C     integrate chebyshev_polynomial_expansion
C
C-&
 

C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Local variables
C 
      DOUBLE PRECISION      A2
      DOUBLE PRECISION      ADEGP1
      DOUBLE PRECISION      ADEGP2
      DOUBLE PRECISION      AI
      DOUBLE PRECISION      C0
      DOUBLE PRECISION      F      ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      S2
      DOUBLE PRECISION      W      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               NTERMS

C
C     Test RETURN but don't check in. Use discovery check-in.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Check the expansion degree. 
C
      IF ( DEGP .LT. 0 )  THEN

         CALL CHKIN  ( 'CHBIGR'                                )
         CALL SETMSG ( 'Expansion degree must be non-negative '
     .   //            'but was #.'                            )
         CALL ERRINT ( '#', DEGP                               )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                  )
         CALL CHKOUT ( 'CHBIGR'                                )
         RETURN

      END IF

C
C     Check the radius of the domain interval.
C
      IF ( X2S(2) .LE. 0.D0 )  THEN

         CALL CHKIN  ( 'CHBIGR'                            )
         CALL SETMSG ( 'Interval radius must be positive '
     .   //            'but was #.'                        )
         CALL ERRDP  ( '#', X2S(2)                         )
         CALL SIGERR ( 'SPICE(INVALIDRADIUS)'              )
         CALL CHKOUT ( 'CHBIGR'                            )
         RETURN

      END IF

      NTERMS = DEGP + 1

C
C     Background 
C     ==========
C
C
C     Let 
C
C        T ,  n = 0, ...
C         n
C
C     represent the nth Chebyshev polynomial of the first kind:
C
C        T (x) = cos( n*arccos(x) )
C         n
C
C     These polynomials satisfy the recurrence relationship
C
C        T   (x) = 2x T (x)  -  T   (x),  n = 2, ...
C         n+1          n         n-1
C
C     The Chebyshev polynomials of the second kind are denoted by
C
C        U ,  n = 0, ...
C         n
C
C     where
C
C        U (x) = 1
C         0
C
C        U (x) = 2x
C         1
C
C        U   (x) = 2x U (x)  -  U   (x),  n = 2, ...
C         n+1          n         n-1
C
C
C     The integration formula (1) below is based on several 
C     identities:
C
C
C        T (x)        = (1/2) * ( U (x) - U   (x) ),  n = 2, ...    (B1)
C         n                        n       n-2
C
C
C        d(T (x))/dx  =  n U   (x),   n = 1, ...                    (B2)
C           n               n-1
C           
C
C                                 d(T   (x))/dx   d(T   (x))/dx
C                                    n+1             n-1
C        T (x)        = (1/2) * ( ------------- - ------------- ), 
C         n                             n+1             n-1
C
C                                     n = 2, ...                    (B3)
C
C
C     Identity (B1) can be proved via mathematical induction. Using
C     (B1) and the Chebyshev recurrence formulas for both kinds of
C     polynomials, identity (B2) can also be proved via mathematical
C     induction. Identity (B3) follows directly from the combination of
C     (B1) and (B2).
C
C     Formula (1) below follows from (B3).
C
C
C     Algorithm
C     =========
C
C     In the discussion below, all Chebyshev polynomials are of the
C     first kind.
C
C     Let the notation
C
C        I( f )
C
C     represent the indefinite integral of a function f.
C
C     The key formula we use below is 
C
C                              T          T             
C                               n+1        n-1
C        I ( T  ) = (1/2) * ( ------  -  ------ )  +  C,  for n > 1  (1)
C             n                 n+1        n-1
C                                                       
C     where C is a constant of integration. Applying (1) to a Chebyshev
C     expansion
C
C               N+1
C               __
C               \
C        f(x) = /  a  T   (x)                                        (2)
C               --  k  k-1
C               k=1
C     
C     we have
C
C                               N+1
C                               __       T (x)    T   (x)
C                               \         k        k-2
C        I( f(x) ) =      (1/2) /  a (   ------ - -------  )       
C                               --  k       k       k-2             
C                               k=3
C                                           
C                    + a T (x) + ( a  / 4 ) T (x) + C ,   for N > 1 (3a)
C                       1 1         2        2       0
C
C     or
C
C        I( f(x) ) = C  + a T (x),   for N = 1                      (3b)
C                     0    1 1
C     
C     where 
C
C        C
C         0
C
C     is a constant of integration. Then by grouping coefficients of
C     the Chebyshev polynomials, we have
C
C      
C                           N+2
C                           __
C                           \
C        I( f(x) ) = C  +   /  A  T   (x),  for N >= 0               (4)
C                     0     --  k  k-1
C                           k=2
C
C     where
C                     
C        A    = a   -  (1/2)a           for N >= 2,    or         
C         2      1           3
C
C        A    = a                       for N <= 1                  (5a)
C         2      1
C
C                 1
C        A    = ------ ( a  -  a    )   for k = 3, ... , N (N>=3)   (5b)
C         k     2(k-1)    k-1   k+1
C
C                1
C        A    = ---- a                  for N >= 2                  (5c)
C         N+1    2N   N
C
C                 1
C        A    = ------ a                for N >= 1                  (5d)
C         N+2   2(N+1)  N+1
C
C
C     Note that (5b) does not conflict with (5c) or (5d) for N < 3, 
C     since (5b) is applicable only when N >= 3.
C
C
C     We'll compute the sums 
C      
C                           N+2
C                           __
C                           \
C        I( f(x) ) - C  =   /  A  T   (x)                            (6)
C                     0     --  k  k-1
C                           k=2
C
C               N+2
C               __
C               \
C        C  = - /  A  T   (0)                                        (7)
C         0     --  k  k-1
C               k=2
C
C     and
C
C               N+1
C               __
C               \
C        f(x) = /  a  T   (x)                                        
C               --  k  k-1
C               k=1
C
C
C     using Clenshaw's recurrence formula. Note that in the above
C     equations, N is the degree of the input expansion, which is
C     given by the input argument DEGP.
C
C     Transform the independent variable X to the interval
C     
C        [-1, 1]
C
C     Call the result S.
C
C     Note we've already checked that X2S(2) is positive.
C
      S     = (X-X2S(1)) / X2S(2)
      S2    = 2.0D0 * S

C
C     Pre-compute the coefficients of the integral expansion
C     that are known at this point. The terms A2, ADEGP1, and
C     ADEGP2 correspond to the variables
C
C        A , A   , A
C         2   N+1   N+2 
C
C     above and are set according to equations 5a, 5c, and 5d.
C
      IF ( NTERMS .GE. 3 ) THEN

         A2 = CP(1) - 0.5D0*CP(3)
      ELSE
         A2 = CP(1)
      END IF

C
C     Initialize the two highest-indexed coefficients of
C     the integral expansion.
C
      ADEGP1 = 0.D0
      ADEGP2 = 0.D0

      IF ( DEGP .GE. 2 ) THEN

         ADEGP1 = 0.5D0 * CP(DEGP) / DEGP

      END IF

      IF ( DEGP .GE. 1 ) THEN

         ADEGP2 = 0.5D0 * CP(DEGP+1) / (DEGP+1)

      END IF

C
C     The three quantities we'll compute require different numbers of
C     loop iterations: the integrals at X and at 0 require that I be
C     initialized to DEGP+2, while the input expansion requires that I
C     be initialized to DEGP+1. Since we wish to save on loop overhead
C     by performing the respective loop body actions in parallel, we
C     perform the integral computations for I = DEGP+2 prior to the
C     start of the loop.
C
C     Since F(1) and F(2) would normally be initialized to 0 prior to
C     the start of the loop, after the first loop pass, F would have
C     the contents assigned below.
C
      F(3) = 0.D0
      F(2) = 0.D0
C
C     The initial value of F(1) is the highest-order coefficient of
C     the integral's expansion.
C
      IF ( DEGP .EQ. 0 ) THEN
         F(1) = A2
      ELSE
         F(1) = ADEGP2
      END IF
C
C     We also want to evaluate [I(f)](0); we'll use this to
C     make the integral 0 at x = 0. We'll use the terms
C     Z(*) to evaluate the expansion of the integral at x = 0.
C
      Z(3) = 0.D0
      Z(2) = 0.D0
      Z(1) = F(1)
C
C     We'll use the terms W(*) to evaluate the input expansion.
C     
      W(1) = 0.D0
      W(2) = 0.D0

C
C     NTERMS is DEGP+1.
C
      I = NTERMS

      DO WHILE ( I .GT. 1 )
C
C        The variable AI represents A(I), which is the Ith coefficient
C        of the Chebyshev expansion of the indefinite integral. AI is 
C        set according to formulas 5a-c above.
C
         IF ( I .EQ. 2 ) THEN

            AI = A2

         ELSE IF ( I .LT. NTERMS ) THEN
C
C           AI is A ; I >= 3; DEGP >= 3.
C                  I
C 
            AI = 0.5D0 * ( CP(I-1) - CP(I+1) ) / (I-1)

         ELSE
C
C           AI is A      ; I = NTERMS.
C                  DEGP+1
C 
            AI = ADEGP1

         END IF

         F(3) = F(2)
         F(2) = F(1)
         F(1) = AI     + ( S2*F(2)  - F(3) )

         Z(3) = Z(2)
         Z(2) = Z(1)
         Z(1) = AI     - Z(3) 

         W(3) = W(2)
         W(2) = W(1)
         W(1) = CP(I)  + ( S2*W(2)  - W(3) )
  
         I    = I - 1
 
      END DO

C
C     C0 is the negative of the input expansion evaluated at x=0, and
C     that expansion value is
C
C        0*Z(1) - Z(2)
C
C     The other terms are computed as in CHBINT.
C     
      C0      =                    Z(2)
      ITGRLP  = C0    +   S*F(1) - F(2) 
      P       = CP(1) + ( S*W(1) - W(2) )

C
C     Scale the integral to account for the change of variables
C     (from the original domain to [-1,1]). The scale factor is
C     
C        dX/dS = X2S(2)
C
      ITGRLP = X2S(2) * ITGRLP 
     
      RETURN
      END 

