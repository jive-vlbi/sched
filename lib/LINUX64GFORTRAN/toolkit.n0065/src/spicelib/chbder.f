C$Procedure      CHBDER ( Derivatives of a Chebyshev expansion )
 
      SUBROUTINE CHBDER ( CP, DEGP, X2S , X, NDERIV, PARTDP, DPDXS )
 
C$ Abstract
C
C     Given the coefficients for the Chebyshev expansion of a
C     polynomial, this returns the value of the polynomial and its
C     first NDERIV derivatives evaluated at the input X.
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
C      INTERPOLATION,  MATH,  POLYNOMIAL
C
C$ Declarations
 
      DOUBLE PRECISION    CP     (      * )
      INTEGER             DEGP
      DOUBLE PRECISION    X2S    (      2 )
      DOUBLE PRECISION    X
      INTEGER             NDERIV
      DOUBLE PRECISION    PARTDP ( 3, 0:* )
      DOUBLE PRECISION    DPDXS  (    0:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      CP         I   NDEG+1 Chebyshev polynomial coefficients.
C      DEGP       I   Degree of polynomial.
C      X2S        I   Transformation parameters of polynomial.
C      X          I   Value for which the polynomial is to be evaluated
C      NDERIV     I   The number of derivatives to compute
C      PARTDP     -   Workspace provided for computing derivatives
C      DPDXS(I)   O   Value of the I'th derivative of the polynomial
C
C$ Detailed_Input
C
C      CP         is an array of coefficients a polynomial with respect
C                 to the Chebyshev basis.  The polynomial to be
C                 evaluated is assumed to be of the form:
C
C                   CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
C
C                                    ... + CP(2)*T(1,S) + CP(1)*T(0,S)
C
C                 where T(I,S) is the I'th Chebyshev polynomial
C                 evaluated  at a number S whose double precision
C                 value lies between -1 and 1.  The value of S is
C                 computed from the input variables P(1), P(2) and X.
C
C      DEGP       is the degree of the Chebyshev polynomial to be
C                 evaluated.
C
C      X2S        is an array of two parameters.  These parameters are
C                 used to transform the domain of the input variable X
C                 into the standard domain of the Chebyshev polynomial.
C                 X2S(1) should be a reference point in the domain of
C                 X; X2S(2) should be the radius by which points are
C                 allowed to deviate from the reference point and while
C                 remaining within the domain of X.  The value of
C                 X is transformed into the value S given by
C
C                           S = ( X - X2S(1) ) / X2S(2)
C
C                 Typically X2S(1) is the midpoint of the interval over
C                 which X is allowed to vary and X2S(2) is the radius
C                 of the interval.
C
C                 The main reason for doing this is that a Chebyshev
C                 expansion is usually fit to data over a span
C                 from A to B where A and B are not -1 and 1
C                 respectively.  Thus to get the "best fit" the
C                 data was transformed to the interval [-1,1] and
C                 coefficients generated. These coefficients are
C                 not rescaled to the interval of the data so that
C                 the numerical "robustness" of the Chebyshev fit will
C                 not be lost. Consequently, when the "best fitting"
C                 polynomial needs to be evaluated at an intermediate
C                 point, the point of evaluation must be transformed
C                 in the same way that the generating points were
C                 transformed.
C
C      X          Value for which the polynomial is to be evaluated.
C
C      NDERIV     is the number of derivatives to be computed by the
C                 routine.  NDERIV should be non-negative.
C
C      PARTDP     Is a work space used by the program to compute
C                 all of the desired derivatives.  It should be declared
C                 in the calling program as
C
C                        DOUBLE PRECISION PARTDP(3, 0:NDERIV)
C
C$ Detailed_Output
C
C      DPDXS(0)   The value of the polynomial to be evaluated.  It
C                 is given by
C
C                   CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
C
C                                    ... + CP(2)*T(1,S) + CP(1)*T(0,S)
C
C                 where T(I,S) is the I'th Chebyshev polynomial
C                 evaluated  at a number S = ( X - P(1) )/P(2)
C
C      DPDXS(I)   The value of the I'th derivative of the polynomial at
C                 X. (I ranges from 1 to NDERIV) It is given by
C
C                                             [i]
C                   (1/P(2)**I) ( CP(DEGP+1)*T   (DEGP,S)
C
C                                           [i]
C                               + CP(DEGP)*T   (DEGP-1,S) + ...
C
C                               .
C                               .
C                               .
C                                        [i]
C                           ... + CP(2)*T   (1,S)
C
C                                        [i]
C                               + CP(1)*T   (0,S) )
C
C                                   [i]
C                 where T(k,S) and T   (I,S)  are the k'th Chebyshev
C                 polynomial and its i'th derivative respectively,
C                 evaluated  at the number S = ( X - X2S(1) )/X2S(2).
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine computes the value of a Chebyshev polynomial
C      expansion and the derivatives of the expansion with respect to X.
C      The polynomial is given by
C
C           CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
C
C                            ... + CP(2)*T(1,S) + CP(1)*T(0,S)
C
C      where
C
C           S  =  ( X - X2S(1) ) / X2S(2)
C
C      and
C
C           T(i,S) is the i'th Chebyshev polynomial of the first kind
C           evaluated at S.
C
C
C$ Examples
C
C     Depending upon the user's needs, there are 3 routines available
C     for evaluating Chebyshev polynomials.
C
C        CHBVAL for evaluating a Chebyshev polynomial when no
C               derivatives are desired.
C
C        CHBINT for evaluating a Chebyshev polynomial and its
C               first derivative.
C
C        CHBDER for evaluating a Chebyshev polynomial and a user
C               or application dependent number of derivatives.
C
C     Of these 3 the one most commonly employed by NAIF software
C     is CHBINT as it is used to interpolate ephemeris state
C     vectors which requires the evaluation of a polynomial
C     and its derivative.  When no derivatives are desired one
C     should use CHBVAL, or when more than one or an unknown
C     number of derivatives are desired one should use CHBDER.
C
C     The code fragment below illustrates how this routine might
C     be used to obtain points for plotting a polynomial
C     and its derivatives.
C
C           fetch the pieces needed for describing the polynomial
C           to be evaluated.
C
C           READ  (*,*) DEGP, ( CP(I), I = 1, DEG+1 ), NDERIV, BEG, END
C
C           check to see that BEG is actually less than END
C
C           IF ( BEG .GE. END ) THEN
C
C              take some appropriate action
C
C           ELSE
C
C              X2S(1) = ( END + BEG ) / 2.0D0
C              X2S(2) = ( END - BEG ) / 2.0D0
C
C           END IF
C
C           STEP = END - BEG / <number of points used for plotting>
C           X    = BEG
C
C           DO WHILE ( X .LE. END )
C
C              CALL CHBDER ( CP, DEGP, X2S , X, NDERIV, PARTDP, DPDXS )
C
C              do something with the pairs ( X, DPDXS(0)),(X,DPDXS(1)),
C              (X,DPDXS(2)) ... (X,DPDXS(NDERIV))
C
C              X = X + STEP
C
C           END DO
C
C$ Restrictions
C
C      The user must be sure that the provided workspace is declared
C      properly in the calling routine.  The proper declaration is:
C
C      INTEGER          NDERIV
C      PARAMETER      ( NDERIV = the desired number of derivatives )
C      DOUBLE PRECISION PARTDP (3, 0:NDERIV)
C
C      If for some reason a parameter is not passed to this routine in
C      NDERIV, the user should make sure that the value of NDERIV is not
C      so large that the work space provided is inadequate.
C
C      One needs to be careful that the value (X-X2S(1)) / X2S(2) lies
C      between -1 and 1.  Otherwise, the routine may fail spectacularly
C      (for example with a floating point overflow).
C
C      While this routine will compute derivatives of the input
C      polynomial, the user should consider how accurately the
C      derivatives of the Chebyshev fit, match the derivatives of the
C      function it approximates.
C
C
C
C$ Exceptions
C
C     Error free
C
C     No tests are performed for exceptional values ( NDERIV negative,
C     DEGP negative, etc.) This routine is expected to be used at a low
C     level in ephemeris evaluations. For that reason it has been
C     elected as a routine that will not participate in error handling.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      "Numerical Recipes -- The Art of Scientific Computing" by
C       William H. Press, Brian P. Flannery, Saul A. Teukolsky,
C       Willam T. Vetterling.  (See Clenshaw's Recurrence Formula)
C
C      "The Chebyshev Polynomials" by Theodore J. Rivlin
C
C      "CRC Handbook of Tables for Mathematics"
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     derivatives of a chebyshev expansion
C
C-&
 
 
 
C$ Revisions
C
C-    Beta Version 1.0.1, 16-FEB-1988 (WLT) (NJB)
C
C     The Error free specification was added to the routine as
C     well as an explanation for this designation.  Examples added.
C     Declaration of unused variable RECIP removed.
C-&
 
C
C     Local variables
C
 
      DOUBLE PRECISION SCALE
 
      DOUBLE PRECISION S
      DOUBLE PRECISION S2
 
      INTEGER          I
      INTEGER          J
 
C
C     Transform X to S and initialize temporary variables.
C
      S     = (X-X2S(1)) / X2S(2)
      S2    = 2.0D0 * S
      J     = DEGP  + 1
 
      DO I = 0,NDERIV
         PARTDP (1,I) = 0.D0
         PARTDP (2,I) = 0.D0
      END DO
 
C
C     Evaluate the polynomial ...
C
      DO WHILE ( J .GT. 1 )
 
         PARTDP (3,0) = PARTDP (2,0)
         PARTDP (2,0) = PARTDP (1,0)
         PARTDP (1,0) = CP (J)   + ( S2*PARTDP(2,0)  - PARTDP(3,0) )
 
C
C        ... and its derivatives using recursion.
C
         SCALE = 2.0D0
 
         DO I = 1, NDERIV
 
            PARTDP (3,I) =   PARTDP(2,I  )
            PARTDP (2,I) =   PARTDP(1,I  )
 
            PARTDP (1,I) =   PARTDP(2,I-1)*SCALE
     .                     + PARTDP(2,I  )*S2
     .                     - PARTDP(3,I  )
 
            SCALE        =   SCALE + 2.0D0
         END DO
 
         J        = J - 1
 
      END DO
 
      DPDXS(0) = CP(1) + ( S*PARTDP(1,0)  - PARTDP(2,0)  )
 
      SCALE    = 1.0D0
 
      DO I = 1, NDERIV
         DPDXS(I)  =   PARTDP(1,I-1)*SCALE
     .               + PARTDP(1,I  )*S
     .               - PARTDP(2,I  )
 
         SCALE  =   SCALE + 1
      END DO
 
C
C     Scale the k'th derivative w.r.t S by (1/X2S(2)**k) so that we have
C     the derivatives
C
C                    2          3          4          5
C        d P(S)     d P(S)     d P(S)     d P(S)     d P(S)
C        ------     ------     ------     ------     ------
C                       2          3          4          5
C          dX         dX         dX         dX         dX
C
C
C     NOTE: In the loop that follows we perform division instead of
C           multiplying by reciprocals so that the algorithm matches
C           CHBINT.  If multiplication by reciprocals is performed
C           CHBINT and CHBDER (although mathematically equivalent) will
C           not produce identical results for the first derivative.
C
C
      SCALE  = X2S(2)
 
      DO I = 1, NDERIV
         DPDXS(I) = DPDXS(I) / SCALE
         SCALE    = X2S(2)   * SCALE
      END DO
 
      RETURN
      END
