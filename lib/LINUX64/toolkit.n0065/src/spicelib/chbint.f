C$Procedure      CHBINT ( Interpolate a Chebyshev expansion )
 
      SUBROUTINE CHBINT ( CP, DEGP, X2S, X, P, DPDX )
 
C$ Abstract
C
C     Given the coefficients for the Chebyshev expansion of a
C     polynomial, this returns the value of the polynomial and its
C     derivative evaluated at the input X.
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
 
      DOUBLE PRECISION   CP  ( * )
      INTEGER            DEGP
      DOUBLE PRECISION   X2S ( 2 )
      DOUBLE PRECISION   X
      DOUBLE PRECISION   P
      DOUBLE PRECISION   DPDX
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      CP         I   NDEG+1 Chebyshev polynomial coefficients.
C      DEGP       I   Degree of polynomial.
C      X2S        I   Transformation parameters of polynomial.
C      X          I   Value for which the polynomial is to be evaluated
C      P          O   Value of the polynomial at X
C      DPDX       O   Value of the derivative of the polynomial at X
C
C$ Detailed_Input
C
C      CP         is an array of coefficients OF a polynomial with
C                 respect to the Chebyshev basis.  The polynomial to be
C                 evaluated is assumed to be of the form:
C
C                   CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
C
C                                    ... + CP(2)*T(1,S) + CP(1)*T(0,S)
C
C                 where T(I,S) is the I'th Chebyshev polynomial
C                 evaluated  at a number S whose double precision
C                 value lies between -1 and 1.  The value of S is
C                 computed from the input variables X2S(1), X2S(2) and X
C
C      DEGP       is the degree of the Chebyshev polynomial to be
C                 evaluated.
C
C      X2S        is an array of two parameters.  These parameters are
C                 used to transform the domain of the input variable X
C                 into the standard domain of the Chebyshev polynomial.
C                 X2S(1) should be a reference point in the domain of X;
C                 X2S(2) should be the radius by which points are
C                 allowed to deviate from the reference point and while
C                 remaining within the domain of X.  The value of
C                 X is transformed into the value S given by
C
C                           S = ( X - X2S(1) ) / X2S(2)
C
C                 Typically X2S(1) is the midpoint of the interval over
C                 which X is allowed to vary and X2S(2) is the radius of
C                 the interval.
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
C$ Detailed_Output
C
C      P          is the value of the polynomial to be evaluated.  It
C                 is given by
C
C                   CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
C
C                                    ... + CP(2)*T(1,S) + CP(1)*T(0,S)
C
C                 where T(I,S) is the I'th Chebyshev polynomial
C                 evaluated  at a number S = ( X - X2S(1) )/X2S(2)
C
C      DPDX       is the value of the derivative of the polynomial at X.
C                 It is given by
C
C                   1/X2S(2) [    CP(DEGP+1)*T'(DEGP,S)
C                               + CP(DEGP)*T'(DEGP-1,S) + ...
C                               .
C                               .
C                               .
C                           ... + CP(2)*T'(1,S)
C                               + CP(1)*T'(0,S) ]
C
C                 where T(I,S) and T'(I,S)  are the I'th Chebyshev
C                 polynomial and its derivative, respectively,
C                 evaluated  at a number S = ( X - X2S(1) )/X2S(2)
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine computes the value of a Chebyshev polynomial
C      expansion and the derivative of the expansion with respect to X.
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
C$ Examples
C
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
C           READ  (*,*) DEGP, ( CP(I), I = 1, DEG+1 ),  BEG, END
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
C              CALL CHBINT ( CP, DEGP, X2S, X, P, DPDX )
C
C              do something with the pairs (X,P) and (X,DPDX)
C
C              X = X + STEP
C
C           END DO
C
C$ Restrictions
C
C      One needs to be careful that the value (X-X2S(1)) / X2S(2) lies
C      between -1 and 1.  Otherwise, the routine may fail spectacularly
C      (for example with a floating point overflow).
C
C$ Exceptions
C
C     Error free
C
C     No tests are performed for exceptional values (DEGP negative,
C     etc.) This routine is expected to be used at a low level in
C     ephemeris evaluations. For that reason it has been elected as a
C     routine that will not participate in error handling.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      "Numerical Recipes -- The Art of Scientific Computing" by
C       William H. Press, Brian P. Flannery, Saul A. Teukolsky,
C       Willam T. Vetterling.  (See Clenshaw's Recurrance Formula)
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
C     interpolate a chebyshev expansion
C
C-&
 
 
 
C$ Revisions
C
C-    Beta Version 1.0.1, 30-DEC-1988 (WLT)
C
C     The Error free specification was added to the routine as
C     well as an explanation for this designation. Examples added.
C
C-&
 
 
C
C     Local variables
C
      INTEGER          J
      DOUBLE PRECISION W (3)
      DOUBLE PRECISION DW(3)
      DOUBLE PRECISION S
      DOUBLE PRECISION S2
C
C     Transform X to S and initialize temporary variables.
C
      S     = (X-X2S(1)) / X2S(2)
      S2    = 2.0D0 * S
      J     = DEGP  + 1
      W (1) = 0.D0
      W (2) = 0.D0
      DW(1) = 0.D0
      DW(2) = 0.D0
C
C     Evaluate the polynomial and its derivative using recursion.
C
      DO WHILE ( J .GT. 1 )
 
         W (3) = W (2)
         W (2) = W (1)
         W (1) = CP(J)  + ( S2*W(2)  - W(3) )
 
         DW(3) = DW(2)
         DW(2) = DW(1)
         DW(1) = W(2)*2.0D0 + DW(2)*S2 - DW(3)
 
         J     = J - 1
 
      END DO
 
      P    = CP(1) + ( S*W (1) - W (2) )
      DPDX =  W(1) + S*DW(1) - DW(2)
C
C     Scale the derivative by 1/X2S(2) so that we have the derivative
C
C                       d P(S)
C                       ------
C                         dX
C
      DPDX = DPDX / X2S(2)
 
      RETURN
      END
