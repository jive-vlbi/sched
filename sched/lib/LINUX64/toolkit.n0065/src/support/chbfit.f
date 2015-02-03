C$Procedure      CHBFIT ( Chebyshev fit )
 
      SUBROUTINE CHBFIT ( FUNC, LEFT, RIGHT, N, WORK, COEFFS )
      IMPLICIT NONE
 
C$ Abstract
C
C     Return the Chebyshev coefficients for a Chebyshev expansion
C     of a specified function.
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
C     INTERPOLATION
C     MATH
C     POLYNOMIAL
C
C$ Declarations
 
      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = 25 )

      DOUBLE PRECISION      FUNC
      EXTERNAL              FUNC
 
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      INTEGER               N
      DOUBLE PRECISION      WORK   ( * )
      DOUBLE PRECISION      COEFFS ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MAXSIZ     P   Maximum number of terms in expansion.
C     FUNC       I   Function to be approximated.
C     LEFT       I   Left endpoint of approximation interval.
C     RIGHT      I   Right endpoint of approximation interval.
C     N          I   Number of terms in Chebyshev expansion.
C     WORK       I   Work space array of dimension N.
C     COEFFS     O   Coefficients of Chebyshev expansion.
C
C$ Detailed_Input
C
C     FUNC           is the function to be approximated.  FUNC must
C                    accept a single, double precision input argument
C                    and must return a double precision value.  FUNC
C                    should be declared EXTERNAL in the caller of this
C                    routine.
C
C     LEFT,
C     RIGHT          are, respectively, the left and right endpoints
C                    of the interval on which the input function is
C                    to be approximated.
C
C     N              is the number of terms in the desired Chebyshev
C                    expansion.  The degree of the highest-order
C                    Chebyshev polynomial in the expansion is N-1.
C
C     WORK           is a work space array of dimension N.
C
C
C$ Detailed_Output
C
C     COEFFS         is an array containing the coefficients of
C                    the N-term Chebyshev expansion of the input
C                    function.
C
C                    Let
C
C                       T (x)   =  cos ( j arccos(x) )
C                        j
C
C                    be the Chebyshev polynomial of degree j; then
C                    COEFFS are computed such that the expansion
C
C                        N
C                       ___
C                       \    COEFFS(j)  T   (x)
C                       /__              j-1
C
C                       j=1
C
C                    is the Chebyshev expansion of F(Y) on the
C                    interval [-1,1], where
C
C                       F(Y) =  FUNC(X)
C
C                    and
C
C                               X  -  (LEFT+RIGHT)/2
C                       Y    =  ---------------------
C                                 (LEFT-RIGHT) / 2
C
C                    The coefficients computed by this routine are
C                    compatible with the SPICELIB routines CHBINT,
C                    CHBVAL, and CHBDER.
C
C                    See the $Particulars section for further details
C                    on the specification of this routine.
C
C$ Parameters
C
C     MAXSIZ         is the maximum number of terms in the Chebyshev
C                    expansion.  This is the maximum allowed value of
C                    N.
C
C$ Exceptions
C
C     1)  If N is less than 1, the error SPICE(INVALIDSIZE) is
C         signaled.  The function will return the value 0.D0.
C
C     2)  If N is greater than MAXSIZ, the error SPICE(INVALIDSIZE) is
C         signaled.  The function will return the value 0.D0.
C
C     3)  This routine does not attempt to ward off or diagnose
C         arithmetic overflows.
C
C     4)  If the endpoints LEFT and RIGHT are not in strictly
C         increasing order, the error SPICE(INVALIDENDPTS)
C         is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The coefficient set produced by this routine is described below:
C
C        Let
C
C           x ,    k = 1, ... , N
C            k
C
C        be the roots of the Chebyshev polynomial
C
C           T (x)   =  cos ( N arccos(x) )
C            N
C
C        These roots are
C
C           cos ( (k-1/2)*PI/N ),    k = 1, ..., N.
C
C
C        For a function f(x) defined on the closed
C        interval [-1,1], the N-term Chebyshev expansion
C        is
C
C            N
C           ___
C           \    C  T   (x)
C           /__   j  j-1
C
C           j=1
C
C        where
C                         N
C                        ___
C           C  =  (2/N)  \   f(x ) T   (x ),  j = 2, ...,N,
C            j           /__    k   j-1  k
C
C                        k=1
C
C                         N
C                        ___
C           C  =  (1/N)  \   f(x )
C            1           /__    k
C
C                        k=1
C
C
C        The definition of
C
C           C
C            1
C
C        used differs from that used in reference [1];
C        our value is half theirs, and yields the simpler
C        expression for the expansion of f(x) shown above.
C
C        When the function f(x) to be approximated is
C        defined on the interval [LEFT,RIGHT], the mapping
C
C                     x  -  (LEFT+RIGHT)/2
C           y(x)  =  ---------------------
C                       (LEFT-RIGHT) / 2
C
C        can be used to define a new function F such that
C        F(y) = f(x).  F has domain [-1,1] and hence admits
C        a Chebyshev expansion.
C
C        In this routine, the above mapping is used to
C        transform the domain of the input function to the
C        interval [-1,1].
C
C
C$ Examples
C
C     1)  Recover coefficients from a function whose Chebyshev
C         expansion is known.  Suppose
C
C            f(x) = 1*T (x) + 2*T (x) + 3*T (x) + 4*T (x).
C                      0         1         2         3
C
C         The following small program produces the Chebyshev
C         coefficients of f:
C
C
C                  PROGRAM TSTCHB
C                  IMPLICIT NONE
C            C
C            C     Test Chebyshev fitting for a simple function.
C            C
C                  INTEGER               NCOEFF
C                  PARAMETER           ( NCOEFF = 4 )
C
C                  DOUBLE PRECISION      FUNC
C                  EXTERNAL              FUNC
C
C                  DOUBLE PRECISION      COEFFS ( NCOEFF )
C                  DOUBLE PRECISION      WORK   ( NCOEFF )
C                  INTEGER               I
C
C
C                  CALL CHBFIT ( FUNC,   -1.D0,  1.D0,
C                 .              NCOEFF,  WORK,  COEFFS )
C
C                  WRITE (*,*) 'Coefficients follow:'
C
C                  DO I = 1, NCOEFF
C                     WRITE (*,*) 'DEGREE: ', I-1, ' = ', COEFFS(I)
C                  END DO
C
C                  END
C
C
C                  DOUBLE PRECISION FUNCTION FUNC ( X )
C                  IMPLICIT NONE
C            C
C            C     Return
C            C
C            C        f(x) = 1*T (x) + 2*T (x) + 3*T (x) + 4*T (x).
C            C                  0         1         2         3
C            C
C                  DOUBLE PRECISION      X
C
C                  INTEGER               NCOEFF
C                  PARAMETER           ( NCOEFF = 4 )
C
C                  DOUBLE PRECISION      CP  ( NCOEFF )
C                  DOUBLE PRECISION      X2S ( 2 )
C                  INTEGER               I
C
C                  DO I = 1, NCOEFF
C                     CP(I) =  DBLE(I)
C                  END DO
C
C                  X2S(1) = 0.D0
C                  X2S(2) = 1.D0
C
C                  CALL CHBVAL ( CP, NCOEFF-1, X2S, X, FUNC )
C                  END
C
C$ Restrictions
C
C     1)  Maximum number of terms in the expansion is limited by the
C         parameter MAXSIZ.
C
C$ Literature_References
C
C     [1]  "Numerical Recipes---The Art of Scientific Computing" by
C           William H. Press, Brian P. Flannery, Saul A. Teukolsky,
C           William T. Vetterling (see section 5.6).
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SUPPORT Version 2.0.0, 14-SEP-2007 (NJB)
C
C        Now pre-computes Chebyvshev polynomial values.  Maximum
C        number of terms in the expansion is limited by the 
C        parameter MAXSIZ.
C
C-    SUPPORT Version 1.0.0, 16-JUN-1996 (NJB)
C
C-&
 
C$ Index_Entries
C
C     fit Chebyshev expansion to a function
C     determine Chebyshev coefficients of a function
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               MAX2
      PARAMETER           ( MAX2    = MAXSIZ * MAXSIZ )

      INTEGER               MAX3
      PARAMETER           ( MAX3    = MAXSIZ * MAX2   )

C
C     Local variables
C
      DOUBLE PRECISION      ARG
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RTAB   ( MAXSIZ, MAXSIZ ) 
      DOUBLE PRECISION      TTAB   ( MAXSIZ, MAXSIZ, MAXSIZ ) 
      DOUBLE PRECISION      X
 
      INTEGER               I
      INTEGER               J
      INTEGER               K
 
      LOGICAL               PASS1

C
C     Saved variables
C
      SAVE                  PASS1
      SAVE                  RTAB
      SAVE                  TTAB

C
C     Initial values
C
      DATA                  PASS1 / .TRUE. /


C
C     Check in only if an error is detected.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Make sure the requested expansion order is not too large.
C
      IF ( N .GT. MAXSIZ ) THEN

         CALL CHKIN  ( 'CHBFIT'                                   )
         CALL SETMSG ( 'The requested expansion order # exceeds ' //
     .                 'the maximum supported order #.'           )
         CALL ERRINT ( '#',  N                                    )
         CALL ERRINT ( '#',  MAXSIZ                               )
         CALL ERRINT ( '#',  N                                    )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                       )
         CALL CHKOUT ( 'CHBFIT'                                   )
         RETURN
         
      END IF
 
C
C     No data, no interpolation.
C
      IF ( N .LT. 1 ) THEN
 
         CALL CHKIN  ( 'CHBFIT'                              )
         CALL SETMSG ( 'Array size must be positive; was #.' )
         CALL ERRINT ( '#',  N                               )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                  )
         CALL CHKOUT ( 'CHBFIT'                              )
         RETURN
 
      END IF
 
C
C     Make sure the input interval is OK.
C
      IF ( LEFT .GE. RIGHT ) THEN
 
         CALL CHKIN  ( 'CHBFIT'                                 )
         CALL SETMSG ( 'Left endpoint = #; right endpoint = #.' )
         CALL ERRDP  ( '#',  LEFT                               )
         CALL ERRDP  ( '#',  RIGHT                              )
         CALL SIGERR ( 'SPICE(INVALIDENDPTS)'                   )
         CALL CHKOUT ( 'CHBFIT'                                 )
         RETURN
 
      END IF


      IF ( PASS1 ) THEN
C
C        On the first pass, compute a table of roots of all 
C        Cheby polynomials from degree 1 to degree N.  The Ith
C        column of the table contains roots of the Ith polynomial.
C
         CALL CLEARD ( MAX2, RTAB )

         DO I = 1, MAXSIZ

            DO K = 1, I
 
               RTAB(K,I) =  DCOS (  PI() * ( K - .5D0 ) / I  )
            
            END DO

         END DO

C
C        Also compute a table of Chebyshev function values.  For
C        each expansion size J from 1 to N, we compute the values
C        of 
C
C           T   (x ) ... T   ( x )  
C            0    1       0     J   
C
C                    .
C                    .
C                    .
C
C           T   (x ) ... T   ( x )  
C            J-1  1       J-1   J   
C
C        where
C
C           x
C            K
C
C        is the Kth root of 
C
C           T
C            J
C
C        In our 3-dimensional table, the (K,I,J) entry is the value
C        of 
C
C           T    ( x  )
C            I-1    K 
C
C        where 
C
C           x
C            K
C
C        is the Kth root of 
C
C           T
C            J
C

         CALL CLEARD ( MAX3, TTAB )


         DO J = 1, MAXSIZ
C
C           Compute Cheby values needed to implement an expansion
C           of size J.
C
            DO I = 1, J
C
C              Compute values of 
C
C                 T
C                  I-1
C
C              on the roots of 
C 
C                 T
C                  J
C
C
               DO K = 1, J
C
C                 Evaluate 
C
C                    T
C                     I-1
C
C                 at the Kth root of
C 
C                    T
C                     J
C                   
                  ARG          =  PI() * ( K - .5D0 ) / J

                  TTAB(K,I,J)  =  DCOS ( (I-1) * ARG )

               END DO

            END DO

         END DO

         PASS1 = .FALSE.

      END IF





C
C     Find the transformation parameters.
C
      MIDPT  =  ( RIGHT + LEFT ) / 2.D0
      RADIUS =  ( RIGHT - LEFT ) / 2.D0
 
C
C     Compute the input function values at the transformed Chebyshev
C     roots.
C
      DO K = 1, N
 
         X       =  RADIUS * RTAB(K,N)   +   MIDPT
         WORK(K) =  FUNC(X)
 
      END DO
 
 
C
C     Compute the coefficients.
C
      DO J = 1, N
 
         COEFFS(J) =  0.D0
 
         DO K = 1, N
 
            COEFFS(J) =  WORK(K) * TTAB(K,J,N)   +  COEFFS(J)
 
         END DO
 
         COEFFS(J) = 2.D0 * COEFFS(J) / N
 
      END DO
 
C
C     Scale the zero-order coefficient to simplify the form of the
C     Chebyshev expansion.
C
      COEFFS(1) = COEFFS(1) * 0.5D0
 
      RETURN
      END
 
