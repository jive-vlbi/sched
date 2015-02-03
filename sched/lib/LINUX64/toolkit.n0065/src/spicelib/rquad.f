C$Procedure      RQUAD ( Roots of a quadratic equation )
 
      SUBROUTINE RQUAD (  A,  B,  C,  ROOT1,  ROOT2  )
 
C$ Abstract
C
C     Find the roots of a quadratic equation.
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
C     MATH
C     POLYNOMIAL
C     ROOT
C
C$ Declarations
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      ROOT1 ( 2 )
      DOUBLE PRECISION      ROOT2 ( 2 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C     A          I   Coefficient of quadratic term.
C     B          I   Coefficient of linear term.
C     C          I   Constant.
C     ROOT1      O   Root built from positive discriminant term.
C     ROOT2      O   Root built from negative discriminant term.
C
C$ Detailed_Input
C
C     A,
C     B,
C     C              are the coefficients of a quadratic polynomial
C
C                         2
C                       Ax   +   Bx   +   C.
C
C$ Detailed_Output
C
C     ROOT1,
C     ROOT2         are the roots of the equation,
C
C                         2
C                       Ax   +   Bx   +   C   =  0.
C
C
C                   ROOT1 and ROOT2 are both arrays of length 2.  The
C                   first element of each array is the real part of a
C                   root; the second element contains the complex part
C                   of the same root.
C
C                   When A is non-zero, ROOT1 represents the root
C
C                                    _____________
C                                   /  2
C                      - B   +    \/  B    -   4AC
C                      ---------------------------
C                                    2A
C
C
C                   and ROOT2 represents the root
C
C                                    _____________
C                                   /  2
C                      - B   -    \/  B    -   4AC
C                      --------------------------- .
C                                    2A
C
C
C                   When A is zero and B is non-zero, ROOT1 and ROOT2
C                   both represent the root
C
C                      - C / B.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If the input coefficients A and B are both zero, the error
C          SPICE(DEGENERATECASE) is signalled.  The output arguments
C          are not modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     1)   Humor us and suppose we want to compute the "golden ratio."
C
C          The quantity r is defined by the equation
C
C             1/r = r/(1-r),
C
C          which is equivalent to
C
C              2
C             r   +  r  -  1  =  0.
C
C          The following code frament does the job.
C
C
C             C
C             C     Compute "golden ratio."  The root we want,
C             C
C             C                ___
C             C               /
C             C        -1 + \/  5
C             C        -----------,
C             C             2
C             C
C             C
C             C     is contained in ROOT1.
C             C
C
C                   CALL RQUAD ( 1.D0, 1.D0, -1.D0, ROOT1, ROOT2 )
C
C                   PRINT *, 'The "golden ratio" is ', ROOT1(1)
C
C
C     2)   The equation,
C
C              2
C             x   +  1  =  0
C
C          can be solved by the code fragment
C
C
C             C
C             C     Let's do one with imaginary roots just for fun.
C             C
C
C                   CALL RQUAD ( 1.D0,  0.D0,  1.D0,  ROOT1,  ROOT2 )
C
C                   PRINT *, 'ROOT1 is ', ROOT1
C                   PRINT *, 'ROOT2 is ', ROOT2
C
C          The printed results will be something like:
C
C
C             ROOT1 is 0.000000000000000    1.000000000000000
C             ROOT2 is 0.000000000000000   -1.000000000000000
C
C$ Restrictions
C
C     No checks for overflow of the roots are performed.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 10-JUL-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     roots of a quadratic equation
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      CON
      DOUBLE PRECISION      DISCRM
      DOUBLE PRECISION      LIN
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SQR
 
      LOGICAL               ZEROED
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RQUAD' )
      END IF
 
C
C     The degree of the equation is zero unless at least one of the
C     second or first degree coefficients is non-zero.
C
      IF (  ( A .EQ. 0.D0 )  .AND.  ( B .EQ. 0.D0 )  )  THEN
 
         CALL SETMSG ( 'Both 1st and 2nd degree coefficients are zero.')
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'RQUAD'                                         )
         RETURN
 
      END IF
 
C
C     If we can scale the coefficients without zeroing any of them out,
C     we will do so, to help prevent overflow.
C
      SCALE  = MAX ( ABS(A), ABS(B), ABS(C) )
 
 
      ZEROED =      (  ( A .NE. 0.D0 ) .AND. ( A / SCALE .EQ. 0.D0 )  )
     .         .OR. (  ( B .NE. 0.D0 ) .AND. ( B / SCALE .EQ. 0.D0 )  )
     .         .OR. (  ( C .NE. 0.D0 ) .AND. ( C / SCALE .EQ. 0.D0 )  )
 
 
      IF ( .NOT. ZEROED ) THEN
 
         SQR = A / SCALE
         LIN = B / SCALE
         CON = C / SCALE
 
      ELSE
 
         SQR = A
         LIN = B
         CON = C
 
      END IF
 
C
C     If the second-degree coefficient is non-zero, we have a bona fide
C     quadratic equation, as opposed to a linear equation.
C
      IF ( SQR .NE. 0.D0 ) THEN
C
C        Compute the discriminant.
C
         DISCRM = LIN**2  -  4.D0 * SQR * CON
 
C
C        A non-negative discriminant indicates that the roots are
C        real.
C
         IF ( DISCRM .GE. 0.D0 ) THEN
C
C           The imaginary parts of both roots are zero.
C
            ROOT1(2) = 0.D0
            ROOT2(2) = 0.D0
C
C           We can take advantage of the fact that CON/SQR is the
C           product of the roots to improve the accuracy of the root
C           having the smaller magnitude.  We compute the larger root
C           first and then divide CON/SQR by it to obtain the smaller
C           root.
C
            IF ( LIN .LT. 0.D0 ) THEN
C
C              ROOT1 will contain the root of larger magnitude.
C
               ROOT1(1) =  ( - LIN + DSQRT(DISCRM) )  /  ( 2.D0 * SQR )
 
               ROOT2(1) =  ( CON / SQR )  /   ROOT1(1)
 
            ELSE IF ( LIN .GT. 0.D0 ) THEN
C
C              ROOT2 will contain the root of larger magnitude.
C
               ROOT2(1) =  ( - LIN - DSQRT(DISCRM) )  /  ( 2.D0 * SQR )
 
               ROOT1(1) =  ( CON / SQR )  /   ROOT2(1)
 
            ELSE
C
C              The roots have the same magnitude.
C
               ROOT1(1)  =    DSQRT( DISCRM )  /  ( 2.D0 * SQR )
 
               ROOT2(1)  =  - ROOT1(1)
 
            END IF
 
C
C        The only other possibility is that the roots are complex.
C
         ELSE
C
C           The roots are complex conjugates, so they have equal
C           magnitudes.
C
            ROOT1(1)  =  -LIN                /   ( 2.D0 * SQR )
            ROOT1(2)  =   DSQRT( -DISCRM )   /   ( 2.D0 * SQR )
 
            ROOT2(1)  =   ROOT1(1)
            ROOT2(2)  =  -ROOT1(2)
 
         END IF
 
 
C
C     If the second-degree coefficient is zero, we actually have a
C     linear equation.
C
      ELSE IF ( LIN .NE. 0.D0 ) THEN
 
         ROOT1(1) =  - CON / LIN
         ROOT1(2) =    0.D0
 
C
C        We set the second root equal to the first, rather than
C        leaving it undefined.
C
         CALL MOVED ( ROOT1, 2, ROOT2 )
 
      END IF
 
 
      CALL CHKOUT ( 'RQUAD' )
      RETURN
      END
