C$Procedure LGRIND (Lagrange polynomial interpolation with derivative)
 
      SUBROUTINE LGRIND ( N, XVALS, YVALS, WORK, X, P, DP )
 
C$ Abstract
C
C     Evaluate a Lagrange interpolating polynomial for a specified
C     set of coordinate pairs, at a specified abscissa value.
C     Return the value of both polynomial and derivative.
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
C     POLYNOMIAL
C
C$ Declarations
 
      INTEGER               N
      DOUBLE PRECISION      XVALS ( N )
      DOUBLE PRECISION      YVALS ( N )
      DOUBLE PRECISION      WORK  ( N, 2 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      P
      DOUBLE PRECISION      DP
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of points defining the polynomial.
C     XVALS      I   Abscissa values.
C     YVALS      I   Ordinate values.
C     WORK      I-O  Work space array.
C     X          I   Point at which to interpolate the polynomial.
C     P          O   Polynomial value at X.
C     DP         O   Polynomial derivative at X.
C
C$ Detailed_Input
C
C     N              is the number of points defining the polynomial.
C                    The arrays XVALS and YVALS contain N elements.
C
C
C     XVALS,
C     YVALS          are arrays of abscissa and ordinate values that
C                    together define N ordered pairs.  The set of points
C
C                       ( XVALS(I), YVALS(I) )
C
C                    define the Lagrange polynomial used for
C                    interpolation.  The elements of XVALS must be
C                    distinct and in increasing order.
C
C
C     WORK           is an N x 2 work space array, where N is the same
C                    dimension as that of XVALS and YVALS.  It is used
C                    by this routine as a scratch area to hold
C                    intermediate results.
C
C
C     X              is the abscissa value at which the interpolating
C                    polynomial is to be evaluated.
C
C$ Detailed_Output
C
C     P              is the value at X of the unique polynomial of
C                    degree N-1 that fits the points in the plane 
C                    defined by XVALS and YVALS.
C
C     DP             is the derivative at X of the interpolating
C                    polynomial described above.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If any two elements of the array XVALS are equal the error
C         SPICE(DIVIDEBYZERO) will be signaled.  
C
C     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is
C         signaled. 
C
C     3)  This routine does not attempt to ward off or diagnose
C         arithmetic overflows.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given a set of N distinct abscissa values and corresponding
C     ordinate values, there is a unique polynomial of degree N-1, often
C     called the `Lagrange polynomial', that fits the graph defined by
C     these values.  The Lagrange polynomial can be used to interpolate
C     the value of a function at a specified point, given a discrete
C     set of values of the function.
C
C     Users of this routine must choose the number of points to use
C     in their interpolation method.  The authors of Reference [1] have
C     this to say on the topic:
C
C        Unless there is solid evidence that the interpolating function
C        is close in form to the true function f, it is a good idea to
C        be cautious about high-order interpolation.  We
C        enthusiastically endorse interpolations with 3 or 4 points, we
C        are perhaps tolerant of 5 or 6; but we rarely go higher than
C        that unless there is quite rigorous monitoring of estimated
C        errors.
C
C     The same authors offer this warning on the use of the
C     interpolating function for extrapolation:
C
C        ...the dangers of extrapolation cannot be overemphasized:
C        An interpolating function, which is perforce an extrapolating
C        function, will typically go berserk when the argument x is
C        outside the range of tabulated values by more than the typical
C        spacing of tabulated points.
C
C$ Examples
C
C     1)  Fit a cubic polynomial through the points
C
C             ( -1, -2 )
C             (  0, -7 )
C             (  1, -8 )
C             (  3, 26 )
C
C         and evaluate this polynomial at x = 2.
C
C
C            PROGRAM TEST_LGRIND
C
C            DOUBLE PRECISION      P
C            DOUBLE PRECISION      DP
C            DOUBLE PRECISION      XVALS (4)
C            DOUBLE PRECISION      YVALS (4)
C            DOUBLE PRECISION      WORK  (4,2)
C            INTEGER               N
C
C            N         =   4
C
C            XVALS(1)  =  -1
C            XVALS(2)  =   0
C            XVALS(3)  =   1
C            XVALS(4)  =   3
C
C            YVALS(1)  =  -2
C            YVALS(2)  =  -7
C            YVALS(3)  =  -8
C            YVALS(4)  =  26
C
C            CALL LGRIND ( N, XVALS, YVALS, WORK, 2.D0, P, DP )
C
C            WRITE (*,*) 'P, DP = ', P, DP
C            END
C
C
C        The returned value of P should be 1.D0, since the
C        unique cubic polynomial that fits these points is
C
C                       3       2
C           f(x)   =   x   +  2x  - 4x  - 7
C
C
C        The returned value of DP should be 1.6D1, since the 
C        derivative of f(x) is
C
C             '         2      
C           f (x)  =  3x   +  4x  - 4
C
C
C        We also could have invoked LGRIND with the reference
C
C           CALL LGRIND ( N, XVALS, YVALS, YVALS, 2.D0, P, DP )
C
C        if we wished to; in this case YVALS would have been
C        modified on output.
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  "Numerical Recipes---The Art of Scientific Computing" by
C           William H. Press, Brian P. Flannery, Saul A. Teukolsky,
C           William T. Vetterling (see sections 3.0 and 3.1).
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-JAN-2014 (NJB)
C
C        Updated description of the workspace array: now the array WORK
C        is not described as being allowed to coincide with the input
C        YVALS. Such overlap would be a violation of the ANSI Fortran
C        77 standard. Corrected a spelling error in header
C        documentation.
C
C-    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB)
C
C-&

C$ Index_Entries
C     
C     interpolate function using Lagrange polynomial
C     Lagrange interpolation
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      DENOM
 
      INTEGER               I
      INTEGER               J
 
 
C
C     Check in only if an error is detected.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     No data, no interpolation.
C
      IF ( N .LT. 1 ) THEN
  
         CALL CHKIN  ( 'LGRIND'                              )
         CALL SETMSG ( 'Array size must be positive; was #.' )
         CALL ERRINT ( '#',  N                               )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                  )
         CALL CHKOUT ( 'LGRIND'                              )
         RETURN
 
      END IF
 
 
C
C     We're going to compute the value of our interpolating polynomial
C     at X by taking advantage of a recursion relation between
C     Lagrange polynomials of order n+1 and order n.  The method works
C     as follows:
C
C        Define
C
C           P               (x)
C            i(i+1)...(i+j)
C
C        to be the unique Lagrange polynomial that interpolates our
C        input function at the abscissa values
C
C           x ,  x   , ... x   .
C            i    i+1       i+j
C
C
C        Then we have the recursion relation
C
C           P              (x)  =
C            i(i+1)...(i+j)
C
C                                  x - x
C                                   i
C                                 -----------  *  P                (x)
C                                  x - x           (i+1)...(i+j)
C                                   i   i+j
C
C
C                                  x  -  x
C                                         i+j
C                               + -----------  *  P                (x)
C                                  x  -  x         i(i+1)...(i+j-1)
C                                   i     i+j
C
C
C        Repeated application of this relation allows us to build
C        successive columns, in left-to-right order, of the
C        triangular table
C
C
C           P (x)
C            1
C                    P  (x)
C                     12
C           P (x)             P   (x)
C            2                 123
C                    P  (x)
C                     23               .
C                             P   (x)
C           .                  234            .
C           .
C           .        .                               .
C                    .
C                    .        .                           P      (x)
C                             .                      .     12...N
C                             .
C                                             .
C
C                                      .
C
C
C                             P           (x)
C                              (N-2)(N-1)N
C                    P     (x)
C                     (N-1)N
C           P (x)
C            N
C
C
C        and after N-1 steps arrive at our desired result,
C
C
C           P       (x).
C            12...N
C
C
C     The computation is easier to do than to describe.
C
C
C     We'll use the scratch array WORK to contain the current column of
C     our interpolation table.  To start out with, WORK(I) will contain
C
C        P (x).
C         I
C
C     For columns 2...N of the table, we'll also carry along the
C     derivative at X of each interpolating polynomial.  This will
C     allow us to find the derivative of the Lagrange polynomial
C     at X.
C

      DO I = 1, N

         WORK(I,1)  =  YVALS(I)
         WORK(I,2)  =  0.D0

      END DO
 
C
C     Compute columns 2 through N of the table.  Note that DENOM must
C     be non-zero, or else a divide-by-zero error will occur.
C
      DO J = 1, N-1
 
         DO I = 1, N-J
 
            DENOM    =     XVALS(I)  -  XVALS(I+J)
 
            IF ( DENOM .EQ. 0.D0 ) THEN
 
               CALL CHKIN  ( 'LGRIND'                  )
               CALL SETMSG ( 'XVALS(#) = XVALS(#) = #' )
               CALL ERRINT ( '#',   I                  )
               CALL ERRINT ( '#',   I + J              )
               CALL ERRDP  ( '#',   XVALS(I)           )
               CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'     )
               CALL CHKOUT ( 'LGRIND'                  )
               RETURN
 
            END IF
 
            C1       =     X         -  XVALS(I+J)
            C2       =     XVALS(I)  -  X

C
C           Use the chain rule to compute the derivatives.  Do this
C           before computing the function value, because the latter
C           computation will overwrite the first column of WORK.
C 
            WORK(I,2) = (   ( C1*WORK(I,2) + C2*WORK(I+1,2) ) 
     .                    + (    WORK(I,1) -    WORK(I+1,1) )  ) / DENOM

C
C           Compute the Ith entry in the Jth column.
C           
            WORK(I,1) =     ( C1*WORK(I,1) + C2*WORK(I+1,1) )    / DENOM
 
         END DO
 
      END DO
 
C
C     Our results are sitting in WORK(1,1) and WORK(1,2) at this point.
C
      P  =  WORK(1,1)
      DP =  WORK(1,2)
 
      RETURN
      END
