C$Procedure QDERIV ( Quadratic derivative )

      SUBROUTINE QDERIV ( N, F0, F2, DELTA, DFDT )
      IMPLICIT NONE 
 
C$ Abstract
C
C     Estimate the derivative of a function by finding the derivative
C     of a quadratic approximating function.  This derivative estimate
C     is equivalent to that found by computing the average of forward
C     and backward differences.
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
C     UTILITY
C
C$ Declarations
 
      INTEGER               N
      DOUBLE PRECISION      F0    ( N )
      DOUBLE PRECISION      F2    ( N )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DFDT  ( N )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     N          I   Dimension of function to be differentiated.
C     F0         I   Function values at left endpoint.
C     F2         I   Function values at right endpoint.
C     DELTA      I   Separation of abscissa points.
C     DFDT       O   Derivative vector.
C
C$ Detailed_Input
C
C     N              is the dimension of the function to be 
C                    differentiated.  The derivative of each 
C                    function component will be found.
C
C     F0             is an array of N function values at a point on
C                    the real line; we'll refer to this point as X0.
C
C     F2             is an array of N function values at a second point
C                    on the real line; we'll refer to this point as X2.
C                    The points X0 and X2 must satisfy
C
C                       X2 = X0 + 2 * DELTA
C                  
C
C     DELTA          is one half of the difference between X2 and X0:
C
C                       DELTA = ( X2 - X0 ) / 2
C   
C                    DELTA may be negative but must be non-zero.
C
C$ Detailed_Output
C
C     DFDT           is an N-dimensional vector representing an estimate
C                    of the derivative of the input function at the
C                    midpoint X1 of the interval between X0 and X2.
C
C                    The Ith component of DFDT is 
C
C                       ( 1 / (2*DELTA) ) * ( F2(I) - F0(I) )
C
C                    We may regard this estimate as the derivative
C                    at X1 of a parabola fitted to the points
C
C                        ( X0, F0(I) ),  ( X2, F2(I) )
C
C                    We may also regard this derivative as the average
C                    of the forward and backward first-order
C                    differences of the input function defined by
C                    F0(I), F2(I), and DELTA.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If DELTA is zero, the error SPICE(DIVIDEBYZERO) is signaled.
C   
C     2) If N is less than 1, this routine will fail in a system-
C        dependent manner.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine estimates the derivative of a vector-valued function
C     using the average of forward and backward differences.
C 
C     The derivative estimate computed by this routine is equivalent to
C     that obtained by fitting each component of the function with a
C     parabola at the points
C
C        (X0, F(X0)), (X1, F(X1)), (X2, F(X2))
C     
C     where 
C
C         X0  =  X1 - DELTA
C         X2  =  X1 + DELTA
C
C     and finding the derivative of the parabolas at X1.
C
C$ Examples
C
C     1) Estimate the derivative of x**2 at x = 2.
C
C           IMPLICIT NONE
C
C           DOUBLE PRECISION     DELTA
C           DOUBLE PRECISION     DFDT  (1)
C           DOUBLE PRECISION     F0    (1)
C           DOUBLE PRECISION     F2    (1)
C           INTEGER              N
C
C           N     = 1
C           DELTA = 1.D-3
C           F0(1) = ( 2.D0 - DELTA ) ** 2.D0
C           F2(1) = ( 2.D0 + DELTA ) ** 2.D0
C
C           CALL QDERIV ( N, F0, F2, DELTA, DFDT )
C
C           WRITE ( *, '(1X,A,E25.16)'  ) '4 - DFDT(1) = ', 4 - DFDT(1)
C           END
C
C        The difference displayed is platform-dependent, but
C        should be on the order of 1.E-12.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) 
C
C-&

C$ Index_Entries
C
C        Estimate function derivative using quadratic fit
C
C-&
 
C
C     Use discovery check-in.
C
      IF ( DELTA .EQ. 0.D0 ) THEN

         CALL CHKIN  ( 'QDERIV'                                    )
         CALL SETMSG ( 'Delta abscissa value is zero; a non-zero ' //
     .                 'value is required.'                        )
         CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                       )
         CALL CHKOUT ( 'QDERIV'                                    )
         RETURN

      END IF
C
C
C     Our derivative estimate is 
C
C            1/2 * (   Backward_difference / DELTA 
C                    + Forward_difference  / DELTA )
C
C        =   ( 1/(2*DELTA) ) * ( ( F(X2) - F(X1) ) +  ( F(X1) - F(X0) )
C
C        =   ( 1/(2*DELTA) ) * ( ( F(X2) - F(X0) )
C
C        =    (0.5/DELTA) * F(X2)  +  (-0.5/DELTA) * F(X0)
C
C
      CALL VLCOMG (  N, 0.5D0/DELTA, F2, -0.5D0/DELTA, F0, DFDT )

      RETURN
      END
