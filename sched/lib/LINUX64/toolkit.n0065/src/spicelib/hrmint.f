C$Procedure HRMINT ( Hermite polynomial interpolation  )
 
      SUBROUTINE HRMINT ( N,  XVALS,  YVALS,  X,  WORK,  F,  DF )
      IMPLICIT NONE
 
C$ Abstract
C
C     Evaluate a Hermite interpolating polynomial at a specified
C     abscissa value.
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
      DOUBLE PRECISION      XVALS (   N )
      DOUBLE PRECISION      YVALS ( 2*N )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      WORK  ( 2*N, 2 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      DF
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of points defining the polynomial.
C     XVALS      I   Abscissa values.
C     YVALS      I   Ordinate and derivative values.
C     X          I   Point at which to interpolate the polynomial.
C     WORK      I-O  Work space array.
C     F          O   Interpolated function value at X.
C     DF         O   Interpolated function's derivative at X.
C
C$ Detailed_Input
C
C     N              is the number of points defining the polynomial.
C                    The arrays XVALS and YVALS contain N and 2*N
C                    elements respectively.
C
C     XVALS          is an array of length N containing abscissa values.
C
C     YVALS          is an array of length 2*N containing ordinate and
C                    derivative values for each point in the domain
C                    defined by FIRST, STEP,  and N.  The elements
C
C                       YVALS( 2*I - 1 )
C                       YVALS( 2*I     )
C
C                    give the value and first derivative of the output
C                    polynomial at the abscissa value
C
C                       XVALS(I)
C
C                    where I ranges from 1 to N.
C
C
C     WORK           is a work space array.  It is used by this routine
C                    as a scratch area to hold intermediate results.  
C
C
C     X              is the abscissa value at which the interpolating
C                    polynomial and its derivative are to be evaluated.
C
C$ Detailed_Output
C
C     F, 
C     DF             are the value and derivative at X of the unique
C                    polynomial of degree 2N-1 that fits the points and 
C                    derivatives defined by XVALS and YVALS.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If two input abscissas are equal, the error 
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
C     1)  Fit a 7th degree polynomial through the points ( x, y, y' )
C
C             ( -1,      6,       3 )
C             (  0,      5,       0 )
C             (  3,   2210,    5115 )
C             (  5,  78180,  109395 )
C
C         and evaluate this polynomial at x = 2.
C
C
C            PROGRAM TEST_HRMINT
C
C            DOUBLE PRECISION      ANSWER
C            DOUBLE PRECISION      DERIV
C            DOUBLE PRECISION      XVALS (4)
C            DOUBLE PRECISION      YVALS (8)
C            DOUBLE PRECISION      WORK  (8,2)
C            INTEGER               N
C
C            N         =   4
C
C            XVALS(1)  =      -1.D0
C            XVALS(2)  =       0.D0
C            XVALS(3)  =       3.D0
C            XVALS(4)  =       5.D0
C
C            YVALS(1)  =       6.D0
C            YVALS(2)  =       3.D0
C            YVALS(3)  =       5.D0
C            YVALS(4)  =       0.D0
C            YVALS(5)  =    2210.D0   
C            YVALS(6)  =    5115.D0   
C            YVALS(7)  =   78180.D0   
C            YVALS(8)  =  109395.D0   
C
C            CALL HRMINT ( N, XVALS, YVALS, 2.D0, WORK, ANSWER, DERIV )
C
C            WRITE (*,*) 'ANSWER = ', ANSWER
C            WRITE (*,*) 'DERIV  = ', DERIV
C            END
C
C
C        The returned value of ANSWER should be 141.D0, and the returned
C        derivative value should be 456.D0, since the unique 7th degree 
C        polynomial that fits these constraints is
C
C                     7       2
C           f(x)  =  x   +  2x  + 5
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
C     [2]  "Elementary Numerical Analysis---An Algorithmic Approach"
C           by S. D. Conte and Carl de Boor.  See p. 64.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 28-JAN-2014 (NJB) 
C
C        Fixed a few comment typos.
C
C-    SPICELIB Version 1.2.0, 01-FEB-2002 (NJB) (EDW)
C
C        Bug fix:  declarations of local variables XI and XIJ
C        were changed from DOUBLE PRECISION to INTEGER.
C        Note:  bug had no effect on behavior of this routine.
C
C-    SPICELIB Version 1.1.0, 28-DEC-2001 (NJB)
C
C        Blanks following final newline were truncated to 
C        suppress compilation warnings on the SGI-N32 platform. 
C      
C-    SPICELIB Version 1.0.0, 01-MAR-2000 (NJB)
C
C-&
 
 

C$ Index_Entries
C
C     interpolate function using Hermite polynomial
C     Hermite interpolation
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
      DOUBLE PRECISION      TEMP
 
      INTEGER               I
      INTEGER               J
      INTEGER               NEXT
      INTEGER               PREV
      INTEGER               THIS
      INTEGER               XI
      INTEGER               XIJ
 
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
 
         CALL CHKIN  ( 'HRMINT'                              )
         CALL SETMSG ( 'Array size must be positive; was #.' )
         CALL ERRINT ( '#',  N                               )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                  )
         CALL CHKOUT ( 'HRMINT'                              )
         RETURN
 
      END IF
 
C
C     Copy the input array into WORK.  After this, the first column
C     of WORK represents the first column of our triangular 
C     interpolation table. 
C
      DO I = 1, 2*N
         WORK(I,1) = YVALS(I)
      END DO
 
C
C     Compute the second column of the interpolation table: this
C     consists of the N-1 values obtained by evaluating the
C     first-degree interpolants at X. We'll also evaluate the
C     derivatives of these interpolants at X and save the results in
C     the second column of WORK. Because the derivative computations
C     depend on the function computations from the previous column in
C     the interpolation table, and because the function interpolation
C     overwrites the previous column of interpolated function values,
C     we must evaluate the derivatives first.
C

      DO I = 1, N-1
 
         C1     =  XVALS(I+1) - X
         C2     =  X          - XVALS(I)
         DENOM  =  XVALS(I+1) - XVALS(I)
 
 
         IF ( DENOM .EQ. 0.D0 ) THEN
         
            CALL CHKIN  ( 'HRMINT'                  )
            CALL SETMSG ( 'XVALS(#) = XVALS(#) = #' )
            CALL ERRINT ( '#',   I                  )
            CALL ERRINT ( '#',   I+1                )
            CALL ERRDP  ( '#',   XVALS(I)           )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'     )
            CALL CHKOUT ( 'HRMINT'                  )
            RETURN
            
         END IF

C  
C        The second column of WORK contains interpolated derivative 
C        values.
C
C        The odd-indexed interpolated derivatives are simply the input
C        derivatives.
C
         PREV = 2*I  - 1
         THIS = PREV + 1
         NEXT = THIS + 1
         
         WORK( PREV,  2 )  =    WORK( THIS, 1 )
 
C
C        The even-indexed interpolated derivatives are the slopes of
C        the linear interpolating polynomials for adjacent input 
C        abscissa/ordinate pairs.
C
         WORK( THIS, 2 ) = ( WORK(NEXT, 1) - WORK(PREV, 1) ) / DENOM
  
C
C        The first column of WORK contains interpolated function values.
C        The odd-indexed entries are the linear Taylor polynomials, 
C        for each input abscissa value, evaluated at X.
C  
         TEMP  = WORK(THIS, 1) * ( X - XVALS(I) )  +  WORK(PREV, 1)


         WORK( THIS, 1 ) = (   C1 * WORK( PREV, 1 )
     .                       + C2 * WORK( NEXT, 1 )  )  /  DENOM
 
         WORK( PREV, 1 ) =     TEMP
 
      END DO

C
C     The last column entries were not computed by the preceding loop; 
C     compute them now.
C 
      WORK ( 2*N - 1,  2 )  =    WORK( 2*N,     1 )
      WORK ( 2*N - 1,  1 )  =    WORK( 2*N,     1 ) * ( X - XVALS(N) )
     .                         + WORK( 2*N - 1, 1 )
 
C
C     Compute columns 3 through 2*N of the table.
C
      DO J = 2, 2*N-1
 
         DO I = 1, 2*N-J
C
C           In the theoretical construction of the interpolation table,
C           there are 2*N abscissa values, since each input abcissa 
C           value occurs with multiplicity two. In this theoretical
C           construction, the Jth column of the interpolation table
C           contains results of evaluating interpolants that span J+1
C           consecutive abscissa values.  The indices XI and XIJ below
C           are used to pick the correct abscissa values out of the 
C           physical XVALS array, in which the abscissa values are not
C           repeated.
C 
            XI       =   ( I     + 1 ) / 2
            XIJ      =   ( I + J + 1 ) / 2
 
            C1       =     XVALS(XIJ)   -   X
            C2       =     X            -   XVALS(XI)
 
            DENOM    =     XVALS(XIJ)   -   XVALS(XI)
 
            IF ( DENOM .EQ. 0.D0 ) THEN
               
               CALL CHKIN  ( 'HRMINT'                  )
               CALL SETMSG ( 'XVALS(#) = XVALS(#) = #' )
               CALL ERRINT ( '#',   XI                 )
               CALL ERRINT ( '#',   XIJ                )
               CALL ERRDP  ( '#',   XVALS(XI)          )
               CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'     )
               CALL CHKOUT ( 'HRMINT'                  )
               RETURN
               
            END IF

C
C           Compute the interpolated derivative at X for the Ith
C           interpolant. This is the derivative with respect to X of
C           the expression for the interpolated function value, which
C           is the second expression below. This derivative computation
C           is done first because it relies on the interpolated
C           function values from the previous column of the
C           interpolation table.
C
C           The derivative expression here corresponds to equation 
C           2.35 on page 64 in reference [2].
C 
            WORK(I,2) = (              C1  * WORK(I,   2) 
     .                    +            C2  * WORK(I+1, 2)
     .                    + ( WORK(I+1, 1) - WORK(I,   1) )  ) / DENOM
C
C           Compute the interpolated function value at X for the Ith 
C           interpolant. 
C 
            WORK(I,1) = ( C1 * WORK(I,1)  +  C2 * WORK(I+1,1) ) / DENOM
 
         END DO
 
      END DO
 
C
C     Our interpolated function value is sitting in WORK(1,1) at this 
C     point.  The interpolated derivative is located in WORK(1,2).
C
      F   =  WORK( 1, 1 )
      DF  =  WORK( 1, 2 )
 
      RETURN
      END
