C$Procedure      NEARPT ( Nearest point on an ellipsoid )
 
      SUBROUTINE NEARPT ( POSITN, A, B, C, NPOINT, ALT )
      IMPLICIT NONE
      
C$ Abstract
C
C     This routine locates the point on the surface of an ellipsoid
C     that is nearest to a specified position. It also returns the
C     altitude of the position above the ellipsoid.
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
C     ALTITUDE  
C     ELLIPSOID
C     GEOMETRY
C
C$ Declarations
 
      DOUBLE PRECISION      POSITN ( 3 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      NPOINT ( 3 )
      DOUBLE PRECISION      ALT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     POSITN     I   Position of a point in body-fixed frame.
C     A          I   Length of semi-axis parallel to x-axis.
C     B          I   Length of semi-axis parallel to y-axis.
C     C          I   Length on semi-axis parallel to z-axis.
C     NPOINT     O   Point on the ellipsoid closest to POSITN.
C     ALT        O   Altitude of POSITN above the ellipsoid.
C
C$ Detailed_Input
C
C     POSITN     3-vector giving the position of a point with respect
C                to the center of an ellipsoid. The vector is expressed
C                in a body-fixed reference frame. The semi-axes of the
C                ellipsoid are aligned with the x, y, and z-axes of the
C                body-fixed frame.
C
C     A          Length of the semi-axis of the ellipsoid that is
C                parallel to the x-axis of the body-fixed reference
C                frame.
C
C     B          Length of the semi-axis of the ellipsoid that is
C                parallel to the y-axis of the body-fixed reference
C                frame.
C
C     C          Length of the semi-axis of the ellipsoid that is
C                parallel to the z-axis of the body-fixed reference
C                frame.
C
C$ Detailed_Output
C
C     NPOINT     is the nearest point on the ellipsoid to POSITN.
C                NPOINT is a 3-vector expressed in the body-fixed
C                reference frame.
C
C     ALT        is the altitude of POSITN above the ellipsoid. If
C                POSITN is inside the ellipsoid, ALT will be negative
C                and have magnitude equal to the distance between
C                NPOINT and POSITN.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If any of the axis lengths A, B or C are non-positive, the
C        error SPICE(BADAXISLENGTH) will be signaled.
C
C     2) If the ratio of the longest to the shortest ellipsoid axis
C        is large enough so that arithmetic expressions involving its
C        squared value may overflow, the error SPICE(BADAXISLENGTH)
C        will be signaled.  
C        
C     3) If any of the expressions
C
C           A * ABS( POSITN(1) ) / m**2
C           B * ABS( POSITN(2) ) / m**2
C           C * ABS( POSITN(3) ) / m**2
C
C        where m is the minimum of { A, B, C }, is large enough so 
C        that arithmetic expressions involving these sub-expressions
C        may overflow, the error SPICE(INPUTSTOOLARGE) is signaled.
C
C     4) If the axes of the ellipsoid have radically different
C        magnitudes, for example if the ratios of the axis lengths vary
C        by 10 orders of magnitude, the results may have poor
C        precision. No error checks are done to identify this problem.
C
C     5) If the axes of the ellipsoid and the input point POSITN have
C        radically different magnitudes, for example if the ratio of
C        the magnitude of POSITN to the length of the shortest axis is
C        1.E25, the results may have poor precision. No error checks
C        are done to identify this problem.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Many applications of this routine are more easily performed
C     using the higher-level SPICELIB routine SUBPNT. This routine
C     is the mathematical workhorse on which SUBPNT relies.
C
C$ Examples
C
C     Example 1.
C
C     The code fragment below illustrates how you can use SPICELIB to
C     compute the apparent sub-earth point on the moon.
C
C     C
C     C     Load the ephemeris, leapseconds and physical constants
C     C     files first.  We assume the names of these files are
C     C     stored in the character variables SPK, LSK and
C     C     PCK.
C     C
C           CALL FURNSH ( SPK )
C           CALL FURNSH ( LSK )
C           CALL FURNSH ( PCK )
C
C     C
C     C     Get the apparent position of the moon as seen from the 
C     C     earth.  Look up this position vector in the moon 
C     C     body-fixed frame IAU_MOON. The orientation of the 
C     C     IAU_MOON frame will be computed at epoch ET-LT. 
C     C
C           CALL SPKPOS ( 'moon',  ET,    'IAU_MOON', 'LT+S', 
C          .              'earth', TRGPOS, LT                 )
C
C     C
C     C     Negate the moon's apparent position to obtain the 
C     C     position of the earth in the moon's body-fixed frame.
C     C 
C           CALL VMINUS ( TRGPOS, EVEC )
C           
C     C
C     C     Get the lengths of the principal axes of the moon.
C     C     Transfer the elements of the array RADII to the 
C     C     variables A, B, C to enhance readability.
C     C
C           CALL BODVRD (  'MOON',    'RADII', DIM, RADII )
C           CALL VUPACK (  RADII,  A,       B,   C     )
C
C     C
C     C     Finally get the point SUBPNT on the surface of the
C     C     moon closest to the earth --- the sub-earth point.
C     C     SUBPNT is expressed in the IAU_MOON reference frame.
C     C
C           CALL NEARPT ( EVEC, A, B, C, SUBPNT, ALT )
C
C
C     Example 2.
C
C     One can use this routine to define a generalization of GEODETIC
C     coordinates called GAUSSIAN coordinates of a triaxial body.  (The
C     name is derived from the famous Gauss-map of classical
C     differential geometry).  The coordinates are longitude,
C     latitude, and altitude.
C
C     We let the x-axis of the body fixed coordinate system point
C     along the longest axis of the triaxial body.  The y-axis points
C     along the middle axis and the z-axis points along the shortest
C     axis.
C
C     Given a point P, there is a point on the ellipsoid that is
C     closest to P, call it Q.  The latitude and longitude of P
C     are determined by constructing the outward pointing unit normal
C     to the ellipsoid at Q.  Latitude of P is the latitude that the
C     normal points toward in the body-fixed frame. Longitude is
C     the longitude the normal points to in the body-fixed frame.
C     The altitude is the signed distance from P to Q, positive if P
C     is outside the ellipsoid, negative if P is inside.
C     (the mapping of the point Q to the unit normal at Q is the
C     Gauss-map of Q).
C
C     To obtain the Gaussian coordinates of a point whose position
C     in body-fixed rectangular coordinates is given by a vector P,
C     the code fragment below will suffice.
C
C        CALL NEARPT ( P,    A, B, C, Q, ALT  )
C        CALL SURFNM (       A, B, C  Q, NRML )
C
C        CALL RECLAT ( NRML, R, LONG, LAT     )
C
C     The Gaussian coordinates are LONG, LAT, and ALT.
C
C
C$ Restrictions
C
C     See the Exceptions header section above.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.4.0, 27-JUN-2013 (NJB)
C
C        Updated in-line comments.
C
C     Last update was 04-MAR-2013 (NJB)
C
C        Bug fix: now correctly computes off-axis solution for
C        the case of a prolate ellipsoid and a viewing point
C        on the interior long axis.
C
C-    SPICELIB Version 1.3.1, 07-FEB-2008 (NJB)
C
C        Header update: header now refers to SUBPNT rather
C        than deprecated routine SUBPT.
C
C-    SPICELIB Version 1.3.0, 07-AUG-2006 (NJB)
C
C        Bug fix:  added initialization of variable SNGLPT to support
C                  operation under the Macintosh Intel Fortran
C                  compiler. Note that this bug did not affect
C                  operation of this routine on other platforms.
C
C-    SPICELIB Version 1.2.0, 15-NOV-2005 (EDW) (NJB)
C     
C        Various changes were made to ensure that all loops terminate.
C         
C        Bug fix:  scale of transverse component of error vector
C        was corrected for the exterior point case.
C
C        Bug fix:  non-standard use of duplicate arguments in VSCL
C        calls was corrected.
C         
C        Error checking was added to screen out inputs that might
C        cause numeric overflow.
C     
C        Replaced BODVAR call in examples to BODVRD.
C
C-    SPICELIB Version 1.1.1, 28-JUL-2003 (NJB) (CHA)
C
C        Various header corrections were made. 
C
C-    SPICELIB Version 1.1.0, 27-NOV-1990 (WLT)
C
C        The routine was substantially rewritten to achieve
C        more robust behavior and document the mathematics
C        of the routine.
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
C     distance from point to ellipsoid
C     nearest point on an ellipsoid
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB)
C
C        Bug fix:  added initialization of variable SNGLPT to support
C                  operation under the Macintosh Intel Fortran
C                  compiler. Note that this bug did not affect
C                  operation of this routine on other platforms. The
C                  statement referencing the uninitialized variable
C                  was:
C
C           IF ( INSIDE  .AND. (      SNGLPT .EQ. 2
C     .                          .OR. SNGLPT .EQ. 3 ) ) THEN
C
C        SNGLPT is uninitialized only if INSIDE is .FALSE.,
C        so the  value of the logical expression is not affected by
C        the uninitialized value of SNGLPT.
C
C        However, the Intel Fortran compiler for the Mac flags a runtime
C        error when the above code is exercised.  So SNGLPT is now 
C        initialized prior to the above IF statement.
C
C
C-    SPICELIB Version 1.2.0, 15-NOV-2005 (EDW) (NJB)
C
C        Bug fix:  scale of transverse component of error vector
C        was corrected for the exterior point case.
C        Replaced BODVAR call in examples to BODVRD.
C
C        Bug fix:  non-standard use of duplicate arguments in VSCL
C        calls was corrected.
C         
C        Various changes were made to ensure that all loops terminate.
C         
C        Error checking was added to screen out inputs that might
C        cause numeric overflow.
C
C        Removed secant solution branch from root-finding loop.
C        Although the secant solution sped up some root searches,
C        it caused large numbers of unnecessary iterations in others.
C
C        Changed the expression:
C
C           IF (       LAMBDA .EQ. LOWER
C     .          .OR.  LAMBDA .EQ. UPPER ) THEN
C
C        to
C
C           IF (      APPROX( LAMBDA, LOWER, CNVTOL )
C     .          .OR. APPROX( LAMBDA, UPPER, CNVTOL )  ) THEN
C
C        Use of APPROX eliminates the possibility of an infinite loop
C        when LAMBDA approaches to within epsilon of, but does not
C        equate to UPPER or LOWER. Infinite loops occurred under some
C        compiler's optimizations.
C
C        The loop also includes a check on number of iterations, 
C        signaling an error if the bisection loop uses more than 
C        MAXITR passes.
C
C        TOUCHD is now used to defeat extended-register usage in
C        cases where such usage may cause logic problems.
C
C        Some minor code changes were made to ensure that various
C        variables remain in their expected ranges.
C
C        A few code changes were made to enhance clarity.
C
C
C-    SPICELIB Version 1.1.0, 27-NOV-1990
C
C      The routine was nearly rewritten so that points
C      near the coordinate planes in the interior of the ellipsoid
C      could be handled without fear of floating point overflow
C      or divide by zero.
C
C      While the mathematical ideas involved in the original routine
C      are retained, the code is for the most part new.  In addition,
C      the new code has been documented far more extensively than was
C      NEARPT 1.0.0.
C
C
C-     Beta Version 2.0.0, 9-JAN-1989 (WLT)
C
C      Error handling added has been added for bad axes values.
C
C      The algorithm did not work correctly for some points inside
C      the ellipsoid lying on the plane orthogonal to the shortest
C      axis of the ellipsoid.  The problem was corrected.
C
C      Finally the algorithm was made slightly more robust and clearer
C      by use of SPICELIB routines and by normalizing the inputs.
C
C      Add an example to the header section.
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM

      LOGICAL               APPROX 
      LOGICAL               RETURN
  
C
C     Parameters
C
C
C     The convergence tolerance CNVTOL is used to terminate the
C     bisection loop when the solution interval is very small but
C     hasn't converged to length zero.  This situation can occur when
C     the root is extremely close to zero.
C
      DOUBLE PRECISION      CNVTOL
      PARAMETER           ( CNVTOL = 1.D-16 )


C
C     Various potentially large numbers we'll compute must not
C     exceed DPMAX()/MARGIN:
C
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D2 )
      
C
C     The parameter MAXSOL determines the maximum number of
C     iterations that will be performed in locating the
C     near point.  This must be at least 3.  To get strong
C     robustness in the routine, MAXSOL should be at least 4.
C
      INTEGER               MAXSOL
      PARAMETER           ( MAXSOL  = 6 )

C
C     MAXITR defines the maximum number of iterations allowed in
C     the bisection loop used to find LAMBDA.  If this loop requires
C     more than MAXITR iterations to achieve convergence, NEARPT
C     will signal an error.
C
C     On a PC/Linux/g77 platform, it has been observed that each 
C     bisection loop normally completes in fewer than 70 iterations.
C     MAXITR is used as a "backstop" to prevent infinite looping in
C     case the normal loop termination conditions don't take effect.
C     The value selected is based on the range of exponents for IEEE
C     double precision floating point numbers.
C
      INTEGER               MAXITR
      PARAMETER           ( MAXITR  = 2048 )

C
C     Length of lines in message buffer.
C
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 80 )

C
C     Local Variables
C
      CHARACTER*(MSGLEN)    MSSG   ( 7 )

      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      AXISQR ( 3 )
      DOUBLE PRECISION      BESTHT
      DOUBLE PRECISION      BESTPT ( 3 )
      DOUBLE PRECISION      COPY   ( 3 )
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DENOM2
      DOUBLE PRECISION      DENOM3
      DOUBLE PRECISION      EPOINT ( 3 )
      DOUBLE PRECISION      ERR    ( 3 )
      DOUBLE PRECISION      ERRP   ( 3 )
      DOUBLE PRECISION      FACTOR
      DOUBLE PRECISION      HEIGHT
      DOUBLE PRECISION      LAMBDA
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      NEWERR
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OLDERR
      DOUBLE PRECISION      ORIGNL ( 3 )
      DOUBLE PRECISION      PNORM
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      PRODCT
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      QLOWER
      DOUBLE PRECISION      QUPPER
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SIGN
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TEMP
      DOUBLE PRECISION      TERM   ( 3 )
      DOUBLE PRECISION      TOOBIG
      DOUBLE PRECISION      TLAMBD ( 3 )
      DOUBLE PRECISION      UPPER
 
      INTEGER               BAD
      INTEGER               I
      INTEGER               IORDER ( 3 )
      INTEGER               SNGLPT
      INTEGER               SOLUTN
      INTEGER               ITR
 
      LOGICAL               EXTRA
      LOGICAL               INSIDE
      LOGICAL               SOLVNG
      LOGICAL               TRIM
 
C
C     Saved variables
C
      SAVE                  MSSG
      
C
C     Initial values
C
      DATA                  MSSG / 'Axis A was nonpositive. ?',
     .                             'Axis B was nonpositive. ?',
     .                             'Axes A and B were nonpositive. ?',
     .                             'Axis C was nonpositive. ?',
     .                             'Axes A and C were nonpositive. ?',
     .                             'Axes B and C were nonpositive. ?',
     .                             'All three axes were nonpositive. ?'/
 
 
C
C     Here's what you can expect to find in the routine below.
C
C        Chapter 1.  Error and Exception Handling.
C
C        Chapter 2.  Mathematical background for the solution---the
C                    lambda equation.
C
C        Chapter 3.  Initializations for the main processing loop.
C
C        Chapter 4.  Mathematical Solution of the lambda equation.
C
C                    Section 4.1  Avoiding numerical difficulties.
C                    Section 4.2  Bracketing the root of the lambda
C                                 equation.
C                    Section 4.3  Refining the estimate of lambda.
C                    Section 4.4  Handling points on the central plane.
C
C        Chapter 5.  Decisions and initializations for sharpening
C                    the solution.
C
C        Chapter 6.  Clean up.
C
 
C
C     Error and Exception Handling.
C     ================================================================
C     ----------------------------------------------------------------
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NEARPT' )
      END IF
 
C
C     Check the axes to make sure that none of them is less than or
C     equal to zero. If one is, signal an error and return.
C
      BAD = 0
 
      IF ( A .LE. 0.0D0 ) THEN
         BAD = BAD + 1
      END IF
 
      IF ( B .LE. 0.0D0 ) THEN
         BAD = BAD + 2
      END IF
 
      IF ( C .LE. 0.0D0 ) THEN
         BAD = BAD + 4
      END IF
 
 
      IF ( BAD .GT. 0 ) THEN

         CALL SETMSG ( MSSG (BAD)                       )
         CALL ERRCH  ( '?', 'The A,B, and C axes were ' //
     .                      '#, #, and # respectively.' )
 
         CALL ERRDP  ( '#', A )
         CALL ERRDP  ( '#', B )
         CALL ERRDP  ( '#', C )
 
         CALL SIGERR ( 'SPICE(BADAXISLENGTH)' )
         CALL CHKOUT ( 'NEARPT' )
         RETURN

      END IF
 
 
 
C
C     Mathematical background for the solution---the lambda equation.
C     ================================================================
C     ----------------------------------------------------------------
C
C  
C     Here is the background and general outline of how this problem is
C     going to be solved.
C  
C     We want to find a point on the ellipsoid
C  
C  
C           X**2       Y**2       Z**2
C          ------  +  ------  +  ------  =  1
C           A**2       B**2       C**2
C  
C     that is closest to the input point POSITN.
C  
C         If one cares about the gory details, we know that such a
C         point must exist because the ellipsoid is a compact subset of
C         Euclidean 3-space and the distance function between the input
C         point and the ellipsoid is continuous. Since any continuous
C         function on a compact set actually achieves its minimum at
C         some point of the compact set, we are guaranteed that a
C         closest point exists. The closest point may not be unique if
C         the input point is inside the ellipsoid.
C
C     If we let NPOINT be a closest point to POSITN, then the line
C     segment joining POSITN to NPOINT is parallel to the normal to the
C     ellipsoid at NPOINT.  Moreover, suppose we let SEGMENT(P) be the
C     line segment that connects an arbitrary point P with POSITN.  It
C     can be shown that there is only one point P on the ellipsoid in
C     the same octant at POSITN such that the normal at P is parallel
C     to SEGMENT(P).
C  
C  
C         More gory details: A normal to a point (X,Y,Z)
C         on the ellipsoid is given by
C  
C           (X/A**2, Y/B**2, Z/C**2)
C  
C         Given a fixed LAMBDA, and allowing (X,Y,Z) to
C         range over all points on the ellipsoid, the set
C         of points
C  
C  
C                 LAMBDA*X      LAMBDA*Y      LAMBDA*Z
C           ( X + --------, Y + --------, Z + -------- )
C                   A**2          B**2          C**2
C  
C         describes another ellipsoid with axes having lengths
C  
C                  LAMBDA         LAMBDA        LAMBDA
C              A + ------ ,   B + ------ ,  C + ------  .
C                    A              B             C
C  
C  
C         Moreover, as long as LAMBDA > - MIN( A**2, B**2, C**2 )
C         none of these ellipsoids intersect.  Thus, as long as
C         the normal lines are not allowed to cross the coordinate plane
C         orthogonal to the smallest axis (called the central plane)
C         they do not intersect.
C  
C  
C         Finally every point that does not lie on the central plane
C         lies on one of the "lambda" ellipsoids described above.
C  
C         Consequently, for each point, P, not on the central plane
C         there is a unique point, NPOINT, on the ellipsoid, such that
C         the normal line at NPOINT also contains P and does not cross
C         the central plane.
C  
C  
C     From the above discussion we see that we can mathematically
C     solve the near point problem by finding a point NPOINT
C     on the ellipsoid given by the equation:
C  
C           X**2       Y**2       Z**2
C          ------  +  ------  +  ------  =  1
C           A**2       B**2       C**2
C  
C  
C     such that for some value of LAMBDA
C  
C          POSITN = NPOINT + LAMBDA*NORMAL(NPOINT).
C  
C     Moreover, if POSITN = (X_o,Y_o,Z_o) then LAMBDA must satisfy
C     the equation:
C  
C              2                   2                   2
C           X_o                 Y_o                 Z_o
C     -----------------  + ------------------  + ------------------ = 1
C                     2                     2                     2
C     ( A + LAMBDA/A )      ( B + LAMBDA/B )      ( C + LAMBDA/C )
C  
C  
C     and LAMBDA must be greater than -MIN(A**2,B**2,C**2)
C  
C  
C     Once LAMBDA is known, NPOINT can be computed from the equation:
C  
C          POSITN = NPOINT + LAMBDA*NORMAL(NPOINT).
C  
C  
C     The process of solving for LAMBDA can be viewed as selecting
C     that ellipsoid
C  
C                2                     2               2
C               x                     y               z
C          --------------- + ---------------- + ---------------  = 1
C                        2                  2                 2
C          (a + lambda/a)    ( b + lambda/b)    (c + lambda/c)
C  
C     that contains the input point POSITN.  For lambda = 0, this
C     ellipsoid is just the input ellipsoid.  When we increase
C     lambda we get a larger "inflated" ellipsoid.  When we
C     decrease lambda we get a smaller "deflated" ellipsoid.  Thus,
C     the search for lambda can be viewed as inflating or deflating
C     the input ellipsoid (in a specially prescribed manner) until
C     the resulting ellipsoid contains the input point POSITN.
C  
C     The mathematical solution laid out above, has some numeric
C     flaws.  However, it is robust enough so that if it is applied
C     repeatedly, we can converge to a good solution of the near point
C     problem.
C  
C     In the code that follows, we will first solve the lambda equation
C     using the original input point.  However, for points near the
C     central plane the solution we obtain may not lie on the
C     ellipsoid.  But, it should lie along the correct normal line.
C  
C     Using this first candidate solution, we find the closest point
C     to it on the ellipsoid.  This second iteration always produces
C     a point that is as close as you can get to the ellipsoid.
C     However, the normal at this second solution may not come as close
C     as desired to pointing toward the input position.  To overcome
C     this deficiency we sharpen the second solution.
C  
C     To sharpen a solution we use the computed near point, the
C     computed altitude of POSITN and the normal at the near point to
C     approximate POSITN. The difference between the approximated
C     position of POSITN and the input value of POSITN is called the
C     error vector. To get a sharpened solution we translate the
C     computed near point in the direction of the component of the
C     error vector orthogonal to the normal. We then find the
C     mathematical near point to our translated solution.
C  
C     The sharpening process is repeated until it no longer produces
C     an "improved" near point.
C  
C     At each step of this procedure, we must compute a solution to
C     the "lambda" equation in order to produce our next estimate of
C     the near point.  If it were possible to create a "private"
C     routine in FORTRAN that only this routine could access, we
C     would do it.  However, things being what they are, we have to
C     compute the lambda solution in a loop.  We keep track of which
C     refinement we are working on by counting the number of
C     lambda solutions that are computed.
C   
C
C     Initializations for the main processing loop
C     ================================================================
C     ----------------------------------------------------------------
C
C
C     Let the game begin!
C
C     First order the axes of the ellipsoid and corresponding
C     component of POSITN by the size of lengths of axes.  Knowing
C     which axes are smallest will simplify our task of computing
C     lambda when the time comes.
C
      AXIS(1)  = A
      AXIS(2)  = B
      AXIS(3)  = C
 
      CALL VEQU   ( POSITN,    POINT  )
 
      CALL ORDERD ( AXIS,   3, IORDER )

      CALL REORDD ( IORDER, 3, AXIS   )
      CALL REORDD ( IORDER, 3, POINT  )
 
C
C     Rescale everything so as to avoid underflows when squaring
C     quantities and copy the original starting point.
C
C     Be sure that this is UNDONE at the end of the routine.
C
      SCALE = 1.0D0/AXIS(1)
 
      CALL VSCLIP ( SCALE, AXIS   )
      CALL VSCLIP ( SCALE, POINT  )
      CALL VEQU   ( POINT, ORIGNL )

C
C     Save the norm of the scaled input point.
C
      PNORM = VNORM ( POINT )

C
C     The scaled axis lengths must be small enough so they can
C     be squared.
C     
      TOOBIG = SQRT ( DPMAX() / MARGIN )

C
C     Note the first axis has length 1.D0, so we don't check it.
C
      DO I = 2, 3

         IF ( AXIS(I) .GT. TOOBIG ) THEN

            CALL SETMSG ( 'Ratio of length of axis #* to length of ' //
     .                    'axis #* is *; this value may cause '      //
     .                    'numeric overflow.'                        )
            CALL ERRINT ( '*', IORDER(I)                             )
            CALL ERRINT ( '*', IORDER(1)                             )
            CALL ERRDP  ( '*', AXIS(I)                               )
            CALL SIGERR ( 'SPICE(BADAXISLENGTH)'                     )
            CALL CHKOUT ( 'NEARPT'                                   )
            RETURN

         END IF

      END DO

C
C     We also must limit the size of the products
C
C        AXIS(I)*POINT(I), I = 1, 3
C
C     We can safely check these by comparing the products of
C     the square roots of the factors to TOOBIG.
C
      DO I = 1, 3

         PRODCT = SQRT( AXIS(I) ) * SQRT ( ABS(POINT(I)) )

         IF ( PRODCT .GT. TOOBIG ) THEN

            CALL SETMSG ( 'Product of length of scaled axis #* and ' //
     .                    'size of corresponding scaled component '  //
     .                    'of POSITN is > *; these values '          //
     .                    'may cause numeric overflow.'              )
            CALL ERRINT ( '*', IORDER(I)                             )
            CALL ERRDP  ( '*', TOOBIG**2.D0                          )
            CALL SIGERR ( 'SPICE(INPUTSTOOLARGE)'                    )
            CALL CHKOUT ( 'NEARPT'                                   )
            RETURN

         END IF

      END DO
      
C
C     Compute the squared lengths of the scaled axes.
C
      AXISQR(1) = AXIS(1)*AXIS(1)
      AXISQR(2) = AXIS(2)*AXIS(2)
      AXISQR(3) = AXIS(3)*AXIS(3)
 
C
C     We will need to "solve" for the NEARPT at least 3 times.
C     SOLUTN is the counter that keeps track of how many times
C     we have actually solved for a near point.  SOLVNG indicates
C     whether we should continue solving for NEARPT.
C
      SNGLPT =  4
      SOLUTN =  1
      SOLVNG = .TRUE.
 
      DO WHILE ( SOLVNG ) 
C
C       Mathematical solution of the lambda equation.
C       ================================================================
C       ----------------------------------------------------------------
C  
C  
C        Make a stab at solving the mathematical problem of finding the
C        near point.  In other words, solve the lambda equation.
C  
C
C        Avoiding Numerical difficulties
C        -------------------------------
C
C        First make a copy of POINT, then to avoid numerical
C        difficulties later on, we will assume that any component of
C        POINT that is not sufficiently different from zero to
C        contribute to an addition to the corresponding component
C        of AXIS, is in fact zero.
C
         CALL VEQU ( POINT, COPY )
 
         DO I = 1, 3
 
            IF (      ( 0.5D0*POINT(I) + AXIS(I) .EQ.  AXIS(I) )
     .           .OR. ( 0.5D0*POINT(I) - AXIS(I) .EQ. -AXIS(I) ) )
     .      THEN
               POINT(I) = 0.0D0
            END IF
 
         END DO
 
C
C        OK. Next we set up the logical that indicates whether
C        the current point is inside the ellipsoid.
C
         INSIDE = .FALSE.
 
C
C        Bracketing the root of the lambda equation.
C        -------------------------------------------
C
C        Let (x,y,z) stand for (POINT(1), POINT(2), POINT(3)) and
C        let (a,b,c) stand for (AXIS(1),  AXIS(2),   AXIS(3)).
C
C        The main step in finding the near point is to find the
C        root of the lambda equation:
C
C                 2                     2               2
C                x                     y               z
C       0 = --------------- + ---------------- + ---------------  - 1
C                         2                  2                 2
C           (a + lambda/a)    ( b + lambda/b)    (c + lambda/c)
C
C
C        We let Q(lambda) be the right hand side of this equation.
C        To find the roots of the equation we determine
C        values of lambda that make Q greater than 0 and less than 0.
C        An obvious value to check is lambda = 0.
C
         Q = (POINT(1)/AXIS(1))**2
     .     + (POINT(2)/AXIS(2))**2
     .     + (POINT(3)/AXIS(3))**2
     .     - 1.0D0
 
C
C        On the first solution pass, we will determine the sign of
C        the altitude of the input POSITN
C
         IF ( SOLUTN .EQ. 1 ) THEN
 
            IF       ( Q .GE. 0 ) THEN
               SIGN =  1.0D0
            ELSE
               SIGN = -1.0D0
            END IF
 
         END IF
 
C
C        OK. Now for the stuff we will have to do on every solution
C        pass.
C
C        Below, LOWER and UPPER are the bounds on our independent
C        variable LAMBDA.  QLOWER and QUPPER are the values of Q
C        evaluated at LOWER and UPPER, respectively.  The root we
C        seek lies in the interval
C
C           [ LOWER, UPPER ]
C
C        At all points in the algorithm, we have, since Q is a 
C        decreasing function to the right of the first non-removable
C        singularity,
C
C           QLOWER > 0
C                  -
C
C           QUPPER < 0
C                  -
C        
C        We'll use bracketing to ensure that round-off errors don't
C        violate these inequalities.
C
C        The logical flag INSIDE indicates whether the point is
C        strictly inside the interior of the ellipsoid.  Points on the
C        surface are not considered to be inside.
C        
         IF ( Q .EQ. 0.0D0 ) THEN
C
C           In this case the point is already on the ellipsoid
C           (pretty lucky eh?)  We simply set our bracketing values,
C           QLOWER and QUPPER, to zero so that that bisection
C           loop won't ever get executed.
C
            QLOWER = 0.0D0
            QUPPER = 0.0D0
            LOWER  = 0.0D0
            UPPER  = 0.0D0
            LAMBDA = 0.0D0

            INSIDE = .FALSE.
 

         ELSE IF ( Q .GT. 0.0D0 ) THEN 
C
C           The input point is outside the ellipsoid (we expect that
C           this is the usual case).  We want to choose our lower
C           bracketing value so that the bracketing values for lambda
C           aren't too far apart.  So we just make sure that the largest
C           term of the expression for Q isn't bigger than 4.
C
            DO I = 1, 3
               TLAMBD(I) = ( 0.5D0*DABS(POINT(I)) - AXIS(I) ) * AXIS(I)
            END DO
 
            LOWER = MAX ( 0.0D0, TLAMBD(1), TLAMBD(2), TLAMBD(3) )
 
C
C           Choose the next value of lambda so that the largest term
C           of Q will be no more than 1/4.
C
            UPPER  = 2.0D0 * MAX ( DABS( AXIS(1)*POINT(1) ),
     .                             DABS( AXIS(2)*POINT(2) ),
     .                             DABS( AXIS(3)*POINT(3) ) )
            LAMBDA = UPPER


            INSIDE = .FALSE.

         ELSE
C
C           In this case the point POSITN is inside the ellipsoid.
C
            INSIDE = .TRUE.
 
C
C           This case is a bit of a nuisance. To solve the lambda
C           equation we have to find upper and lower bounds on
C           lambda such that one makes Q greater than 0, the other
C           makes Q less than 0.  Once the root has been bracketed
C           in this way it is a straightforward problem to find
C           the value of LAMBDA that is closest to the root we
C           seek.  We already know that for LAMBDA = 0, Q is negative.
C           So we only need to find a value of LAMBDA that makes
C           Q positive.  But... the expression for Q has singularities
C           at LAMBDA = -a**2, -b**2, and -c**2.
C
C           These singularities are not necessarily to be avoided.
C           If the numerator of one of the terms for Q is zero, we
C           can simply compute Q ignoring that particular term.  We
C           say that a singularity corresponding to a term whose
C           numerator is zero is a viable singularity.  By being
C           careful in our computation of Q, we can assign LAMBDA to
C           the value of the singularity. A singularity that is not
C           viable is called a true singularity.
C
C           By choosing LAMBDA just slightly greater than the largest
C           true singularity, we can bracket the root we seek.
C
C           First we must decide which singularity is the first true
C           one.
C
            SNGLPT = 4
 
            DO I = 3, 1, -1
 
               IF ( POINT(I) .NE. 0 ) THEN
                  SNGLPT = I
               END IF
 
            END DO
 
C
C           If there is a singular point, compute LAMBDA so that the
C           largest term of Q is equal to 4.
C
            IF ( SNGLPT .LE. 3 ) THEN
 
               DO I = 1, 3

                  IF ( POINT(I) .EQ. 0 ) THEN
                     TLAMBD(I) = -AXISQR(3)
                  ELSE
                     TLAMBD(I) = AXIS(I) * (   0.5D0*DABS(POINT(I))
     .                                       - AXIS(I)              )
                  END IF

               END DO
 
               LAMBDA = MAX ( TLAMBD(1), TLAMBD(2), TLAMBD(3) )
               LOWER  = LAMBDA
               UPPER  = MAX ( LOWER, 0.0D0 )
 

            ELSE
C
C              The point must be at the origin.  In this case
C              we know where the closest point is. WE DON'T have
C              to compute anything. It's just at the end of the
C              shortest semi-major axis.  However, since we
C              may have done some rounding off, we will make
C              sure that we pick the side of the shortest axis
C              that has the same sign as COPY(1).
C
C              We are going to be a bit sneaky here.  We know
C              where the closest point is so we are going to
C              simply make POINT and COPY equal to that point
C              and set the upper and lower bracketing bounds
C              to zero so that we won't have to deal with any
C              special cases later on.
C
               IF ( COPY(1) .LT. 0 ) THEN
                  POINT(1) = -AXIS(1)
                  COPY(1)  = -AXIS(1)
               ELSE
                  POINT(1) =  AXIS(1)
                  COPY(1)  =  AXIS(1)
               END IF
 
               COPY(2) = 0.0D0
               COPY(3) = 0.0D0
 
               UPPER   = 0.0D0
               LOWER   = 0.0D0
               LAMBDA  = 0.0D0
               Q       = 0.0D0
 
               INSIDE  = .FALSE.
 
            END IF
 
         END IF


C
C        OK. Now compute the value of Q at the two bracketing
C        values of LAMBDA.
C
         DO I = 1, 3
 
            IF ( POINT(I) .EQ. 0 ) THEN
 
               TERM(I) = 0.0D0
 
            ELSE 
C
C              We have to be a bit careful for points inside the
C              ellipsoid. The denominator of the factor we are going to
C              compute is ( AXIS + LAMBDA/AXIS ). Numerically this may
C              be too close to zero for us to actually divide POINT by
C              it.  However, since our solution algorithm for lambda
C              does not depend upon the differentiability of Q---in
C              fact it depends only on Q having the correct sign---we
C              can simply truncate its individual terms when we are in
C              danger of division overflows.

               DENOM = AXIS(I) + LAMBDA/AXIS(I)

               TRIM  = 0.5D0 * DABS( POINT(I) )   .GT.   DENOM
 
               IF ( INSIDE .AND. TRIM ) THEN
 
                 FACTOR = 2.0D0

               ELSE
C
C                 We don't expect DENOM to be zero here, but we'll
C                 check anyway.
C
                  IF ( DENOM .EQ. 0.D0 ) THEN

                     CALL SETMSG ( 'AXIS(#) + LAMBDA/AXIS(#) is zero.' )
                     CALL ERRINT ( '#',  I                             )
                     CALL ERRINT ( '#',  I                             )
                     CALL SIGERR ( 'SPICE(BUG)'                        )
                     CALL CHKOUT ( 'NEARPT'                            )
                     RETURN
 
                  END IF

                  FACTOR =  POINT(I) / DENOM

               END IF
 
               TERM(I) = FACTOR*FACTOR
 
            END IF

         END DO

 
         IF ( .NOT. INSIDE ) THEN
            QLOWER = Q
            QUPPER = TERM(1) + TERM(2) + TERM(3) - 1.0D0
         ELSE
            QUPPER = Q
            QLOWER = TERM(1) + TERM(2) + TERM(3) - 1.0D0
         END IF
 
C
C        Bracket QLOWER and QUPPER.
C
         QLOWER = MAX ( 0.D0,  QLOWER )
         QUPPER = MIN ( 0.D0,  QUPPER )


         LAMBDA = UPPER
         Q      = QUPPER
 
C
C        Refining the estimate of lambda
C        -------------------------------
C
C        Now find the root of Q by bisection.
C
         ITR = 0
C
C        Throughout this loop we'll use TOUCHD to avoid logic problems
C        that may be caused by extended precision register usage by
C        some compilers.
C
         DO WHILE ( TOUCHD(UPPER-LOWER) .GT. 0.D0 )

            ITR = ITR + 1

            IF ( ITR .GT. MAXITR ) THEN

               CALL SETMSG ('Iteration limit # exceeded in NEARPT. ' //
     .                      'A, B, C = # # #; POSITN = ( #, #, # ). '//
     .                      'LOWER = #; UPPER = #; UPPER-LOWER = #. '//
     .                      'Solution pass number = #.  This event ' //
     .                      'should never occur. Contact NAIF.'      )
               CALL ERRINT ( '#',  MAXITR                            )
               CALL ERRDP  ( '#',  A                                 )
               CALL ERRDP  ( '#',  B                                 )
               CALL ERRDP  ( '#',  C                                 )
               CALL ERRDP  ( '#',  POSITN(1)                         )
               CALL ERRDP  ( '#',  POSITN(2)                         )
               CALL ERRDP  ( '#',  POSITN(3)                         )
               CALL ERRDP  ( '#',  LOWER                             )
               CALL ERRDP  ( '#',  UPPER                             )
               CALL ERRDP  ( '#',  UPPER - LOWER                     )
               CALL SIGERR ( 'SPICE(BUG)'                            )
               CALL CHKOUT ( 'NEARPT'                                )
               RETURN

            END IF

C
C           Bracket LOWER, QLOWER, and QUPPER.
C
            LOWER  = MIN ( LOWER, UPPER  )
            QLOWER = MAX ( 0.D0,  QLOWER )
            QUPPER = MIN ( 0.D0,  QUPPER )

C
C           Depending upon how Q compares with Q at the
C           bracketing endpoints we adjust the endpoints
C           of the bracketing interval
C
            IF ( Q .EQ. 0 ) THEN
C
C              We've found the root.
C
               LOWER = LAMBDA
               UPPER = LAMBDA

            ELSE

               IF ( Q .LT. 0.D0 ) THEN

                  UPPER  = LAMBDA
                  QUPPER = Q

               ELSE
C
C                 We have Q > 0
C
                  LOWER  = LAMBDA
                  QLOWER = Q

               END IF
C
C              Update LAMBDA.
C
               LAMBDA = 0.5D0*LOWER + 0.5D0*UPPER

C
C              It's quite possible as we get close to the root for Q
C              that round off errors in the computation of the next
C              value of LAMBDA will push it outside of the current
C              bracketing interval.  Force it back in to the current
C              interval.
C
               LAMBDA = BRCKTD ( LAMBDA, LOWER, UPPER )
 
            END IF 
C
C           At this point, it's guaranteed that
C
C              LOWER  <  LAMBDA  <  UPPER
C                     -          -
C
C           If we didn't find a root, we've set LAMBDA to the midpoint
C           of the previous values of LOWER and UPPER, and we've moved
C           either LOWER or UPPER to the old value of LAMBDA, thereby
C           halving the distance between LOWER and UPPER.
C
C           If we are still at an endpoint, we might as well cash in
C           our chips.  We aren't going to be able to get away from the
C           endpoints.  Set LOWER and UPPER equal so that the loop will
C           finally terminate.
C
            IF (      APPROX( LAMBDA, LOWER, CNVTOL )
     .           .OR. APPROX( LAMBDA, UPPER, CNVTOL )  ) THEN
C
C              Make the decision as to which way to push
C              the boundaries, by selecting that endpoint
C              at which Q is closest to zero.
 
               IF ( DABS(QLOWER) .LT. DABS(QUPPER) ) THEN
                  UPPER = LOWER
               ELSE
                  LOWER = UPPER
               END IF
C
C              Since LOWER is equal to UPPER, the loop will terminate.
C 
            END IF
 
C
C           If LOWER and UPPER aren't the same, we compute the
C           value of Q at our new guess for LAMBDA.
C
            IF ( TOUCHD(UPPER-LOWER) .GT. 0 ) THEN
 
               DO I = 1, 3
 
                  IF ( POINT(I) .EQ. 0 ) THEN
 
                     TERM(I) = 0.0D0
 
                  ELSE
 
                     DENOM =  AXIS(I) + LAMBDA/AXIS(I)

                     TRIM  =  0.5D0 * DABS( POINT(I) )   .GT.   DENOM

                     IF ( INSIDE .AND. TRIM ) THEN

                        FACTOR = 2.0D0

                     ELSE
C
C                       We don't expect DENOM to be zero here, but we'll
C                       check anyway.
C
                        IF ( DENOM .EQ. 0.D0 ) THEN

                           CALL SETMSG ( 'AXIS(#) + LAMBDA/AXIS(#) ' //
     .                                   'is zero.'                  )
                           CALL ERRINT ( '#',  I                     )
                           CALL ERRINT ( '#',  I                     )
                           CALL SIGERR ( 'SPICE(BUG)'                )
                           CALL CHKOUT ( 'NEARPT'                    )
                           RETURN

                        END IF

                        FACTOR =  POINT(I) / DENOM

                     END IF
 
                     TERM(I) = FACTOR*FACTOR
 
                  END IF

               END DO
 
               Q  =  TOUCHD( TERM(1) + TERM(2) + TERM(3) - 1.0D0 )
 
            END IF
C
C           Q(LAMBDA) has been set unless we've already found
C           a solution.
C 
C           Loop back through the bracketing refinement code.
C
        END DO
 
C
C       Now we have LAMBDA, compute the nearest point based upon
C       this value.
C
        LAMBDA = LOWER
 
 
        DO I = 1, 3
 
           IF ( POINT(I) .EQ. 0.0D0 ) THEN

              SPOINT(I) = 0.0D0

           ELSE

              DENOM = 1.0D0 +  LAMBDA/AXISQR(I)
C
C             We don't expect that DENOM will be non-positive, but we
C             check for this case anyway.
C
              IF ( DENOM .LE. 0.D0 ) THEN
                 
                 CALL SETMSG ( 'Denominator in expression for ' //
     .                         'SPOINT(#) is #.'                )
                 CALL ERRINT ( '#', I                           )
                 CALL ERRDP  ( '#', DENOM                       )
                 CALL SIGERR ( 'SPICE(BUG)'                     )
                 CALL CHKOUT ( 'NEARPT'                         )
                 RETURN

              END IF

              SPOINT(I) = COPY(I) / DENOM

           END IF

        END DO
 
C
C       Handling points on the central plane.
C       -------------------------------------
C
C       I suppose you thought you were done at this point. Not
C       necessarily.  If POINT is INSIDE the ellipsoid and happens to
C       lie in the Y-Z plane, there is a possibility (perhaps even
C       likelihood) that the nearest point on the ellipsoid is NOT in
C       the Y-Z plane. we must consider this possibility next.
C
        IF ( INSIDE  .AND. (      SNGLPT .EQ. 2
     .                       .OR. SNGLPT .EQ. 3 ) ) THEN
C
C          There are two ways to get here. SNGLPT = 2 or SNGLPT = 3.
C          Fortunately these two cases can be handled simultaneously by
C          code. However, they are most easily understood if explained
C          separately.
C
C          Case 1.  SNGLPT = 2
C
C          The input to the lambda solution POINT lies in the Y-Z plane.
C          We have already detected one critical point of the
C          distance function to POINT restricted to the ellipsoid.
C          This point also lies in the Y-Z plane.  However, when
C          POINT lies on the Y-Z plane close to the center of the
C          ellipsoid, there may be a point that is nearest that does
C          not lie in the Y-Z plane. Assuming the existence of such a
C          point, (x,y,z) it must satisfy the equations
C
C                   lambda*x
C             x  +  --------  = POINT(1)  = 0
C                      a*a
C
C
C                    lambda*y
C             y  +  --------  = POINT(2)
C                      b*b
C
C
C                   lambda*z
C             z  +  --------  = POINT(3)
C                      c*c
C
C
C          Since we are assuming that this undetected solution (x,y,z)
C          does not have x equal to 0, it must be the case that
C
C          lambda = -a*a.
C
C          Because of this, we must have
C
C             y    =  POINT(2) / ( 1 - (a**2/b**2) )
C             z    =  POINT(3) / ( 1 - (a**2/c**2) )
C
C          The value of x is obtained by forcing
C
C             (x/a)**2 + (y/b)**2 + (z/c)**2 = 1.
C
C          This assumes of course that a and b are not equal. If a and
C          b are the same, then since POINT(2) is not zero, the
C          solution we found above by deflating the original ellipsoid
C          will find the near point.
C
C             (If a and b are equal, the ellipsoid deflates to a
C              segment on the z-axis when lambda = -a**2. Since
C              POINT(2) is not zero, the deflating ellipsoid must pass
C              through POINT before it collapses to a segment.)
C
C
C          Case 2.  SNGLPT = 3
C
C          The input to the lambda solution POINT lies on the Z-axis.
C          The solution obtained in the generic case above will
C          locate the critical point of the distance function
C          that lies on the Z.  However, there will also be
C          critical points in the X-Z plane and Y-Z plane.  The point
C          in the X-Z plane is the one to examine.  Why?  We are looking
C          for the point on the ellipsoid closest to POINT.  It must
C          lie in one of these two planes. But the ellipse of
C          intersection with the X-Z plane fits inside the ellipse
C          of intersection with the Y-Z plane.  Therefore the closest
C          point on the Y-Z ellipse must be at a greater distance than
C          the closest point on the X-Z ellipse.  Thus, in solving
C          the equations
C
C
C                   lambda*x
C             x  +  --------  = POINT(1)  = 0
C                      a*a
C
C
C                    lambda*y
C             y  +  --------  = POINT(2)  = 0
C                      b*b
C
C
C                   lambda*z
C             z  +  --------  = POINT(3)
C                      c*c
C
C
C          We have
C
C             lambda = -a*a,
C
C             y      =  0,
C
C             z      =  POINT(3) / ( 1 - (a**2/c**2) )
C
C          The value of x is obtained by forcing
C
C             (x/a)**2 + (y/b)**2 + (z/c)**2 = 1.
C
C          This assumes that a and c are not equal.  If
C          a and c are the same, then the solution we found above
C          by deflating the original ellipsoid, will find the
C          near point.
C
C             ( If a = c, then the input ellipsoid is a sphere.
C               The ellipsoid will deflate to the center of the
C               sphere.  Since our point is NOT at the center,
C               the deflating sphere will cross through
C               (x,y,z) before it collapses to a point )
C
C          We begin by assuming this extra point doesn't exist.
C
           EXTRA = .FALSE.
 
C
C          Next let's note a few simple tests we can apply to
C          eliminate searching for an extra point.
C
C          First of all the smallest axis must be different from
C          the axis associated with the first true singularity.
C
C
C          Next, whatever point we find, it must be true that
C
C               |y| < b, |z| < c
C
C          because of the condition on the absolute values, we must
C          have:
C
C               | POINT(2) | <= b - a*(a/b)
C
C               | POINT(3) | <= c - a*(a/c)
C

           IF (     (     AXIS (1)  .NE. AXIS(SNGLPT)                )
     .         .AND.(DABS(POINT(2)) .LE. AXIS(2) - AXISQR(1)/AXIS(2) )
     .         .AND.(DABS(POINT(3)) .LE. AXIS(3) - AXISQR(1)/AXIS(3) ))
     .     THEN
C
C             What happens next depends on whether the ellipsoid is
C             prolate or triaxial.
C
              IF ( AXIS(1) .EQ. AXIS(2) ) THEN
C
C                This is the prolate case; we need to compute the
C                z component of the solution.
C               
                 DENOM3 = 1.0D0 - AXISQR(1)/AXISQR(3)

                 IF ( DENOM3 .GT. 0.D0 ) THEN

                    EPOINT(2) = 0.D0
C
C                   Concerning the safety of the following division:
C
C                      - A divide-by-zero check has been done above.
C
C                      - The numerator can be squared without exceeding
C                        DPMAX(). This was checked near the start of the
C                        routine.
C
C                      - The denominator was computed as the difference
C                        of 1.D0 and another number. The smallest
C                        possible magnitude of a non-zero value of the
C                        denominator is roughly 1.D-16, assuming IEEE
C                        double precision numeric representation.
C
C
                    EPOINT(3) = POINT(3) / DENOM3
 
C
C                   See if these components can even be on the
C                   ellipsoid...
C   
                    TEMP      = 1.0D0  - ( EPOINT(2) / AXIS(2) )**2
     .                                 - ( EPOINT(3) / AXIS(3) )**2
 
                    IF ( TEMP .GT. 0 ) THEN
C
C                      ...and compute the x component of the point.
C
                       EPOINT(1) = AXIS(1) * DSQRT( MAX(0.D0,TEMP) )
                       EXTRA     = .TRUE.

                    END IF 

                 END IF

              ELSE
C
C                This is the triaxial case.
C
C                Compute the y and z components (2 and 3) of the extra
C                point.
C
                 DENOM2 = 1.0D0 - AXISQR(1)/AXISQR(2)
                 DENOM3 = 1.0D0 - AXISQR(1)/AXISQR(3)

C
C                We expect DENOM2 and DENOM3 will always be positive.
C                Nonetheless, we check to make sure this is the case.
C                If not, we don't compute the extra point.
C
                 IF (       ( DENOM2 .GT. 0.D0 ) 
     .                .AND. ( DENOM3 .GT. 0.D0 ) ) THEN

C
C                   Concerning the safety of the following divisions:
C
C                      - Divide-by-zero checks have been done above.
C
C                      - The numerators can be squared without exceeding
C                        DPMAX(). This was checked near the start of the
C                        routine.
C
C                      - Each denominator was computed as the difference
C                        of 1.D0 and another number. The smallest
C                        possible magnitude of a non-zero value of
C                        either denominator is roughly 1.D-16, assuming
C                        IEEE double precision numeric representation.
C

                    EPOINT(2) = POINT(2) / DENOM2
                    EPOINT(3) = POINT(3) / DENOM3
C
C                   See if these components can even be on the
C                   ellipsoid...
C   
                    TEMP      = 1.0D0  - ( EPOINT(2) / AXIS(2) )**2
     .                                 - ( EPOINT(3) / AXIS(3) )**2
 
                    IF ( TEMP .GT. 0 ) THEN
C
C                      ...and compute the x component of the point.
C
                       EPOINT(1) = AXIS(1) * DSQRT(TEMP)
                       EXTRA     = .TRUE.

                    END IF 

                 END IF

              END IF
 
           END IF

C
C          Ok.  If an extra point is possible, check and see if it
C          is the one we are searching for.
C
           IF ( EXTRA ) THEN
 
              IF (      VDIST(EPOINT, POINT)
     .             .LT. VDIST(SPOINT, POINT) ) THEN
 
                 CALL VEQU ( EPOINT, SPOINT )
 
              END IF
 
           END IF
 
        END IF

 
C
C       Decisions and initializations for sharpening the solution.
C       ================================================================
C       ----------------------------------------------------------------
C
        IF ( SOLUTN .EQ. 1 ) THEN
 
C  
C           The first solution for the nearest point may not be
C           very close to being on the ellipsoid.  To
C           take care of this case, we next find the point on the
C           ellipsoid, closest to our first solution point.
C           (Ideally the normal line at this second point should
C           contain both the current solution point and the
C           original point).
C  
            CALL   VEQU ( SPOINT, POINT  )
            CALL   VEQU ( SPOINT, BESTPT )
 
            BESTHT = VDIST ( BESTPT, ORIGNL )
 

         ELSE IF ( SOLUTN .EQ. 2 ) THEN 
C  
C           The current solution point will be very close to lying
C           on the ellipsoid.  However, the normal at this solution
C           may not actually point toward the input point.
C  
C           With the current solution we can predict
C           the location of the input point.  The difference between
C           this predicted point and the actual point can be used
C           to sharpen our estimate of the solution.
C  
C           The sharpening is performed by
C  
C              1) Compute the vector ERR that gives the difference
C                 between the input point (POSITN) and the point
C                 computed using the solution point, normal and
C                 altitude.
C  
C              2) Find the component of ERR that is orthogonal to the
C                 normal at the current solution point. If the point
C                 is outside the ellipsoid, scale this component to 
C                 the approximate scale of the near point.  We use
C                 the scale factor 
C
C                     ||near point|| / ||input point||
C
C                  Call this scaled component ERRP.
C  
C              3) Translate the solution point by ERRP to get POINT.
C  
C              4) Find the point on the ellipsoid closest to POINT.
C                 (step 4 is handled by the solution loop above).
C  
C
C           First we need to compute the altitude
C
            HEIGHT  = SIGN * VDIST( SPOINT, ORIGNL )
 
C
C           Next compute the difference between the input point and
C           the one we get by moving out along the normal at our
C           solution point by the computed altitude.
C
            CALL SURFNM ( AXIS(1), AXIS(2), AXIS(3), SPOINT, NORMAL )

            DO I=1, 3
            
               ERR(I) = ORIGNL(I) - SPOINT(I) - HEIGHT*NORMAL(I)

            END DO
 
C
C           Find the component of the error vector that is
C           perpendicular to the normal, and shift our solution
C           point by this component.
C
            CALL VPERP ( ERR, NORMAL, ERRP )

C
C           The sign of the original point's altitude tells
C           us whether the point is outside the ellipsoid.
C
            IF ( SIGN .GE. 0.D0 ) THEN
C
C              Scale the transverse component down to the local radius
C              of the surface point.
C
               IF ( PNORM .EQ. 0.D0 ) THEN
C
C                 Since the point is outside of the scaled ellipsoid,
C                 we don't expect to arrive here. This is a backstop
C                 check.
C
                  CALL SETMSG ( 'Norm of scaled point is 0. ' //
     .                          'POSITN = ( #, #, # )'        )
                  CALL ERRDP  ( '#',  POSITN(1)               )
                  CALL ERRDP  ( '#',  POSITN(2)               )
                  CALL ERRDP  ( '#',  POSITN(3)               )
                  CALL SIGERR ( 'SPICE(BUG)'                  )
                  CALL CHKOUT ( 'NEARPT'                      )
                  RETURN

               END IF

               CALL VSCLIP ( VNORM(SPOINT)/PNORM, ERRP )

            END IF

            CALL VADD ( SPOINT, ERRP, POINT )
 
            OLDERR = VNORM ( ERR )
            BESTHT = HEIGHT
 
C
C           Finally store the current solution point, so that if
C           this sharpening doesn't improve our estimate of the
C           near point, we can just return our current best guess.
C
            CALL VEQU ( SPOINT, BESTPT )
 
 
         ELSE IF ( SOLUTN .GT. 2 ) THEN            
C  
C           This branch exists for the purpose of testing our
C           "sharpened" solution and setting up for another sharpening
C           pass.
C  
C           We have already stored our best guess so far in BESTPT and
C           the vector ERR is the difference
C  
C              ORIGNL - ( BESTPT + BESTHT*NORMAL )
C  
C           We have just computed a new candidate "best" near point.
C           SPOINT.
C  
C           If the error vector
C  
C              ORIGNL - ( SPOINT + HEIGHT*NORMAL)
C  
C           is shorter than our previous error, we will make SPOINT
C           our best guess and try to sharpen our estimate again.
C  
C           If our sharpening method hasn't improved things, we just
C           call it quits and go with our current best guess.
C  
 
C
C           First compute the altitude,
C
            HEIGHT  = SIGN * VDIST( SPOINT, ORIGNL )
 
C
C           ... compute the difference
C
C              ORIGNL - SPOINT - HEIGHT*NORMAL,
C
            CALL SURFNM ( AXIS(1), AXIS(2), AXIS(3), SPOINT, NORMAL )

            DO I = 1, 3
            
               ERR(I) = ORIGNL(I) - SPOINT(I) - HEIGHT*NORMAL(I)

            END DO

C
C           ...and determine the magnitude of the error due to our
C           sharpened estimate.
C
            NEWERR = VNORM ( ERR )
 
C
C           If the sharpened estimate yields a smaller error ...
C
            IF ( NEWERR .LT. OLDERR ) THEN 
C
C              ...our current value of SPOINT becomes our new
C              best point and the current altitude becomes our
C              new altitude.
C
               OLDERR  =  NEWERR
               BESTHT  =  HEIGHT
 
               CALL  VEQU ( SPOINT,  BESTPT )
 
C
C              Next, if we haven't passed the limit on the number of
C              iterations allowed we prepare the initial point for our
C              "sharpening" estimate.
C
               IF ( SOLUTN .LE. MAXSOL ) THEN
 
                  CALL VPERP ( ERR, NORMAL, ERRP )
C
C                 The sign of the original point's altitude tells
C                 us whether the point is outside the ellipsoid.
C
                  IF ( SIGN .GE. 0.D0 ) THEN
C
C                    Scale the transverse component down to the local
C                    radius of the surface point.
C
                     IF ( PNORM .EQ. 0.D0 ) THEN
C
C                       Since the point is outside of the scaled
C                       ellipsoid, we don't expect to arrive here.
C                       This is a backstop check.
C
                        CALL SETMSG ( 'Norm of scaled point is 0. ' //
     .                                'POSITN = ( #, #, # )'        )
                        CALL ERRDP  ( '#',  POSITN(1)               )
                        CALL ERRDP  ( '#',  POSITN(2)               )
                        CALL ERRDP  ( '#',  POSITN(3)               )
                        CALL SIGERR ( 'SPICE(BUG)'                  )
                        CALL CHKOUT ( 'NEARPT'                      )
                        RETURN

                     END IF
           
                     CALL VSCLIP ( VNORM(SPOINT)/PNORM, ERRP )

                  END IF

                  CALL VADD  ( SPOINT, ERRP, POINT )

               END IF
 
            ELSE
C
C              If things didn't get better, there is no point in
C              going on.  Just set the SOLVNG flag to .FALSE. to
C              terminate the outer loop.
C
               SOLVNG = .FALSE.
 
            END IF
 
         END IF
 
C
C        Increment the solution counter so that eventually this
C        loop will terminate.
C
         SOLUTN = SOLUTN + 1
 
         SOLVNG = SOLVNG .AND. ( SOLUTN .LE. MAXSOL )
 
      END DO
 
C
C     Clean up.
C     ==================================================================
C     ------------------------------------------------------------------
C
C     Re-scale and re-order the components of our solution point. Scale
C     and copy the value of BESTHT into the output argument.
C
      CALL VSCLIP( 1.0D0/SCALE, BESTPT )
 
      DO I = 1,3
         NPOINT( IORDER(I) ) = BESTPT(I)
      END DO
 
      ALT = BESTHT/SCALE
 
      CALL CHKOUT ( 'NEARPT' )
      RETURN 
      END
