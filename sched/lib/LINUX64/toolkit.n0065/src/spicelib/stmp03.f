C$Procedure      STMP03 ( Stumpff functions 0 through 3 )
 
      SUBROUTINE STMP03 ( X, C0, C1, C2, C3 )
 
C$ Abstract
C
C     Compute the values of the Stumpff functions C_0 through C_3 at
C     a specified point.
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
C     CONIC
C     MATH
C     UTILITY
C
C$ Declarations
 
      DOUBLE PRECISION      X
      DOUBLE PRECISION      C0
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      C3
 
 
      INTEGER               TRUNC
      PARAMETER           ( TRUNC = 11   )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     X          I   Argument to each Stumpff function C_0 to C_3.
C     C0         O   Value of C_0(X)
C     C1         O   Value of C_1(X)
C     C2         O   Value of C_2(X)
C     C3         O   Value of C_3(X)
C     TRUNC      P   Number of terms needed in Maclaurin series for C_3.
C
C$ Detailed_Input
C
C     X          is the argument to use in each of the Stumpff functions
C                C_0, C_1, C_2, and C_3.
C
C$ Detailed_Output
C
C     C0         are the values of the Stumpff functions
C     C1         C_0(X), C_1(X), C_2(X), and C_3(X).
C     C2
C     C3
C
C$ Parameters
C
C     TRUNC     The Maclaurin series for C_3 and C_2 respectively are:
C
C                                      2     3                 k
C                         1     X     X     X              (-X)
C              C_3(X) =  --- - --- + --- - --- + . . . + ----------. . .
C                         3!    5!    7!    9!           (3 + 2*K)!
C
C               and
C
C                                      2     3                 k
C                         1     X     X     X              (-X)
C              C_2(X) =  --- - --- + --- - --- + . . . + ----------. . .
C                         2!    4!    6!    8!           (2 + 2*K)!
C
C               These series are used in the evaluation of C_3 and C_2.
C               Thus, it is necessary to make a decision about where to
C               truncate the series in our evaluation of C_3 and C_2.
C
C               TRUNC is used to tell this routine where to truncate
C               the Maclaurin series for C_3 and C_2.
C
C               The value of TRUNC for your machine  is the smallest
C               integer such that
C
C                                 1
C                   1.0D0  +  ----------        =  1.0D0
C                             (2*TRUNC)!
C
C               The following program will (if compiled and linked)
C               will produce the values of TRUNC for your machine.
C
C               INTEGER               TRUNC
C
C               DOUBLE PRECISION      DENOM
C               DOUBLE PRECISION      FACTR
C
C               DOUBLE PRECISION      X
C
C               DENOM = 2.0D0
C               FACTR = 2.0D0
C               TRUNC = 1
C
C               X      = 1.0D0 / DENOM
C
C               DO WHILE ( 1.0D0 + X .GT. 1.0D0 )
C                  DENOM = DENOM * (2.0D0+FACTR) * (1.0D0+FACTR)
C                  FACTR = FACTR +  2.0D0
C                  TRUNC = TRUNC +  1
C                  X     = 1.0D0 /  DENOM
C               END DO
C
C               WRITE (*,*) 'The value of TRUNC is: ', TRUNC
C
C               END
C
C$ Exceptions
C
C     1)  If the input value of X is not in the domain of values
C         for which the Stumpff functions can be computed, the error
C         SPICE(VALUEOUTOFRANGE) is signalled.
C
C         The range of valid inputs is from  -[ln(2) + ln(DPMAX)]**2
C         to DPMAX.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes the values of the Stumpff functions C_0,
C     C_1, C_2, and C_3 at the input X.
C
C     The Stumpff function C_k(X) for k = 0, 1, ... is given by the
C     series:
C
C                                 2        3                  m
C                1      X        X        X               (-X)
C     C_k(X) =  --- - ------ + ------ - ------ + . . . + ------- + . . .
C                k!   (k+2)!   (k+4)!   (k+6)!           (k+2m)!
C
C
C     These series converge for all real values of X.
C
C
C$ Examples
C
C      For positive X,
C
C         C_0(X)   =  COS ( DSQRT(X) )
C
C
C                     SIN ( DSQRT(X) )
C         C_1(X)   =  ---------------
C                           DSQRT(X)
C
C
C                     1 - COS ( DSQRT(X) )
C         C_2(X)   = ---------------------
C                            X
C
C
C
C                     1  -  SIN ( DSQRT(X) ) / DSQRT(X)
C         C_3(X)   =  ----------------------------------
C                               X
C
C      Thus the following block of code can be used to check this
C      routine for reasonableness:
C
C      INTEGER               I
C
C      DOUBLE PRECISION      X
C      DOUBLE PRECISION      ROOTX
C
C      DOUBLE PRECISION      TC0
C      DOUBLE PRECISION      TC1
C      DOUBLE PRECISION      TC2
C      DOUBLE PRECISION      TC3
C
C      DOUBLE PRECISION      C0
C      DOUBLE PRECISION      C1
C      DOUBLE PRECISION      C2
C      DOUBLE PRECISION      C3
C
C      DO I = 1, 10
C
C         X     = DBLE (I)
C         ROOTX = DSQRT(X)
C
C         TC0   = COS ( ROOTX )
C         TC1   = SIN ( ROOTX ) / ROOTX
C
C         TC2   = ( 1.0D0 - COS( ROOTX )         ) / X
C         TC3   = ( 1.0D0 - SIN( ROOTX ) / ROOTX ) / X
C
C         CALL STMP03 ( X, C0, C1, C2, C3 )
C
C         WRITE (*,*)
C         WRITE (*,*) 'Expected - Computed for X = ', X
C         WRITE (*,*)
C         WRITE (*,*) 'Delta C0 :', TC0 - C0
C         WRITE (*,*) 'Delta C1 :', TC1 - C1
C         WRITE (*,*) 'Delta C2 :', TC2 - C2
C         WRITE (*,*) 'Delta C3 :', TC3 - C3
C
C      END DO
C
C      END
C
C      You should expect all of the differences to be on the order of
C      the precision of the machine on which this program is executed.
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C     [1] `Fundamentals of Celestial Mechanics', Second Edition
C         by J.M.A. Danby;  Willman-Bell, Inc., P.O. Box 35025
C         Richmond Virginia;  pp 168-180
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     H.A. Neilan    (JPL)
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.27.0, 08-APR-2014 (NJB)
C
C        Updated in-line documentation and cleaned up
C        code following changes made in version 3.21.0.
C
C-    SPICELIB Version 3.26.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 3.25.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 3.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 3.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 3.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 3.21.0, 09-APR-2012 (WLT)
C
C        Code was updated to correct execessive round-off
C        errors in the case where |X| > 1.
C
C-    SPICELIB Version 3.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 3.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 3.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 3.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 3.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 3.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 3.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 3.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 3.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 3.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 3.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 3.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 3.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 3.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 3.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 3.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 3.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 3.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 3.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 3.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 3.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 3.0.0, 08-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN)
C
C       The file was modified to include values for other platforms.
C       Also, the file was formatted for use by the program that
C       creates the environment specific source files.
C
C-    SPICELIB Version 1.0.0, 17-FEB-1992 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Evaluate the first four Stumpff functions
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.27.0, 08-APR-2014 (NJB) (WLT)
C
C        In version 3.21.0, the routine was re-written to use the
C        standard trigonometric and hyperbolic trigonometric formulas
C        for the Stumpff functions for input arguments having magnitude
C        greater than or equal to 1. This was done to prevent loss of
C        accuracy for some input values.
C
C        In version 3.27.0, the code was cleaned up: unreachable code
C        was deleted, and comments were changed to match the updated
C        code.
C
C        The derivation of the argument mapping formulas has been
C        retained as an appendix in a comment section at the end of
C        this source file. These formulas may be used a future revision
C        of this routine.
C
C-    SPICELIB Version 3.0.0, 08-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.
C
C-    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN)
C
C       The file was modified to include values for other platforms.
C       Also, the file was formatted for use by the program that
C       creates the environment specific source files.
C
C-&
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      DPMAX
 
C
C     Local Parameters
C
 
      DOUBLE PRECISION      HALF
      PARAMETER           ( HALF  = 1.0D0 / 2.0D0 )
 
      DOUBLE PRECISION      EIGHTH
      PARAMETER           ( EIGHTH = 1.0D0 / 8.0D0 )
 
C
C     The integers NPAIRS, LPAIR2, and LPAIR3 are used to declare
C     space for Maclaurin series coefficients and for determining how
C     many terms of these series to use in the computation of
C     C_2 and C_3.
C
C     Here's what is supposed to be true.
C
C        1/(TRUNC*2)!  + 1.0D0 = 1.0D0
C
C     using this machine's double precision arithmetic.
C
C     We will map the input X to a value y between -1 and 1 and then
C     construct the values of the functions at X from their values at y.
C     Since we will only evaluate the series expansion for C_2 and C_3
C     for values of y between -1 and 1, its easy to show that we don't
C     need to consider terms in the series whose coefficients have
C     magnitudes less than or equal 1/(2*TRUNC)! .
C
C     If the value of TRUNC is 10, then the series expansions for
C     C_2(y) and C_3(y) are can be truncated as shown here:
C
C                                   2             7       8
C              .    1      y       y             y       y
C       C_3(y) =   --- -  ---  +  ---  +  ... - ---  +  ---
C                   3!     5!      7!           17!     19!
C
C
C                 1        y         y            y           y
C              = ---( 1 - --- ( 1 - --- (...( 1- ----- ( 1 - ----- )...)
C                2*3      4*5       6*7          16*17       18*19
C
C
C
C
C              .    1      y       y             y       y
C       C_2(y) =   --- -  ---  +  ---  +  ... + ---  -  ---
C                   2!     4!      6!           16!     18!
C
C
C                 1        y         y            y           y
C              = ---( 1 - --- ( 1 - --- (...( 1- ----- ( 1 - ----- )...)
C                1*2      3*4       5*6          15*16       17*18
C
C     As is evident from the above, we are going to need the
C     "reciprocal pairs"
C
C       1/(1*2),  1/(2*3),  1/(3*4), 1/(4*5), ...
C
C     The number of such fractions be computed directly from
C     TRUNC. LPAIR3 and LPAIR2 indicate which of these pairs
C     (counting 1/(1*2) as the first) will be the last one needed in
C     the evaluation of C_2 and C_3.
C
      INTEGER               NPAIRS
      PARAMETER           ( NPAIRS = 2*TRUNC - 2 )
 
      INTEGER               LPAIR3
      PARAMETER           ( LPAIR3 = NPAIRS      )
 
      INTEGER               LPAIR2
      PARAMETER           ( LPAIR2 = NPAIRS - 1  )
 
 
C
C     Local variables
C
      DOUBLE PRECISION      LBOUND
      DOUBLE PRECISION      PAIRS  ( NPAIRS )
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
 
      INTEGER               I
 
      LOGICAL               FIRST
 
C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  LBOUND
      SAVE                  PAIRS
 
C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /
 
 
C
C     We are going to need the numbers
C
C        1/(2*3), 1/(3*4), 1/(4*5), ...
C
C     but we don't want to compute them every time this routine is
C     called.  So the first time this routine is called we compute
C     them and put them in the array PAIRS for use on subsequent
C     calls. (This could be done via parameters, but computing them
C     at run time seems to have a better chance of being
C     easily maintained.)
C
C     In addition we will need to compute the lower bound for which
C     C_0,...,C_3 can be computed.  This lower bound is computed by
C     noting that C_0 has the largest magnitude of all the Stumpff
C     functions over the domain from -infinity to -1.  Moreover, in this
C     range
C
C        C_0(X) = Cosh( SQRT(-X) )
C
C     Thus the range of X for which the Stumpff functions can be
C     computed is bounded below by the value of X for which
C
C        Cosh ( SQRT(-X) ) = DPMAX
C
C     Which implies the lower bound for valid inputs is at
C
C        X = - ( DLOG ( 2.0 ) + DLOG( DPMAX ) ) ** 2
C
C          = - ( DLOG ( 2*N ) + DLOG ( DPMAX/N  ) ) ** 2
C
C     We point out the second formulation of the bound just in case
C     your compiler can't handle the computation of DLOG ( DPMAX ).
C     If this unfortunate situation should arise, complain to the
C     company that produces your compiler and in the code below
C     compute LBOUND using the second form above with N equal to
C     some large power of 2 (say 2**20).
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
 
         DO I = 1, NPAIRS
            PAIRS(I) = 1.0D0 / (DBLE(I)*DBLE(I+1))
         END DO
 
         Y      =   DLOG(2.0D0) + DLOG( DPMAX() )
         LBOUND = - Y*Y
 
      END IF
 
 
C
C     First we make sure that the input value of X is within the
C     range that we are confident we can use to compute the Stumpff
C     functions.
C
      IF ( X .LE. LBOUND ) THEN
 
         CALL CHKIN ( 'STMP03'                                     )
         CALL SETMSG( 'The input value of X must be greater than '
     .   //           '#.  The input value was #'                  )
         CALL ERRDP ( '#', LBOUND                                  )
         CALL ERRDP ( '#', X                                       )
         CALL SIGERR( 'SPICE(VALUEOUTOFRANGE)'                     )
         CALL CHKOUT( 'STMP03'                                     )
         RETURN
 
      END IF
 
C
C     From the definition of the Stumpff functions it can be seen that
C     C_0(X), C_1(X) are given by
C
C          COS ( DSQRT(X) )   and   SIN ( DSQRT(X) ) / DSQRT(X)
C
C     for positive X. Moreover, the series used to define them converges
C     for all real X.
C
C     These functions have a number of simple relationships that make
C     their computations practical.  Among these are:
C
C                         1
C          x*C_k+2(x) =  ---  -  C_k(x)
C                         k!
C
C
C
C                                 2
C           C_0(4x)   =  2*[ C_0(x) ]  -  1
C
C
C
C
C           C_1(4x)   =    C_1(x)*C_0(x)
C
C
C
C                                 2
C           C_2(4x)   =   [C_1(x)]  / 2
C
C
C
C
C           C_3(4x)   = [ C_2(x) + C_0(x)*C_3(x) ] / 4
C
C      These can be used to derive formulae for C_0(16x) ... C_3(16x)
C      that involve only C_0(x) ... C_3(x).  If we let
C
C                                      2
C                     Z        = C_0(x)  - 0.5
C
C      and
C
C                     W        = 2*C_0(x)*C_1(x)
C
C      then
C
C                                   2
C                     C_0(16x) = 8*Z  -  1
C
C
C                     C_1(16x) = W*Z
C
C
C                                 2
C                     C_2(16x) = W  / 8
C
C
C                                       2
C                                 C_1(x)  + Z*[C_2(x) + C_0(x)*C_3(x)]
C                     C_3(16x) =  ----------------------------------
C                                                  8
C
C
       IF ( X .LT. -1.D0 ) THEN
 
         Z  = SQRT(-X)
         C0 = COSH(Z)
         C1 = SINH(Z)/Z
         C2 = (1-C0)/X
         C3 = (1-C1)/X
         RETURN
 
      END IF
 
      IF ( X .GT. 1.D0 ) THEN
 
         Z  = SQRT(X)
         C0 = COS(Z)
         C1 = SIN(Z)/Z
         C2 = (1-C0)/X
         C3 = (1-C1)/X
         RETURN
 
      END IF
 
C
C     If the magnitude of X is less than or equal to 1, we compute
C     the function values directly from their power series
C     representations.
C
C
C     Compute C_3 of x :
C
C                                   2             7       8
C              .    1      x       x             x       x
C       C_3(x) =   --- -  ---  +  ---  +  ... - ---  +  ---
C                   3!     5!      7!           17!     19!
C
C
C                 1        x         x            x           x
C              = ---( 1 - --- ( 1 - --- (...( 1- ----- ( 1 - ----- )...)
C                2*3      4*5       6*7          16*17       18*19
C
C                 ^        ^         ^             ^           ^
C                 |        |         |             |           |
C                 |        |         |             |           |
C              PAIR(2)  PAIR(4)   PAIR(6)  ...  PAIR(16)    PAIR(18)
C
C     Assuming that we don't need to go beyond the term with 1/19!,
C     LPAIR3 will be 18.
C
 
      C3    = 1.0D0
 
      DO I  = LPAIR3, 4, -2
         C3 = 1.0D0 - X*PAIRS(I)*C3
      END DO
 
      C3    =           PAIRS(2)*C3
 
 
C
C     Compute C_2 of x  :
C
C        Here's how we do it.
C                                   2             7       8
C              .    1      x       x             x       x
C       C_2(x) =   --- -  ---  +  ---  +  ... + ---  -  ---
C                   2!     4!      6!           16!     18!
C
C
C                 1        x         x            x           x
C              = ---( 1 - --- ( 1 - --- (...( 1- ----- ( 1 - ----- )...)
C                1*2      3*4       5*6          15*16       17*18
C
C                 ^        ^         ^             ^           ^
C                 |        |         |             |           |
C                 |        |         |             |           |
C              PAIR(1)  PAIR(3)   PAIR(5)  ...  PAIR(15)    PAIR(17)
C
C     Assuming that we don't need to go beyond  the term with 1/18!,
C     LPAIR2 will be 17.
C
      C2    = 1.0D0
 
      DO I  = LPAIR2, 3, -2
         C2 = 1.0D0 - X*PAIRS(I)*C2
      END DO
 
      C2    =           PAIRS(1)*C2
 
C
C     Get C1 and C0 via the recursion formula:
C
C                         1
C          x*C_k+2(y) =  ---  -  C_k(x)
C                         k!
C
      C1 =  1.0D0 - X*C3
      C0 =  1.0D0 - X*C2
 
      RETURN
      END
 
 
 
C
C     Appendix
C     ========
C
C     The formulas below show how to relate Stumpff function values
C     at arbitrary points in the functions' domain to values at points
C     in the interval [-1, 1]. These formulas might be used to improve
C     accuracy of the computed function values. The original algorithm
C     of this routine attempted to do this, but was unsuccessful.
C
C
C     These functions have a number of simple relationships that make
C     their computations practical.  Among these are:
C
C                         1
C          x*C_k+2(x) =  ---  -  C_k(x)
C                         k!
C
C
C
C                                 2
C           C_0(4x)   =  2*[ C_0(x) ]  -  1
C
C
C
C
C           C_1(4x)   =    C_1(x)*C_0(x)
C
C
C
C                                 2
C           C_2(4x)   =   [C_1(x)]  / 2
C
C
C
C
C           C_3(4x)   = [ C_2(x) + C_0(x)*C_3(x) ] / 4
C
C      These can be used to derive formulae for C_0(16x) ... C_3(16x)
C      that involve only C_0(x) ... C_3(x).  If we let
C
C                                      2
C                     Z        = C_0(x)  - 0.5
C
C      and
C
C                     W        = 2*C_0(x)*C_1(x)
C
C      then
C
C                                   2
C                     C_0(16x) = 8*Z  -  1
C
C
C                     C_1(16x) = W*Z
C
C
C                                 2
C                     C_2(16x) = W  / 8
C
C
C                                       2
C                                 C_1(x)  + Z*[C_2(x) + C_0(x)*C_3(x)]
C                     C_3(16x) =  ----------------------------------
C                                                  8
C
C  [End]
C
