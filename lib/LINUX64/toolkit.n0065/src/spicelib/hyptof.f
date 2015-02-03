 
C$Procedure      HYPTOF ( Hyperbolic time of flight )
 
      SUBROUTINE HYPTOF ( MA, ECC, F )
 
C$ Abstract
C
C     Solve the time of flight equation MA = e sinh(F) - F for the
C     hyperbolic eccentric anomaly F, given the mean anomaly, MA,
C     and the eccentricity, e.
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
C
C$ Declarations
 
      DOUBLE PRECISION      MA
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      F
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MA         I   Mean anomaly at epoch.
C     ECC        I   Eccentricity.
C     F          O   Hyperbolic eccentric anomaly.
C
C$ Detailed_Input
C
C     MA          is the hyperbolic mean anomaly of an orbiting body at
C                 some epoch t,
C
C                                         3 1/2
C                       MA = (t-T)(mu/(-a) )
C
C                 where T is the time of periapsis passage, a is
C                 the semi-major axis of the orbit, and mu is the
C                 gravitational parameter of the primary body.
C
C     ECC         is the eccentricity of the orbit.
C
C$ Detailed_Output
C
C     F           is the corresponding eccentric anomaly. This is the
C                 solution to the time of flight equation
C
C                       MA = e sinh(F) - F
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the eccentricity (ECC) is less than one, the error
C        'SPICE(WRONGCONIC)' is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Iterate to solve
C
C           f(F,MA,e) = e sinh(F) - F - MA = 0
C
C$ Examples
C
C     ELLTOF, HYPTOF, and PARTOF are used by CONICS.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1] Roger Bate, Fundamentals of Astrodynamics, Dover, 1971.
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 13-JUL-2007 (NJB)
C
C        Bug fix: MAXLOG is now saved.
C
C-    SPICELIB Version 3.0.0, 14-DEC-1994 (WLT)
C
C        A counter was placed in the loop which bisects to a
C        solution to the hyperbolic version of Kepler's equation.
C        This addition forces the loop to terminate.  On some platforms
C        the loop would not terminate without this additional
C        check.  This was due to the compiler performing tests on
C        extended precision registers.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 19-APR-1990 (WLT)
C
C        A bad initial guess at bracketing the solution to the
C        hyperbolic time of flight equation was corrected so that
C        floating point overflows are now avoided.  In addition, the
C        Newton's method used before has been replaced by simply
C        bisection.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     hyperbolic time of flight
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 3.0.0, 14-DEC-1994 (WLT)
C
C        A counter was placed in the loop which bisects to a
C        solution to the hyperbolic version of Kepler's equation.
C        This addition forces the loop to terminate.  On some platforms
C        the loop would not terminate without this additional
C        check.  This was due to the compiler performing tests on
C        extended precision registers.
C
C        This is not due to a bug in the algorithm but rather to
C        what NAIF feels is an error on the part of some compiler
C        vendors.  If the difference between two d.p. numbers is
C        zero to double precision we feel that that is the number
C        that should be used in subsequent statements---ESPECIALLY
C        in comparisons.  However, since we don't have control
C        over how compiler writers decide to compile code, we have
C        added the loop counter to guarantee that the loop solving
C        the hyperbolic Kepler's equation terminates.
C
C-    SPICELIB Version 2.0.0, 19-APR-1990 (WLT)
C
C        A bad initial guess at bracketing the solution to the
C        hyperbolic time of flight equation was corrected so that
C        floating point overflows are now avoided.  In addition, the
C        Newton's method used before has been replaced by simply
C        bisection.
C
C-    Beta Version 1.1.1, 27-JAN-1989 (IMU)
C
C        Examples section completed.
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
C
C        The routine now verifies that the eccentricity is in the
C        proper range---(1,+infinity)---before proceeding.
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DCBRT
 
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               COUNT
      INTEGER               MCOUNT
 
      DOUBLE PRECISION      MAXLOG
 
      DOUBLE PRECISION      M
 
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      MIDDLE
      DOUBLE PRECISION      MIDVAL
      DOUBLE PRECISION      UPPER
 
      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      LASTDF
 
      LOGICAL               FIRST

C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  MAXLOG

C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'HYPTOF' )
      END IF
 
      IF ( FIRST ) THEN
         FIRST  = .FALSE.
         MAXLOG = LOG ( DPMAX() )
      END IF
 
      IF ( ECC .LT. 1.D0 ) THEN
         CALL SIGERR ( 'SPICE(WRONGCONIC)' )
         CALL CHKOUT ( 'HYPTOF'            )
         RETURN
      END IF
 
C
C     For reasons of numerical stability, we have to intercept cases
C     where the mean anomaly is zero or negative (since log x is not
C     defined for non-positive x). If the mean anomaly is zero, the
C     eccentric anomaly is also zero (by inspection).
C
C     Since the function e sinh(F) - F is an odd function, we can
C     solve the equation ABS(MA) = e sinh(F) - F for F and get
C     the solution to MA = e sinh(F) - F by negating F if MA is
C     less than 0.
C
      IF ( MA .EQ. 0.D0 ) THEN
         F = 0.D0
         CALL CHKOUT ( 'HYPTOF' )
         RETURN
      ELSE
         M = ABS ( MA )
      END IF
 
 
C
C     The initial bounds for the eccentric anomaly F are determined
C     as follows:
C
C     For the value of F we seek,
C
C        M = e sinh F - F
C
C     Thus
C
C        M < e sinh F =    (e/2) { Exp(F) - Exp(-F)}
C
C     Hence
C
C        2 M                1
C        ---   < Exp(F) - -----
C         e               Exp(F)
C
C     which yields
C
C
C        2 M Exp(F)
C        ----------   < Exp(F)**2 - 1
C            e
C
C     and
C
C           M**2                2M Exp(F)     M**2
C       1 + ---- <  Exp(F)**2 - ---------  +  ---- = {Exp(F) - (M/e)}**2
C           e**2                   e          e**2
C
C
C     Therefore we must have one of the following be true.
C
C
C      SQRT( 1 + (M/e)**2 )  <  Exp(F) - (M/e)
C
C    or
C
C     - SQRT( 1 + (M/e)**2 )  >  Exp(F) - (M/e)
C
C    The second case implies that
C
C     0 > (M/e) - SQRT( 1 + (M/e)**2 ) > Exp(F)
C
C    but since Exp(F) > 0 for all F it must be the case that
C
C      (M/e) + SQRT( 1 + (M/e)**2 ) < Exp(F)
C
C
C    Hence
C
C       Log ( (M/e) + SQRT(1 + (M/e)**2) )  < F
C
C
C
C     Returning to our initial equation:
C
C        M = e sinh F - F
C
C                         3        5
C                        F        F
C          =  e ( F  +  ---   +  --- + ...   )  -  F
C                        3!       5!
C
C               3
C          >  eF / 6
C
C     Thus
C
C
C             3 __________
C        F <  \/  6M / e
C
C
C     Thus our solution must satisfy the inequalities
C
C
C                                                      3 __________
C      LOG ( (M/e) + SQRT(1 + (M/e)**2) )  <   F   <   \/  6M/e
C
C
C     In addition we know that the solution must lie somewhere
C     in the region between 0 and the maximum value of F for which
C     (e sinh F - F) can be computed.  This turns out to be
C     approximately LOG( DPMAX() / e ) = LOG(DPMAX()) - LOG(e) .
C
C
      LOWER = LOG ( (M/ECC)  +  DSQRT( 1.0D0 + (M/ECC)**2 ) )
      UPPER = MIN ( DCBRT(6.0D0*M/ECC), MAXLOG - LOG (ECC ) )
      UPPER = MAX ( LOWER, UPPER )
C
C     Perform some simple checks first to avoid problems with
C     convergence of the loop below.  If LOWER is zero, then
C     M/ECC is so small that when added to 1 it doesn't make
C     any difference ( dLOG/dt = 1 at 1 after all).  So in this
C     case we will just solve the linear portion of the
C     expansion of e SINH(F) - F = M
C
C
C     Now we simply perform bisection to locate the root.
C
      MIDDLE = MAX( LOWER, MIN( UPPER, 0.5D0*UPPER + 0.5D0*LOWER ) )
      MIDVAL = ECC * SINH(MIDDLE) - MIDDLE - M
 
      DIFF   = UPPER - LOWER
C
C     Finally pick a reasonable upper bound on the number of loop
C     iterations we shall need to perform.
C
      MCOUNT = 100
      COUNT  = 0
      DO WHILE ( DIFF .GT. 0.0D0 .AND. COUNT .LT. MCOUNT )
 
C
C        Move one of the endpoints to the middle.
C
         IF ( MIDVAL .GT. 0.0D0 ) THEN
 
            UPPER = MIDDLE
 
         ELSE IF ( MIDVAL .LT. 0.0D0 ) THEN
 
            LOWER = MIDDLE
 
         ELSE
 
            LOWER = MIDDLE
            UPPER = MIDDLE
 
         END IF
 
C
C        Compute the next middle point.
C
         MIDDLE = MAX( LOWER, MIN( UPPER, 0.5D0*UPPER + 0.5D0*LOWER ) )
         LASTDF = DIFF
         COUNT  = COUNT + 1
 
C
C        If we are on an endpoint, we are ready to call it quits.
C
         IF (      ( MIDDLE .EQ. LOWER )
     .        .OR. ( MIDDLE .EQ. UPPER ) ) THEN
 
            DIFF   = 0.0D0
 
         ELSE
 
            DIFF   = UPPER - LOWER
            MIDVAL = ECC * SINH ( MIDDLE ) - MIDDLE - M
 
         END IF
 
 
      END DO
 
C
C     Restore the proper sign, if necessary.
C
      IF ( MA .LT. 0 ) THEN
         F = - MIDDLE
      ELSE
         F =   MIDDLE
      END IF
 
 
      CALL CHKOUT ( 'HYPTOF' )
      RETURN
      END
 
 
