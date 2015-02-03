C$Procedure DVSEP ( Derivative of separation angle )

      DOUBLE PRECISION FUNCTION DVSEP( S1, S2 )

C$ Abstract
C
C     Calculate the time derivative of the separation angle between
C     two input states, S1 and S2.
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
C     GEOMETRY
C     DERIVATIVES
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      S1 (6)
      DOUBLE PRECISION      S2 (6)

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S1         I   State vector of the first body.
C     S2         I   State vector of the second body.
C
C$ Detailed_Input
C
C     S1         the state vector of the first target body as seen from
C                the observer.
C
C     S2         the state vector of the second target body as seen from
C                the observer.
C
C     An implicit assumption exists that both states lie in the same
C     reference frame with the same observer for the same epoch. If this
C     is not the case, the numerical result has no meaning.
C
C$ Detailed_Output
C
C     The function returns the double precision value of the time
C     derivative of the angular separation between S1 and S2.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) The routine in the call tree of this routine signal errors for
C        numeric overflow and underflow cases.
C
C     2) If called in RETURN mode, the return has value 0.
C
C     3) Linear dependent position components of S1 and S1 constitutes
C        a non-error exception. The function returns 0 for this case.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     In this discussion, the notation
C
C        < V1, V2 >
C
C     indicates the dot product of vectors V1 and V2. The notation
C
C        V1 x V2
C
C     indicates the cross product of vectors V1 and V2.
C
C     To start out, note that we need consider only unit vectors,
C     since the angular separation of any two non-zero vectors
C     equals the angular separation of the corresponding unit vectors.
C     Call these vectors U1 and U2; let their velocities be V1 and V2.
C
C     For unit vectors having angular separation
C
C        THETA
C
C     the identity
C
C        || U1 x U1 || = ||U1|| * ||U2|| * sin(THETA)                (1)
C
C     reduces to
C
C        || U1 x U2 || = sin(THETA)                                  (2)
C
C     and the identity
C
C        | < U1, U2 > | = || U1 || * || U2 || * cos(THETA)           (3)
C
C     reduces to
C
C        | < U1, U2 > | = cos(THETA)                                 (4)
C
C     Since THETA is an angular separation, THETA is in the range
C
C        0 : Pi
C
C     Then letting s be +1 if cos(THETA) > 0 and -1 if cos(THETA) < 0,
C     we have for any value of THETA other than 0 or Pi
C
C
C                                  2          1/2
C        cos(THETA) = s * ( 1 - sin (THETA)  )                       (5)
C
C     or
C
C                                  2          1/2
C        < U1, U2 > = s * ( 1 - sin (THETA)  )                       (6)
C
C
C     At this point, for any value of THETA other than 0 or Pi,
C     we can differentiate both sides with respect to time (T)
C     to obtain
C
C                                                      2        -1/2
C        < U1, V2 > + < V1, U2 > =    s * (1/2)(1 - sin (THETA))
C
C                                   * (-2) sin(THETA)*cos(THETA)
C
C                                   * d(THETA)/dT                   (7a)
C
C
C     Using equation (5), and noting that s = 1/s, we can cancel
C     the cosine terms on the right hand side
C
C                                                      -1
C        < U1, V2 > + < V1, U2 > =    (1/2)(cos(THETA))
C
C                                   * (-2) sin(THETA)*cos(THETA)
C
C                                   * d(THETA)/dT                   (7b)
C
C     With (7b) reducing to
C
C        < U1, V2 > + < V1, U2 > = - sin(THETA) * d(THETA)/dT        (8)
C
C     Using equation (2) and switching sides, we obtain
C
C        || U1 x U2 || * d(THETA)/dT  =  - < U1, V2 > - < V1, U2 >   (9)
C
C     or, provided U1 and U2 are linearly independent,
C
C        d(THETA)/dT = ( - < U1, V2 > - < V1, U2 > ) / ||U1 x U2||  (10)
C
C     Note for times when U1 and U2 have angular separation 0 or Pi
C     radians, the derivative of angular separation with respect to
C     time doesn't exist. (Consider the graph of angular separation
C     with respect to time; typically the graph is roughly v-shaped at
C     the singular points.)
C
C$ Examples
C
C           PROGRAM DVSEP_T
C           IMPLICIT              NONE
C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      DSEPT
C           DOUBLE PRECISION      STATEE (6)
C           DOUBLE PRECISION      STATEM (6)
C
C           INTEGER               STRLEN
C           PARAMETER           ( STRLEN = 64 )
C
C           CHARACTER*(STRLEN)    BEGSTR
C
C           DOUBLE PRECISION      DVSEP
C
C     C
C     C     Load kernels.
C     C
C           CALL FURNSH ('standard.tm')
C
C     C
C     C     An arbitrary time.
C     C
C           BEGSTR = 'JAN 1 2009'
C           CALL STR2ET( BEGSTR, ET )
C
C     C
C     C     Calculate the state vectors sun to Moon, sun to earth at ET.
C     C
C     C
C           CALL SPKEZR ( 'EARTH', ET, 'J2000', 'NONE', 'SUN',
C          .               STATEE, LT)
C
C           CALL SPKEZR ( 'MOON', ET, 'J2000', 'NONE', 'SUN',
C          .               STATEM, LT)
C
C     C
C     C     Calculate the time derivative of the angular separation of
C     C     the earth and Moon as seen from the sun at ET.
C     C
C           DSEPT = DVSEP( STATEE, STATEM )
C           WRITE(*,*) 'Time derivative of angular separation: ', DSEPT
C
C           END
C
C   The program compiled on OS X with g77 outputs (radians/sec):
C
C      Time derivative of angular separation:   3.81211936E-09
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E.D. Wright    (JPL)
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 21-MAR-2014 (EDW)
C
C       Reimplemented algorithm using ZZDIV.
C
C-    SPICELIB Version 1.0.1, 15-MAR-2010 (EDW)
C
C       Trivial header format clean-up.
C
C-    SPICELIB Version 1.0.0, 31-MAR-2009 (EDW)
C
C-&

C$ Index_Entries
C
C   time derivative of angular separation
C
C-&

C
C     SPICELIB functions
C

      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      ZZDIV

      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local variables
C

      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      NUMR
      DOUBLE PRECISION      PCROSS (3)
      DOUBLE PRECISION      U1     (6)
      DOUBLE PRECISION      U2     (6)


      IF ( RETURN() ) THEN
         DVSEP = 0.D0
         RETURN
      END IF

      CALL CHKIN( 'DVSEP' )

C
C     Compute the unit vectors and corresponding time derivatives
C     for the input state vectors.
C
      CALL DVHAT( S1, U1 )
      CALL DVHAT( S2, U2 )

C
C     Calculate the cross product vector of U1 and U2. As both vectors
C     have magnitude one, the magnitude of the cross product equals
C     sin(THETA), with THETA the angle between S1 and S2.
C
      CALL VCRSS( U1(1), U2(1), PCROSS )

C
C     Handle the parallel and anti-parallel cases.
C
      IF ( VZERO(PCROSS) ) THEN

         DVSEP = 0.D0
         CALL CHKOUT( 'DVSEP' )
         RETURN

      END IF

C
C     Now calculate the time derivative of the angular separation
C     between S1 and S2.
C

C
C     Separately calculate the numerator and denominator.
C
      NUMR  = VDOT( U1(1), U2(4) ) + VDOT( U1(4), U2(1) )
      DENOM = VNORM(PCROSS)

C
C     ZZDIV checks for over- or underflow. Finite precision
C     arithmetic is a pain.
C
      DVSEP = ZZDIV( -NUMR, DENOM )

C
C     Return, the expectation exists that a FAILED() call
C     follows the DVSEP call.
C
      CALL CHKOUT( 'DVSEP' )

      RETURN
      END

