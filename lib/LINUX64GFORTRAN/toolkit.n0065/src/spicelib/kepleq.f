C$Procedure      KEPLEQ ( Kepler's Equation - Equinoctial Version )
 
      DOUBLE PRECISION FUNCTION KEPLEQ (ML,H,K)
 
 
C$ Abstract
C
C    This function solves the equinoctial version of Kepler's
C    equation.
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
C     SPK
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION  ML
      DOUBLE PRECISION  H
      DOUBLE PRECISION  K
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ML         I   Mean longitude
C     H          I   h component of equinoctial elements
C     K          I   k component of equinoctial elements
C
C$ Detailed_Input
C
C     ML         mean longitude of some body following two body
C                motion.  (Mean longitude = Mean anomaly + argument
C                of periapse + longitude of ascending node.)
C
C     H          The h component of the equinoctial element set
C                ( h = ECC*SIN( arg of periapse + long ascending node) )
C
C     K          The k component of the equinoctial element set
C                ( k = ECC*COS( arg of periapse + long ascending node) )
C
C     Note that ECC = DSQRT ( K*K + H*H )
C
C$ Detailed_Output
C
C     The function returns the value of F such that
C     ML = F + h*COS(F) - k*SIN(F)
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the sum of the squares of F and K is not less than .9
C        the error 'SPICE(ECCOUTOFBOUNDS)' will be signalled.
C
C     2) If the iteration for a solution to the equinoctial Kepler's
C        equation does not converge in 10 or fewer steps, the error
C        'SPICE(NOCONVERGENCE)' is signalled.
C
C$ Particulars
C
C     This routine solves the equinoctial element version of
C     Kepler's equation.
C
C        ML = F + h*COS(F) - k*SIN(F)
C
C     Here F is an offset from the eccentric anomaly E.
C
C        F = E - argument of periapse - longitude of ascending node.
C
C     where E is eccentric anomaly.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     "Optical Navigation Program Mathematical Models" JPL
C     Engineering Memorandum 314-513.  By William M. Owen
C     August 9, 1991.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 11-DEC-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Solve the equinoctial version of Kepler's equation
C
C-&
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      KPSOLV
 
C
C     Local variables
C
      DOUBLE PRECISION      EVEC ( 2 )
      DOUBLE PRECISION      E2
 
C
C     Make sure that H and K are in the expected range.
C
      E2 = H*H + K*K
 
      IF ( E2 .GE. 0.81D0 ) THEN
 
         KEPLEQ = 0.0D0
         CALL CHKIN  ( 'KEPLEQ' )
         CALL SETMSG ( 'The values of H and K supplied to KEPLEQ '
     .   //            'must satisfy the inequality H*H + K*K < '
     .   //            'ECC**2 where ECC is the eccentricity '
     .   //            'threshold of 0.9.  The values of H and K '
     .   //            'are: # and # respectively. H*H + K*K = '
     .   //            '#. ' )
 
         CALL ERRDP  ( '#', H  )
         CALL ERRDP  ( '#', K  )
         CALL ERRDP  ( '#', E2 )
         CALL SIGERR ( 'SPICE(ECCOUTOFBOUNDS)' )
         CALL CHKOUT ( 'KEPLEQ' )
         RETURN
 
      END IF
 
C
C     Instead of solving the equation
C
C            ML  = F + H*DCOS(F) - K*DSIN(F)
C
C     We set X equal to F - ML and solve the equivalent equation
C
C            0   = X + H*DCOS(ML+X) - K*DSIN(ML+X)
C
C                = X + H*{DCOS(ML)*DCOS(X) - DSIN(ML)*DSIN(X)}
C                    - K*{DSIN(ML)*DCOS(X) + DCOS(ML)*DSIN(X)}
C
C                = X + { H*DCOS(ML) - K*DSIN(ML) }*DCOS(X)
C                    - { H*DSIN(ML) + K*DCOS(ML) }*DSIN(X)
C
C
C     We can rearrange this to:
C
C                                 -                    -     -       -
C                                |  DCOS(ML)  -DSIN(ML) |   | DCOS(X) |
C            0 = X + [ H  -K ] * |  DSIN(ML)   DCOS(ML) | * | DSIN(X) |
C                                 -                    -     -       -
C
C     Finally if we let
C
CC                                       -                    -
C                                       |  DCOS(ML)  -DSIN(ML) |
C      EVEC =  [ EX  EY ] = [ -H  K ] * |  DSIN(ML)   DCOS(ML) |
C                                        -                    -
C
C     and
C
C              DCOS(X)
C      U(X) =  DSIN(X)
C
C     Then we can rewrite the equation as:
C
C        0  =  X - < EVEC, U(X) >
C
C     where <,> denotes the dot product operation.  Note that X
C     is necessarily in the range from -ECC to ECC where ECC = | EVEC |
C
C     Once we've computed X, F is just ML + X.
C
C     For those of you who are fans of the classical keplerian
C     elements:
C
C        x = F - ML = E - M
C
C     where E denotes eccentric anomaly and M denotes mean anomaly.
C
C     The routine KPEVEC returns the value of X that solves
C     the equation X - < EVEC, UVEC(X) >
C
 
      EVEC(1) = -H*DCOS(ML) + K*DSIN(ML)
      EVEC(2) =  H*DSIN(ML) + K*DCOS(ML)
      KEPLEQ  =  ML         + KPSOLV( EVEC )
 
      RETURN
      END
