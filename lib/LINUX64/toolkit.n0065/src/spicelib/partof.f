 
C$Procedure      PARTOF ( Parabolic time of flight )
 
      SUBROUTINE PARTOF ( MA, D )
 
C$ Abstract
C
C     Solve the time of flight equation MA = D + (D**3) / 3
C     for the parabolic eccentric anomaly D, given mean anomaly.
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
      DOUBLE PRECISION      D
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MA         I   Mean anomaly at epoch.
C     D          O   Parabolic eccentric anomaly.
C
C$ Detailed_Input
C
C     MA          is the parabolic mean anomaly of an orbiting body at
C                 some epoch t,
C
C                                         3  1/2
C                       MA = (t-T) (mu/(2q ))
C
C                 where T is the time of periapsis passage, mu is
C                 the gravitational parameter of the primary body,
C                 and q is the perifocal distance.
C
C$ Detailed_Output
C
C     D           is the corresponding parabolic anomaly. This is the
C                 solution to the time of flight equation
C
C                                 3
C                       MA = D + D / 3
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Iterate to solve
C
C                            3
C           f(D,MA,p) = D + D / 3 - MA = 0
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
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 19-APR-1991 (WLT)
C
C        A write statement left over from debugging days was removed.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     parabolic time of flight
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 19-APR-1991 (WLT)
C
C        A write statement left over from debugging days was removed.
C
C-    Beta Version 1.0.1, 27-JAN-1989 (IMU)
C
C        Examples section completed.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      DCBRT
 
C
C     Local parameters
C
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL = 1.D-13 )
 
C
C     Local variables
C
      DOUBLE PRECISION      M
      DOUBLE PRECISION      FN
      DOUBLE PRECISION      DERIV
      DOUBLE PRECISION      DERIV2
      DOUBLE PRECISION      CHANGE
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PARTOF' )
      END IF
 
 
 
C
C     If the mean anomaly is zero, the eccentric anomaly is also zero
C     (by inspection). If the mean anomaly is negative, we can pretend
C     that it's positive (by symmetry).
C
 
      IF ( MA .EQ. 0.D0 ) THEN
         D = 0.D0
         CALL CHKOUT ( 'PARTOF' )
         RETURN
      ELSE
         M = ABS ( MA )
      END IF
 
 
C
C     We need an initial guess for the eccentric anomaly D. The function
C     is well behaved, so just about any guess will do.
C
      D = DCBRT ( 3.D0 * M )
 
 
C
C     Use the Newton second-order method,
C
C                                           2
C          F    = F  - (f/f')*(1 + f*f''/2f' )
C           i+1    i
C
C     where
C
C                     3
C          f   = D + D / 3 - M
C
C                     2
C          f'  = 1 + D
C
C
C          f'' = 2 D
C
 
      CHANGE = 1.D0
 
      DO WHILE ( ABS ( CHANGE ) .GT. TOL )
 
         FN     = D    + D**3/3.D0 - M
         DERIV  = 1.D0 + D**2
         DERIV2 = 2.D0 * D
 
         CHANGE = (FN/DERIV) * ( 1.D0 + (FN*DERIV2) / (2.D0*DERIV**2) )
         D      = D - CHANGE
 
      END DO
 
      IF ( MA .LT. 0.D0 ) THEN
         D = -D
      END IF
 
 
      CALL CHKOUT ( 'PARTOF' )
      RETURN
      END
