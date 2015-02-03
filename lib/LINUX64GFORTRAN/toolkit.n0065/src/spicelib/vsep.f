C$Procedure      VSEP  ( Angular separation of vectors, 3 dimensions )
 
      DOUBLE PRECISION FUNCTION VSEP ( V1, V2 )
 
C$ Abstract
C
C      Find the separation angle in radians between two double
C      precision, 3-dimensional vectors.  This angle is defined as zero
C      if either vector is zero.
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
C      ANGLE,  VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION  V1 ( 3 )
      DOUBLE PRECISION  V2 ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       V1        I     First vector.
C       V2        I     Second vector.
C
C
C$ Detailed_Input
C
C      V1      is an arbitrary double precision, 3-dimensional vector.
C      V2      is also an arbitrary double precision, 3-dimensional
C              vector.  V1 or V2 or both may be the zero vector.
C
C$ Detailed_Output
C
C      VSEP    is the angle between V1 and V2 expressed in radians.
C              VSEP is strictly non-negative.  If either V1 or V2 is
C              the zero vector, then VSEP is defined to be 0 radians.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      In the plane, it is a simple matter to calculate the angle
C      between two vectors once the two vectors have been made to be
C      unit length.  Then, since the two vectors form the two equal
C      sides of an isosceles triangle, the length of the third side
C      is given by the expression
C
C            LENGTH = 2.0 * SINE ( VSEP/2.0 )
C
C      The length is given by the magnitude of the difference of the
C      two unit vectors
C
C            LENGTH = NORM ( U1 - U2 )
C
C      Once the length is found, the value of VSEP may be calculated
C      by inverting the first expression given above as
C
C            VSEP = 2.0 * ARCSINE ( LENGTH/2.0 )
C
C      This expression becomes increasingly unstable when VSEP gets
C      larger than PI/2 or 90 degrees.  In this situation (which is
C      easily detected by determining the sign of the dot product of
C      V1 and V2) the supplementary angle is calculated first and
C      then VSEP is given by
C
C            VSEP = PI - SUPPLEMENTARY_ANGLE
C
C$ Examples
C
C      The following table gives sample values for V1, V2 and VSEP
C      implied by the inputs.
C
C      V1                        V2                     VSEP
C      ----------------------------------------------------------------
C      (1, 0, 0)                  (1, 0, 0)             0.0D0
C      (1, 0, 0)                  (0, 1, 0)             PI/2 (=1.571...)
C
C
C$ Restrictions
C
C      The user is required to insure that the input vectors will not
C      cause floating point overflow upon calculation of the vector
C      dot product since no error detection or correction code is
C      implemented.  In practice, this is not a significant
C      restriction.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None
C
C$ Author_and_Institution
C
C      K.R. Gehringer  (JPL)
C      W.M. Owen       (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 17-APR-2006 (EDW)
C
C       Typo correction to the value of PI/2 in the Examples
C       section, 1.571 instead of 1.71.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     angular separation of 3-dimensional vectors
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-    Beta Version 1.0.1, 10-JAN-1989 (WLT)
C
C     Error free specification added.
C
C-&
 
C
C     SPICELIB functions
C
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
C
C     Local Variables
C
C     The following declarations represent, respectively:
C        Magnitudes of V1, V2
C        Either of the difference vectors: V1-V2 or V1-(-V2)
C        Unit vectors parallel to V1 and V2
C
      DOUBLE PRECISION      DMAG1
      DOUBLE PRECISION      DMAG2
      DOUBLE PRECISION      VTEMP(3)
      DOUBLE PRECISION      U1(3)
      DOUBLE PRECISION      U2(3)
C
C  Calculate the magnitudes of V1 and V2; if either is 0, VSEP = 0
C
      CALL UNORM ( V1, U1, DMAG1 )
      IF ( DMAG1 .EQ. 0.D0 ) THEN
         VSEP = 0.D0
         RETURN
      END IF
 
      CALL UNORM ( V2, U2, DMAG2 )
      IF ( DMAG2 .EQ. 0.D0 ) THEN
         VSEP = 0.D0
         RETURN
      END IF
 
      IF      ( VDOT(U1,U2) .GT. 0 ) THEN
         VTEMP(1) = U1(1) - U2(1)
         VTEMP(2) = U1(2) - U2(2)
         VTEMP(3) = U1(3) - U2(3)
 
         VSEP = 2.0D0 * ASIN (0.5D0 * VNORM(VTEMP))
 
      ELSE IF ( VDOT(U1,U2) .LT. 0 ) THEN
         VTEMP(1) = U1(1) + U2(1)
         VTEMP(2) = U1(2) + U2(2)
         VTEMP(3) = U1(3) + U2(3)
 
         VSEP = PI() - 2.0D0 * ASIN (0.5D0 * VNORM(VTEMP))
 
      ELSE
         VSEP = PI() / 2.0D0
      END IF
 
      RETURN
      END
