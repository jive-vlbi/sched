C$Procedure VPERP ( Perpendicular component of a 3-vector )

      SUBROUTINE VPERP ( A, B, P )

C$ Abstract
C
C     Find the component of a vector that is perpendicular to a second
C     vector.  All vectors are 3-dimensional.
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
C     VECTOR
C
C$ Declarations

      DOUBLE PRECISION   A ( 3 )
      DOUBLE PRECISION   B ( 3 )
      DOUBLE PRECISION   P ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A          I    The vector whose orthogonal component is sought.
C     B          I    The vector used as the orthogonal reference.
C     P          O    The component of A orthogonal to B.
C
C$ Detailed_Input
C
C     A     is a double precision, 3-dimensional vector.  It the vector
C           whose component orthogonal to B is sought. (There is a
C           unique decomposition of A into a sum V + P, where V is
C           parallel to B and P is orthogonal to B.  We want the
C           component P.)
C
C     B     is a double precision, 3-dimensional vector.  This
C           vector is the vector used as a reference for the
C           decomposition of A.
C
C
C$ Detailed_Output
C
C     P     is a double precision, 3-dimensional vector containing
C           the component of A that is orthogonal to B.
C           P may overwrite either A or B.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given and non-zero vector B and a vector A, there is a unique
C     decomposition of A as a sum V + P such that P is orthogonal
C     to B and V is parallel to B.  This routine finds the vector P.
C
C     If B is a zero vector, P will be identical to A.
C
C$ Examples
C
C     The following table gives sample inputs and results from calling
C     VPERP.
C
C        A                  B                 P
C        ------------------------------------------
C        (6, 6, 6)      ( 2, 0, 0)        (0, 6, 6)
C        (6, 6, 6)      (-3, 0, 0)        (0, 6, 6)
C        (6, 6, 0)      ( 0, 7, 0)        (6, 0, 0)
C        (6, 0, 0)      ( 0, 0, 9)        (6, 0, 0)
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     Any reasonable calculus text (for example Thomas)
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 11-MAY-2010 (EDW)
C
C       Minor edit to code comments eliminating typo.
C
C        Reordered header sections to proper NAIF convention.
C        Removed Revision section, it listed a duplication of a
C        Version section entry.
C
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&

C$ Index_Entries
C
C     perpendicular component of a 3-vector
C
C-&

C
C     Local variables
C
      DOUBLE PRECISION      BIGA
      DOUBLE PRECISION      BIGB
      DOUBLE PRECISION      R(3)
      DOUBLE PRECISION      T(3)
      DOUBLE PRECISION      V(3)


C
C     Error free routine:  no check-in.
C
      BIGA = MAX ( DABS(A(1)), DABS(A(2)), DABS(A(3)) )
      BIGB = MAX ( DABS(B(1)), DABS(B(2)), DABS(B(3)) )

C
C     If A is the zero vector, just set P to zero and return.
C
      IF ( BIGA .EQ. 0.0D0 ) THEN

         P(1) = 0.0D0
         P(2) = 0.0D0
         P(3) = 0.0D0
         RETURN

      END IF

C
C     If B is the zero vector, then set P equal to A.
C
      IF ( BIGB .EQ. 0.0D0 ) THEN

         P(1) = A(1)
         P(2) = A(2)
         P(3) = A(3)
         RETURN

      END IF

      T(1) = A(1) / BIGA
      T(2) = A(2) / BIGA
      T(3) = A(3) / BIGA

      R(1) = B(1) / BIGB
      R(2) = B(2) / BIGB
      R(3) = B(3) / BIGB

      CALL VPROJ  ( T,    R, V )
      CALL VSUB   ( T,    V, P )
      CALL VSCLIP ( BIGA,    P )

      RETURN
      END
