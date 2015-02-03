C$Procedure      VPROJ ( Vector projection, 3 dimensions )
 
      SUBROUTINE VPROJ ( A, B, P )
 
C$ Abstract
C
C     VPROJ finds the projection of one vector onto another vector.
C     All vectors are 3-dimensional.
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
C     A          I   The vector to be projected.
C     B          I   The vector onto which A is to be projected.
C     P          O   The projection of A onto B.
C
C$ Detailed_Input
C
C     A     is a double precision, 3-dimensional vector.  This
C           vector is to be projected onto the vector B.
C
C     B     is a double precision, 3-dimensional vector.  This
C           vector is the vector which receives the projection.
C
C$ Detailed_Output
C
C     P     is a double precision, 3-dimensional vector containing the
C           projection of A onto B.  (P is necessarily parallel to B.)
C           If B is the zero vector then P will be returned as the zero
C           vector.
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
C     The given any vectors A and B there is a unique decomposition of
C     A as a sum V + P such that V  the dot product of V and B is zero,
C     and the dot product of P with B is equal the product of the
C     lengths of P and B.  P is called the projection of A onto B.  It
C     can be expressed mathematically as
C
C        DOT(A,B)
C        -------- * B
C        DOT(B,B)
C
C     (This is not necessarily the prescription used to compute the
C     projection. It is intended only for descriptive purposes.)
C
C$ Examples
C
C     The following table gives sample inputs and results from calling
C     VPROJ.
C
C        A                  B           NDIM               P
C        -------------------------------------------------------
C        (6, 6, 6)      ( 2, 0, 0)        3            (6, 0, 0)
C        (6, 6, 6)      (-3, 0, 0)        3            (6, 0, 0)
C        (6, 6, 0)      ( 0, 7, 0)        3            (0, 6, 0)
C        (6, 0, 0)      ( 0, 0, 9)        3            (0, 0, 0)
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
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
C     3-dimensional vector projection
C
C-&
 
      DOUBLE PRECISION VDOT
C
      DOUBLE PRECISION      BIGA
      DOUBLE PRECISION      BIGB
      DOUBLE PRECISION      R(3)
      DOUBLE PRECISION      T(3)
      DOUBLE PRECISION      SCALE
C
      BIGA = MAX ( DABS(A(1)), DABS(A(2)), DABS(A(3)) )
      BIGB = MAX ( DABS(B(1)), DABS(B(2)), DABS(B(3)) )
 
      IF ( BIGA .EQ. 0 ) THEN
         P(1) = 0.0D0
         P(2) = 0.0D0
         P(3) = 0.0D0
         RETURN
      END IF
 
      IF (  BIGB .EQ. 0 ) THEN
         P(1) = 0.0D0
         P(2) = 0.0D0
         P(3) = 0.0D0
         RETURN
      END IF
 
      R(1)  = B(1) / BIGB
      R(2)  = B(2) / BIGB
      R(3)  = B(3) / BIGB
 
      T(1)  = A(1) / BIGA
      T(2)  = A(2) / BIGA
      T(3)  = A(3) / BIGA
 
      SCALE = VDOT (T,R) * BIGA  / VDOT (R,R)
 
      CALL VSCL ( SCALE, R, P )
 
      RETURN
      END
