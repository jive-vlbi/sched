C$Procedure      UCRSS ( Unitized cross product, 3x3 )
 
      SUBROUTINE UCRSS ( V1, V2, VOUT )
 
C$ Abstract
C
C      Compute the normalized cross product of two 3-vectors.
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
C      VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION   V1   ( 3 )
      DOUBLE PRECISION   V2   ( 3 )
      DOUBLE PRECISION   VOUT ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       V1        I     Left vector for cross product.
C       V2        I     Right vector for cross product.
C       VOUT      O     Normalized cross product (V1xV2) / |V1xV2|.
C
C$ Detailed_Input
C
C      V1   A 3-vector.
C
C      V2   A 3-vector.
C
C$ Detailed_Output
C
C      VOUT is the result of the computation (V1xV2)/|V1xV2|
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C      To get a unit normal to the plane spanned by two vectors
C      V1 and V2. Simply call
C
C         CALL UCRSS ( V1, V2, NORMAL )
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the cross product of V1 and V2 yields the zero-vector, then
C        the zero-vector is returned instead of a vector of unit length.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.M. Owen       (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None
C
C$ Version
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
C     unitized cross product
C
C-&
 
C$ Revisions
C
C-    Beta Version 1.1.0, 10-JAN-1989 (WLT)
C
C     Error free specification added. In addition the algorithm was made
C     more robust in the sense that floating point overflows cannot
C     occur.
C
C-&
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VCROSS(3)
      DOUBLE PRECISION      VMAG
 
      DOUBLE PRECISION      MAXV1
      DOUBLE PRECISION      MAXV2
 
      DOUBLE PRECISION      TV1 ( 3 )
      DOUBLE PRECISION      TV2 ( 3 )
 
C
C     Get the biggest component of each of the two vectors.
C
      MAXV1 = MAX ( DABS(V1(1)), DABS(V1(2)), DABS(V1(3)) )
      MAXV2 = MAX ( DABS(V2(1)), DABS(V2(2)), DABS(V2(3)) )
 
C
C     Scale V1 and V2 by 1/MAXV1 and 1/MAXV2 respectively
C
      IF ( MAXV1 .NE. 0 ) THEN
         TV1(1) = V1(1)/MAXV1
         TV1(2) = V1(2)/MAXV1
         TV1(3) = V1(3)/MAXV1
      ELSE
         TV1(1) = 0.0D0
         TV1(2) = 0.0D0
         TV1(3) = 0.0D0
      END IF
 
      IF ( MAXV2 .NE. 0 ) THEN
         TV2(1) = V2(1)/MAXV2
         TV2(2) = V2(2)/MAXV2
         TV2(3) = V2(3)/MAXV2
      ELSE
         TV2(1) = 0.0D0
         TV2(2) = 0.0D0
         TV2(3) = 0.0D0
      END IF
 
C
C  Calculate the cross product of V1 and V2
C
      VCROSS(1) = TV1(2)*TV2(3) - TV1(3)*TV2(2)
      VCROSS(2) = TV1(3)*TV2(1) - TV1(1)*TV2(3)
      VCROSS(3) = TV1(1)*TV2(2) - TV1(2)*TV2(1)
C
C  Get the magnitude of VCROSS and normalize it
C
      VMAG = VNORM(VCROSS)
 
      IF (VMAG.GT.0.D0) THEN
         VOUT(1) = VCROSS(1) / VMAG
         VOUT(2) = VCROSS(2) / VMAG
         VOUT(3) = VCROSS(3) / VMAG
      ELSE
         VOUT(1) = 0.D0
         VOUT(2) = 0.D0
         VOUT(3) = 0.D0
      END IF
 
      RETURN
      END
