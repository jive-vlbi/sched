C$Procedure  VZEROG ( Is a vector the zero vector?---general dim. )
 
      LOGICAL FUNCTION VZEROG ( V, NDIM )
 
C$ Abstract
C
C     Indicate whether a general-dimensional vector is the zero vector.
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
C     MATH
C     VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION      V    ( * )
      INTEGER               NDIM
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     V          I   Vector to be tested.
C     NDIM       I   Dimension of V.
C
C     The function returns the value .TRUE. if and only if V is the
C     zero vector.
C
C$ Detailed_Input
C
C     V,
C     NDIM           are, respectively, a vector and its dimension.
C
C$ Detailed_Output
C
C     The function returns the value .TRUE. if and only if V is the
C     zero vector.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)   When NDIM is non-positive, this function returns the value
C          .FALSE.  (A vector of non-positive dimension cannot be the
C          zero vector.)
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function has the same truth value as the logical expression
C
C        VNORMG ( V, NDIM )  .EQ.  0.D0
C
C     Replacing the above expression by
C
C        VZEROG ( V, NDIM )
C
C     has several advantages:  the latter expresses the test more
C     clearly, looks better, and doesn't go through the work of scaling,
C     squaring, taking a square root, and re-scaling (all of which
C     VNORMG must do) just to find out that a vector is non-zero.
C
C     A related function is VZERO, which accepts three-dimensional
C     vectors.
C
C$ Examples
C
C     1)  When testing whether a vector is the zero vector, one
C         normally constructs tests like
C
C            IF (  VNORMG ( V, NDIM )  .EQ.  0.D0  ) THEN
C                        .
C                        .
C                        .
C
C         These can be replaced with the code
C
C            IF (  VZEROG ( V, NDIM )  ) THEN
C                        .
C                        .
C                        .
C
C     2)  Make sure that a `unit' quaternion is non-zero before
C         converting it to a rotation matrix.
C
C            IF (  VZEROG ( Q, 4 )  ) THEN
C
C               [ handle error ]
C
C            ELSE
C
C               CALL VHATG ( Q, 4, Q )
C               CALL Q2M   ( Q, M )
C                        .
C                        .
C                        .
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
C     N.J. Bachman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 18-JUL-1990 (NJB) (IMU)
C
C-&
 
C$ Index_Entries
C
C     test whether an n-dimensional vector is the zero vector
C
C-&
 
 
 
C
C     Local variables
C
      INTEGER               I
 
C
C     Leave as soon as we find a non-zero component.  If we get through
C     the loop, we have a zero vector, as long as the vector's dimension
C     is valid.
C
      DO I = 1, NDIM
 
         IF ( V(I) .NE. 0.D0 ) THEN
            VZEROG = .FALSE.
            RETURN
         END IF
 
      END DO
 
C
C     We have a zero vector if and only if the vector's dimension is at
C     least 1.
C
      VZEROG  =  ( NDIM .GE. 1 )
 
      END
