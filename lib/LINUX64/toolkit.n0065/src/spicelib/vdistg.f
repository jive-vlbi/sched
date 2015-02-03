C$Procedure      VDISTG ( Vector distance, general dimension )
 
      DOUBLE PRECISION FUNCTION VDISTG ( V1, V2, NDIM )
 
C$ Abstract
C
C     Return the distance between two vectors of arbitrary dimension.
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
 
      DOUBLE PRECISION      V1   ( * )
      DOUBLE PRECISION      V2   ( * )
      INTEGER               NDIM
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     V1,
C     V2         I   Two vectors of arbitrary dimension.
C     NDIM       I   The common dimension of V1 and V2
C
C     The function returns the distance between V1 and V2.
C
C$ Detailed_Input
C
C     V1,
C     V2         are two vectors of arbitrary dimension, the
C                distance between which is desired.
C
C     NDIM       is the common dimension of V1 and V2.  NDIM must be
C                non-negative and must not exceed the minimum of the
C                declared sizes of the actual arguments corresponding
C                to V1 and V2.
C
C$ Detailed_Output
C
C     The function returns the distance between V1 and V2.  This is
C     defined as
C
C              ||  V1 - V2  ||,
C
C     where || x || indicates the Euclidean norm of the vector x.
C
C     If NDIM is less than 1, the function value is set to 0.D0.
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
C     The Euclidean norm of an n-dimensional vector
C
C        (x ,  x , ... , x )
C          1    2         n
C
C     is defined as
C
C                                                1/2
C              2        2                  2
C        (   x    +   x    +  . . .  +   x     ).
C             1        2                  n
C
C     This number is the distance of the point (x, y, z) from the
C     origin.  If n = 3, and A and B are two vectors whose components
C     are
C
C        ( A(1), A(2), A(3) )    and    ( B(1), B(2), B(3) ),
C
C     then the distance between A and B is the norm of the difference
C     A - B, which has components
C
C        (  A(1) - B(1),  A(2) - B(2),  A(3) - B(3)  ).
C
C     A related routine is VDIST, which computes the distance between
C     two 3-vectors.
C
C$ Examples
C
C     1)  If V1 is
C
C            ( 2.0D0,  3.0D0 )
C
C         and V2 is
C
C            ( 5.0D0,  7.0D0 ),
C
C         and NDIM is 2, then
C
C            VDISTG (V1, V2, NDIM )
C
C         will be 5.D0.
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
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 17-JUL-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     distance between n-dimensional vectors
C
C-&
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION      SCALE
      INTEGER               I
 
C
C     We find the norm of a scaled version of the difference vector,
C     and then rescale this norm.  This method helps prevent overflow
C     due to squaring the components of the difference vector.
C
C     The code here is almost identical to that of VNORMG.  We'd love
C     to just call VNORMG, but that would require storage for the
C     difference vector.  So we do the job ourselves.
C
 
C
C     Find the scale factor.
C
      SCALE = 0.D0
 
      DO I = 1, NDIM
         SCALE = MAX (  SCALE,  DABS ( V1(I) - V2(I) )  )
      END DO
 
 
      IF ( SCALE .EQ. 0.D0 ) THEN
 
         VDISTG = 0.D0
         RETURN
 
      ELSE
 
         VDISTG = 0.D0
 
         DO I = 1, NDIM
            VDISTG = VDISTG  +  (  ( V1(I) - V2(I) ) / SCALE  ) ** 2
         END DO
 
         VDISTG = SCALE  *  SQRT ( VDISTG )
 
      END IF
 
      RETURN
      END
