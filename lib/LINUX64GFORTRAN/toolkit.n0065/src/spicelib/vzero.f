C$Procedure  VZERO    ( Is a vector the zero vector? )
 
      LOGICAL FUNCTION VZERO ( V )
 
C$ Abstract
C
C     Indicate whether a 3-vector is the zero vector.
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
 
      DOUBLE PRECISION      V ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     V          I   Vector to be tested.
C
C     The function returns the value .TRUE. if and only if V is the
C     zero vector.
C
C$ Detailed_Input
C
C     V              is a vector in 3-space.
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
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function has the same truth value as the logical expression
C
C        VNORM ( V )  .EQ.  0.D0
C
C     Replacing the above expression by
C
C        VZERO ( V )
C
C     has several advantages:  the latter expresses the test more
C     clearly, looks better, and doesn't go through the work of scaling,
C     squaring, taking a square root, and re-scaling (all of which
C     VNORM must do) just to find out that a vector is non-zero.
C
C     A related function is VZEROG, which accepts vectors of arbitrary
C     dimension.
C
C$ Examples
C
C     1)  When testing whether a vector is the zero vector, one
C         normally constructs tests like
C
C            IF (  VNORM ( V )  .EQ.  0.D0  ) THEN
C                        .
C                        .
C                        .
C
C
C         These can be replaced with the code
C
C            IF (  VZERO ( V )  ) THEN
C                        .
C                        .
C                        .
C
C
C      2)  Check that a normal vector is non-zero before creating
C         a plane with PNV2PL:
C
C         IF (  VZERO ( NORMAL )  )  THEN
C
C            [ handle error ]
C
C         ELSE
C
C            CALL PNV2PL ( POINT, NORMAL, PLANE )
C                          .
C                          .
C                          .
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
C-    SPICELIB Version 1.0.0, 17-JUL-1990  (NJB) (IMU)
C
C-&
 
C$ Index_Entries
C
C     test whether a 3-dimensional vector is the zero vector
C
C-&
 
 
 
C
C     `Just do it'.
C
C
      VZERO   =        (  V(1) .EQ. 0.D0  )
     .          .AND.  (  V(2) .EQ. 0.D0  )
     .          .AND.  (  V(3) .EQ. 0.D0  )
 
      RETURN
      END
