C$Procedure      VDIST ( Vector distance )
 
      DOUBLE PRECISION FUNCTION  VDIST ( V1, V2 )
 
C$ Abstract
C
C     Return the distance between two three-dimensional vectors.
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
 
      DOUBLE PRECISION      V1 ( 3 )
      DOUBLE PRECISION      V2 ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C     V1,
C     V2         I   Two 3-vectors.
C
C     The function returns the distance between V1 and V2.
C
C$ Detailed_Input
C
C     V1,
C     V2         are two vectors in three-dimensional space, the
C                distance between which is desired.
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
C     This function is simply shorthand for the code
C
C        CALL VSUB ( V1, V2, DIFF )
C
C        DIST = VNORM ( DIFF )
C
C     Using this function saves you the annoyance of declaring local
C     storage for the difference vector DIFF.
C
C
C     The Euclidean norm of a three-dimensional vector (x, y, z) is
C     defined as
C
C                                     1/2
C             2        2        2
C        (   x    +   y    +   z    ).
C
C
C     This number is the distance of the point (x, y, z) from the
C     origin.  If A and B are two vectors whose components are
C
C        ( A(1), A(2), A(3) )    and    ( B(1), B(2), B(3) ),
C
C     then the distance between A and B is the norm of the difference
C     A - B, which has components
C
C
C        (  A(1) - B(1),  A(2) - B(2),  A(3) - B(3)  ).
C
C
C     A related routine is VDISTG, which computes the distance between
C     two vectors of general dimension.
C
C$ Examples
C
C     1)  If V1 is
C
C            ( 2.0D0,  3.0D0,  0.D0 )
C
C         and V2 is
C
C            ( 5.0D0,  7.0D0,  12.D0 ),
C
C         VDIST (V1, V2) will be 13.D0.
C
C
C     2)  If VGR2 and NEP are states of the Voyager 2 spacecraft and
C         Neptune with respect to some common center at a given time
C         ET, then
C
C            VDIST ( VGR2, NEP )
C
C         yields the distance between the spacecraft and Neptune at time
C         ET.
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
C-    SPICELIB Version 1.0.0, 08-JUL-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     distance between 3-dimensional vectors
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM
 
C
C     Local variables
C
      DOUBLE PRECISION      DIFF ( 3 )
 
 
C
C     No surprises.
C
      CALL VSUB ( V1, V2, DIFF )
 
      VDIST = VNORM (DIFF)
 
      RETURN
      END
