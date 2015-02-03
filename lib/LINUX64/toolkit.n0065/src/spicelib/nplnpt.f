C$Procedure      NPLNPT ( Nearest point on line to point )
 
      SUBROUTINE NPLNPT ( LINPT, LINDIR, POINT, PNEAR, DIST )
 
C$ Abstract
C
C     Find the nearest point on a line to a specified point, and find
C     the distance between the two points.
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
C     GEOMETRY
C     MATH
C     VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION      LINPT  ( 3 )
      DOUBLE PRECISION      LINDIR ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
 
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      DIST
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LINPT,
C     LINDIR     I   Point on a line and the line's direction vector.
C     POINT      I   A second point.
C     PNEAR      O   Nearest point on the line to POINT.
C     DIST       O   Distance between POINT and PNEAR.
C
C$ Detailed_Input
C
C     LINPT
C     LINDIR         are, respectively, a point and a direction vector
C                    that define a line in 3-dimensional space.  The
C                    line is the set of points
C
C                       LINPT   +   t * LINDIR
C
C                    where t is any real number.
C
C     POINT          is a point in 3-dimensional space.
C
C$ Detailed_Output
C
C     PNEAR          is the nearest point on the input line to the input
C                    point.
C
C     DIST           is the distance between the input line and input
C                    point.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the line direction vector LINDIR is the zero vector, the
C         error SPICE(ZEROVECTOR) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     For every line L and point P, there is a unique closest point
C     on L to P.  Call this closest point C.  It is always true that
C     P - C  is perpendicular to L, and the length of P - C is called
C     the `distance' between P and L.
C
C$ Examples
C
C     1)  Suppose a line passes through the point ( 1, 2, 3 ) and
C         has direction vector ( 0, 1, 1 ).  We wish to find the
C         closest point on the line to the point ( -6, 9, 10 ).  We
C         can use the code fragment
C
C            LINPT(1)   =  1.D0
C            LINPT(2)   =  2.D0
C            LINPT(3)   =  3.D0
C
C            LINDIR(1)  =  0.D0
C            LINDIR(2)  =  1.D0
C            LINDIR(3)  =  1.D0
C
C            POINT(1)   = -6.D0
C            POINT(2)   =  9.D0
C            POINT(3)   = 10.D0
C
C            CALL NPLNPT ( LINPT, LINDIR, POINT, PNEAR, DIST )
C
C         After the call, PNEAR will take the value
C
C            ( 1.D0, 9.D0, 10.D0 );
C
C         DIST will be 7.0.
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
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VADD call.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     distance between point and line
C     nearest point on line to point
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VADD call.
C
C-& 


 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDIST
 
      LOGICAL               RETURN
      LOGICAL               VZERO
 
C
C     Local variables
C
      DOUBLE PRECISION      PROJ  ( 3 )
      DOUBLE PRECISION      TRANS ( 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NPLNPT' )
      END IF
 
C
C     We need a real direction vector to work with.
C
      IF (  VZERO (LINDIR)  )  THEN
 
         CALL SETMSG ( 'Direction vector must be non-zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                  )
         CALL CHKOUT ( 'NPLNPT'                             )
         RETURN
 
      END IF
 
C
C     We translate line and input point so as to put the line through
C     the origin.  Then the nearest point on the translated line to the
C     translated point TRANS is the projection of TRANS onto the line.
C
      CALL VSUB  ( POINT,  LINPT,  TRANS )
      CALL VPROJ ( TRANS,  LINDIR, PROJ  )
      CALL VADD  ( PROJ,   LINPT,  PNEAR )
 
      DIST = VDIST ( PNEAR,  POINT )
 
      CALL CHKOUT ( 'NPLNPT' )
      RETURN
      END
