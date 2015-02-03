C$Procedure      SURFNM ( Surface normal vector on an ellipsoid )
 
      SUBROUTINE SURFNM ( A, B, C, POINT, NORMAL )
 
C$ Abstract
C
C     This routine computes the outward-pointing, unit normal vector
C     from a point on the surface of an ellipsoid.
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
C      ELLIPSOID,  GEOMETRY
C
C$ Declarations
 
      DOUBLE PRECISION   A
      DOUBLE PRECISION   B
      DOUBLE PRECISION   C
      DOUBLE PRECISION   POINT  ( 3 )
      DOUBLE PRECISION   NORMAL ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A          I   Length of the ellisoid semi-axis along the x-axis.
C      B          I   Length of the ellisoid semi-axis along the y-axis.
C      C          I   Length of the ellisoid semi-axis along the z-axis.
C      POINT      I   Body-fixed coordinates of a point on the ellipsoid
C      NORMAL     O   Outward pointing unit normal to ellipsoid at POINT
C
C$ Detailed_Input
C
C      A          This is the length of the semi-axis of the ellipsoid
C                 that is parallel to the x-axis of the body-fixed
C                 coordinate system.
C
C      B          This is the length of the semi-axis of the ellipsoid
C                 that is parallel to the y-axis of the body-fixed
C                 coordinate system.
C
C      C          This is the length of the semi-axis of the ellipsoid
C                 that is parallel to the z-axis of the body-fixed
C                 coordinate system.
C
C      POINT      This is a 3-vector giving the bodyfixed coordinates
C                 of a point on the ellipsoid. In bodyfixed coordinates,
C                 the semi-axes of the ellipsoid are aligned with the
C                 x, y, and z-axes of the coordinate system.
C
C$ Detailed_Output
C
C      NORMAL    A unit vector pointing away from the ellipsoid and
C                normal to the ellipsoid at POINT.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If any of the axes are non-positive, the error
C        'SPICE(BADAXISLENGTH)' will be signalled.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      This routine computes the outward pointing unit normal vector to
C      the ellipsoid having semi-axes of length A, B, and C from the
C      point POINT.
C
C$ Examples
C
C      A typical use of SURFNM would be to find the angle of incidence
C      of the light from the sun at a point on the surface of an
C      ellipsoid.
C
C      Let Q be a 3-vector representing the rectangular body-fixed
C      coordinates of a point on the ellipsoid (we are assuming that
C      the axes of the ellipsoid are aligned with the axes of the
C      body fixed frame.)  Let V be the vector from Q to the sun in
C      bodyfixed coordinates.  Then the following code fragment could
C      be used to compute angle of incidence of sunlight at Q.
C
C            CALL SURFNM   ( A, B, C, Q, NRML )
C
C            INCIDN = VSEP ( V,          NRML )
C
C
C$ Restrictions
C
C      It is assumed that the input POINT is indeed on the ellipsoid.
C      No checking for this is done.
C
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.1, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
C
C-    SPICELIB Version 1.3.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHAT call.
C
C-    SPICELIB Version 1.2.0, 07-AUG-1996 (WLT)
C
C        Added a SAVE statement so that the error message will
C        not be lost between separate invocations of the routine.
C         
C-    SPICELIB Version 1.1.0, 21-JUL-1995 (WLT)
C
C        A typo in the Examples section was corrected
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
C     surface normal vector on an ellipsoid
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.3.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHAT call.
C
C-    Beta Version 2.0.0, 9-JAN-1989  (WLT)
C
C     Error handling added.
C
C     The algorithm was modified from the initial obvious routine
C     to one that is immune to numerical catastrophes (multiplication
C     or division overflows).
C
C-&
 
C
C     Spicelib Functions
C
      LOGICAL               RETURN
 
C
C     Local Variables
C
      DOUBLE PRECISION      M
      DOUBLE PRECISION      A1
      DOUBLE PRECISION      B1
      DOUBLE PRECISION      C1
 
      CHARACTER*32          MSSG ( 7 )
      INTEGER               BAD
      SAVE
 
C
C     Initial values
C
      DATA                  MSSG  / 'Axis A was nonpositive.',
     .                              'Axis B was nonpositive.',
     .                              'Axes A and B were nonpositive.',
     .                              'Axis C was nonpositive.',
     .                              'Axes A and C were nonpositive.',
     .                              'Axes B and C were nonpositive.',
     .                              'All three axes were nonpositive.' /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SURFNM' )
      END IF
 
 
C
C     Check the axes to make sure that none of them is less than or
C     equal to zero. If one is, signal an error and return.
C
      BAD = 0
 
      IF ( A .LE. 0 )
     .   BAD = BAD + 1
 
      IF ( B .LE. 0 )
     .   BAD = BAD + 2
 
      IF ( C .LE. 0 )
     .   BAD = BAD + 4
 
 
      IF ( BAD .GT. 0 ) THEN
         CALL SETMSG ( MSSG (BAD)                                     //
     .                 ' ? '                                           )
         CALL ERRCH  ( ' ? ', 'The A,B, and C axes were '             //
     .                        '#, #, and # respectively.'              )
 
         CALL ERRDP  ( '#', A )
         CALL ERRDP  ( '#', B )
         CALL ERRDP  ( '#', C )
 
         CALL SIGERR ( 'SPICE(BADAXISLENGTH)' )
         CALL CHKOUT ( 'SURFNM' )
         RETURN
      END IF
 
C
C     Mathematically we want to compute (Px/a**2, Py/b**2, Pz/c**2)
C     and then convert this to a unit vector. However, computationally
C     this can blow up in our faces.  But note that only the ratios
C     a/b, b/c and a/c are important in computing the unit normal.
C     We can use the trick below to avoid the unpleasantness of
C     multiplication and division overflows.
C
      M         = MIN ( A, B, C )
 
C
C     M can be divided by A,B or C without fear of an overflow
C     occuring.
C
      A1        = M/A
      B1        = M/B
      C1        = M/C
 
C
C     All of the terms A1,B1,C1 are less than 1. Thus no overflows
C     can occur.
C
      NORMAL(1) = POINT(1) * (A1*A1)
      NORMAL(2) = POINT(2) * (B1*B1)
      NORMAL(3) = POINT(3) * (C1*C1)
 
      CALL VHATIP ( NORMAL )
 
      CALL CHKOUT ( 'SURFNM' )
      RETURN
      END
