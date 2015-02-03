C$Procedure      EUL2M ( Euler angles to matrix )
 
      SUBROUTINE EUL2M (  ANGLE3,   ANGLE2,   ANGLE1,
     .                    AXIS3,    AXIS2,    AXIS1,   R  )
 
C$ Abstract
C
C     Construct a rotation matrix from a set of Euler angles.
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
C     ROTATION
C
C$ Keywords
C
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations
 
      DOUBLE PRECISION      ANGLE3
      DOUBLE PRECISION      ANGLE2
      DOUBLE PRECISION      ANGLE1
 
      INTEGER               AXIS3
      INTEGER               AXIS2
      INTEGER               AXIS1
 
      DOUBLE PRECISION      R ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ANGLE3,
C     ANGLE2,
C     ANGLE1     I   Rotation angles about third, second, and first
C                    rotation axes (radians).
C     AXIS3,
C     AXIS2,
C     AXIS1      I   Axis numbers of third, second, and first rotation
C                    axes.
C
C     R          O   Product of the 3 rotations.
C
C$ Detailed_Input
C
C     ANGLE3,
C     ANGLE2,
C     ANGLE1,
C
C     AXIS3,
C     AXIS2,
C     AXIS1          are, respectively, a set of three angles and three
C                    coordinate axis numbers; each pair ANGLEx and
C                    AXISx specifies a coordinate transformation
C                    consisting of a rotation by ANGLEx radians about
C                    the coordinate axis indexed by AXISx.  These
C                    coordinate transformations are typically
C                    symbolized by
C
C                       [ ANGLEx ]     .
C                                 AXISx
C
C                    See the $ Particulars section below for details
C                    concerning this notation.
C
C                    Note that these coordinate transformations rotate
C                    vectors by
C
C                       -ANGLEx
C
C                    radians about the axis indexed by AXISx.
C
C                    The values of AXISx may be 1, 2, or 3, indicating
C                    the x, y, and z axes respectively.
C
C$ Detailed_Output
C
C     R              is a rotation matrix representing the composition
C                    of the rotations defined by the input angle-axis
C                    pairs.  Together, the three pairs specify a
C                    composite transformation that is the result of
C                    performing the rotations about the axes indexed
C                    by AXIS1, AXIS2, and AXIS3, in that order.  So,
C
C                       R = [ ANGLE3 ]    [ ANGLE2 ]      [ ANGLE1 ]
C                                    AXIS3          AXIS2          AXIS1
C
C                    See the $ Particulars section below for details
C                    concerning this notation.
C
C                    The resulting matrix R may be thought of as a
C                    coordinate transformation; applying it to a vector
C                    yields the vector's coordinates in the rotated
C                    system.
C
C                    Viewing R as a coordinate transformation matrix,
C                    the basis that R transforms vectors to is created
C                    by rotating the original coordinate axes first by
C                    ANGLE1 radians about the coordinate axis indexed
C                    by AXIS1, next by ANGLE2 radians about the
C                    coordinate axis indexed by AXIS2, and finally by
C                    ANGLE3 radians about coordinate axis indexed by
C                    AXIS3.  At the second and third steps of this
C                    process, the coordinate axes about which rotations
C                    are performed belong to the bases resulting from
C                    the previous rotations.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If any of AXIS3, AXIS2, or AXIS1 do not have values in
C
C             { 1, 2, 3 },
C
C          the error SPICE(BADAXISNUMBERS) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A word about notation:  the symbol
C
C        [ x ]
C             i
C
C     indicates a rotation of x radians about the ith coordinate axis.
C     To be specific, the symbol
C
C        [ x ]
C             1
C
C     indicates a coordinate system rotation of x radians about the
C     first, or x-, axis; the corresponding matrix is
C
C        +-                    -+
C        |  1      0       0    |
C        |                      |
C        |  0    cos(x)  sin(x) |.
C        |                      |
C        |  0   -sin(x)  cos(x) |
C        +-                    -+
C
C     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
C     matrix, when applied to a vector, rotates the vector by -x
C     radians, not x radians.  Applying the matrix to a vector yields
C     the vector's representation relative to the rotated coordinate
C     system.
C
C     The analogous rotation about the second, or y-, axis is
C     represented by
C
C        [ x ]
C             2
C
C     which symbolizes the matrix
C
C        +-                    -+
C        | cos(x)   0   -sin(x) |
C        |                      |
C        |  0       1      0    |,
C        |                      |
C        | sin(x)   0    cos(x) |
C        +-                    -+
C
C     and the analogous rotation about the third, or z-, axis is
C     represented by
C
C        [ x ]
C             3
C
C     which symbolizes the matrix
C
C        +-                    -+
C        |  cos(x)  sin(x)   0  |
C        |                      |
C        | -sin(x)  cos(x)   0  |.
C        |                      |
C        |  0        0       1  |
C        +-                    -+
C
C     From time to time, (depending on one's line of work, perhaps) one
C     may happen upon a pair of coordinate systems related by a
C     sequence of rotations.  For example, the coordinate system
C     defined by an instrument such as a camera is sometime specified
C     by RA, DEC, and twist; if alpha, delta, and phi are the rotation
C     angles, then the series of rotations
C
C        [ phi ]     [ pi/2  -  delta ]     [ alpha ]
C               3                      2             3
C
C     produces a transformation from inertial to camera coordinates.
C
C     This routine is related to the SPICELIB routine M2EUL, which
C     produces a sequence of Euler angles, given a rotation matrix.
C     This routine is a `left inverse' of M2EUL:  the sequence of
C     calls
C
C        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
C       .                 ANGLE3,  ANGLE2,  ANGLE1     )
C
C        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
C       .                 AXIS3,   AXIS2,   AXIS1,   R )
C
C     preserves R, except for round-off error.
C
C
C     On the other hand, the sequence of calls
C
C        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
C       .                 AXIS3,   AXIS2,   AXIS1,   R )
C
C        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
C       .                 ANGLE3,  ANGLE2,  ANGLE1     )
C
C     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start
C     out in the ranges that M2EUL's outputs are restricted to.
C
C$ Examples
C
C     1)  Create a coordinate transformation matrix by rotating
C         the original coordinate axes first by 30 degrees about
C         the z axis, next by 60 degrees about the y axis resulting
C         from the first rotation, and finally by -50 degrees about
C         the z axis resulting from the first two rotations.
C
C
C            C
C            C     Create the coordinate transformation matrix
C            C
C            C                   o          o          o
C            C        R  =  [ -50  ]   [  60  ]   [  30  ]
C            C                      3          2          3
C            C
C            C     All angles in radians, please.   The SPICELIB
C            C     function RPD (radians per degree) gives the
C            C     conversion factor.
C            C
C            C     The z axis is `axis 3'; the y axis is `axis 2'.
C            C
C                  ANGLE1 = RPD() *  30.D0
C                  ANGLE2 = RPD() *  60.D0
C                  ANGLE3 = RPD() * -50.D0
C
C                  AXIS1  = 3
C                  AXIS2  = 2
C                  AXIS3  = 3
C
C                  CALL EUL2M (  ANGLE3, ANGLE2, ANGLE1,
C                 .              AXIS3,  AXIS2,  AXIS1,   R  )
C
C
C     2)  A trivial example using actual numbers.
C
C         The code fragment
C
C            CALL EUL2M (  0.D0,     0.D0,     HALFPI(),
C           .                 1,        1,            3,      R  )
C
C         sets R equal to the matrix
C
C            +-                  -+
C            |  0      1       0  |
C            |                    |
C            | -1      0       0  |.
C            |                    |
C            |  0      0       1  |
C            +-                  -+
C
C
C     3)  Finding the rotation matrix specified by a set of `clock,
C         cone, and twist' angles, as defined on the Voyager 2 project:
C
C            Voyager 2 narrow angle camera pointing, relative to the
C            Sun-Canopus coordinate system, was frequently specified
C            by a set of Euler angles called `clock, cone, and twist'.
C            These defined a 3-2-3 coordinate transformation matrix
C            TSCTV as the product
C
C               [ twist ]  [ cone ]   [ clock ] .
C                        3         2           3
C
C            Given the angles CLOCK, CONE, and TWIST (in units of
C            radians), we can compute TSCTV with the code fragment
C
C               CALL EUL2M (  TWIST,  CONE,  CLOCK,
C              .              3,      2,     3,      TSCTV  )
C
C
C     4)  Finding the rotation matrix specified by a set of `right
C         ascension, declination, and twist' angles, as defined on the
C         Galileo project:
C
C            Galileo scan platform pointing, relative to an inertial
C            reference frame, (EME50 variety) is frequently specified
C            by a set of Euler angles called `right ascension (RA),
C            declination (Dec), and twist'. These define a 3-2-3
C            coordinate transformation matrix TISP as the product
C
C               [ Twist ]  [ pi/2 - Dec ]   [ RA ] .
C                        3               2        3
C
C            Given the angles RA, DEC, and TWIST (in units of radians),
C            we can compute TISP with the code fragment
C
C               CALL EUL2M (  TWIST,   HALFPI()-DEC,   RA,
C              .              3,       2,              3,   TISP  )
C
C
C$ Restrictions
C
C     Beware:  more than one definition of "RA, DEC and twist" exists.
C
C$ Literature_References
C
C     [1]  `Galileo Attitude and Camera Models', JPL IOM 314-323,
C           W. M. Owen, Jr.,  Nov. 11, 1983.  NAIF document number
C           204.0.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 26-DEC-2006 (NJB)
C
C        Corrected header typo.
C
C-    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in ROTMAT calls.
C
C-    SPICELIB Version 1.1.2, 14-OCT-2004 (LSE)
C
C        Corrected a typo in the header.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
C
C        Names of input arguments changed to reflect the order in
C        which the rotations are applied when their product is
C        computed.  The header was upgraded to describe notation in
C        more detail.  Examples were added.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     euler angles to matrix
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in ROTMAT calls.
C
C-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
C
C        Argument names were changed to describe the use of the
C        arguments more accurately.  The axis and angle numbers
C        now decrease, rather than increase, from left to right.
C        The current names reflect the order of operator application
C        when the Euler angle rotations are applied to a vector:  the
C        rightmost matrix
C
C           [ ANGLE1 ]
C                     AXIS1
C
C        is applied to the vector first, followed by
C
C           [ ANGLE2 ]
C                     AXIS2
C
C        and then
C
C           [ ANGLE3 ]
C                     AXIS3
C
C        Previously, the names reflected the order in which the Euler
C        angle matrices appear on the page, from left to right.  This
C        naming convention was found to be a bit obtuse by a various
C        users.
C
C        No change in functionality was made; the operation of the
C        routine is identical to that of the previous version.
C
C        Two new examples were added to assist users in verifying
C        their understanding of the routine.
C
C        Also, the header was upgraded to describe the notation in more
C        detail.  The symbol
C
C           [ x ]
C                i
C
C        is explained at mind-numbing length.  An example was added
C        that shows a specific set of inputs and the resulting output
C        matrix.
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      R1    ( 3, 3 )
      LOGICAL               BADAX
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EUL2M' )
      END IF
 
C
C     Make sure the axis numbers are all right:  They must belong to
C     the set {1, 2, 3}.
C
      BADAX =       (  ( AXIS3 .LT. 1 ) .OR. ( AXIS3 .GT. 3 )  )
     .         .OR. (  ( AXIS2 .LT. 1 ) .OR. ( AXIS2 .GT. 3 )  )
     .         .OR. (  ( AXIS1 .LT. 1 ) .OR. ( AXIS1 .GT. 3 )  )
 
 
      IF ( BADAX ) THEN
 
         CALL SETMSG ( 'Axis numbers are #,  #,  #. ' )
         CALL ERRINT ( '#',  AXIS3                    )
         CALL ERRINT ( '#',  AXIS2                    )
         CALL ERRINT ( '#',  AXIS1                    )
         CALL SIGERR ( 'SPICE(BADAXISNUMBERS)'        )
         CALL CHKOUT ( 'EUL2M'                        )
         RETURN
 
      END IF
 
C
C     Just do it.
C
      CALL ROTATE (      ANGLE1,  AXIS1,  R  )
      CALL ROTMAT ( R,   ANGLE2,  AXIS2,  R1 )
      CALL ROTMAT ( R1,  ANGLE3,  AXIS3,  R  )
 
      CALL CHKOUT ( 'EUL2M' )
      RETURN
      END
