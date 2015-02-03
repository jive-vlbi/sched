C$Procedure      AXISAR ( Axis and angle to rotation )
 
      SUBROUTINE AXISAR ( AXIS, ANGLE, R )
 
C$ Abstract
C
C     Construct a rotation matrix that rotates vectors by a specified
C     angle about a specified axis.
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
C
C$ Declarations
 
      DOUBLE PRECISION      AXIS  ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      R     ( 3, 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     AXIS       I   Rotation axis.
C     ANGLE      I   Rotation angle, in radians.
C     R          O   Rotation matrix corresponding to AXIS and ANGLE.
C
C$ Detailed_Input
C
C     AXIS,
C     ANGLE          are, respectively, a rotation axis and a rotation
C                    angle.  AXIS and ANGLE determine a coordinate
C                    transformation whose effect on any vector V is to
C                    rotate V by ANGLE radians about the vector AXIS.
C
C$ Detailed_Output
C
C     R              is a rotation matrix representing the coordinate
C                    transformation determined by AXIS and ANGLE:  for
C                    each vector V, R*V is the vector resulting from
C                    rotating V by ANGLE radians about AXIS.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If AXIS is the zero vector, the rotation generated is the
C         identity.  This is consistent with the specification of VROTV.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     AXISAR can be thought of as a partial inverse of RAXISA.  AXISAR
C     really is a `left inverse':  the code fragment
C
C        CALL RAXISA ( R,    AXIS,  ANGLE )
C        CALL AXISAR ( AXIS, ANGLE, R     )
C
C     preserves R, except for round-off error, as long as R is a
C     rotation matrix.
C
C     On the other hand, the code fragment
C
C        CALL AXISAR ( AXIS, ANGLE, R     )
C        CALL RAXISA ( R,    AXIS,  ANGLE )
C
C     preserves AXIS and ANGLE, except for round-off error, only if
C     ANGLE is in the range (0, pi).  So AXISAR is a right inverse
C     of RAXISA only over a limited domain.
C
C$ Examples
C
C     1)  A matrix that rotates vectors by pi/2 radians about the z-axis
C         can be found using the code fragment
C
C            AXIS(1) = 0.D0
C            AXIS(2) = 0.D0
C            AXIS(3) = 1.D0
C
C            CALL AXISAR ( AXIS, HALFPI(), R )
C
C         The returned matrix R will equal
C
C            +-               -+
C            |  0    -1     0  |
C            |                 |
C            |  1     0     0  |.
C            |                 |
C            |  0     0     1  |
C            +-               -+
C
C
C     2)  Linear interpolation between two rotation matrices:
C
C            Let R(t) be a time-varying rotation matrix; R could be
C            a C-matrix describing the orientation of a spacecraft
C            structure.  Given two points in time t1 and t2 at which
C            R(t) is known, and given a third time t3, where
C
C               t1  <  t3  <  t2,
C
C            we can estimate R(t3) by linear interpolation.  In other
C            words, we approximate the motion of R by pretending that
C            R rotates about a fixed axis at a uniform angular rate
C            during the time interval [t1, t2].  More specifically, we
C            assume that each column vector of R rotates in this
C            fashion.  This procedure will not work if R rotates through
C            an angle of pi radians or more during the time interval
C            [t1, t2]; an aliasing effect would occur in that case.
C
C            If we let
C
C               R1 = R(t1)
C               R2 = R(t2), and
C
C                           -1
C               Q  = R2 * R1  ,
C
C            then the rotation axis and angle of Q define the rotation
C            that each column of R(t) undergoes from time t1 to time
C            t2.  Since R(t) is orthogonal, we can find Q using the
C            transpose of R1.  We find the rotation axis and angle via
C            RAXISA.
C
C               CALL MXMT   ( R2,   R1,    Q      )
C               CALL RAXISA ( Q,    AXIS,  ANGLE  )
C
C            Find the fraction of the total rotation angle that R
C            rotates through in the time interval [t1, t3].
C
C               FRAC = ( T3 - T1 )  /  ( T2 - T1 )
C
C            Finally, find the rotation DELTA that R(t) undergoes
C            during the time interval [t1, t3], and apply that rotation
C            to R1, yielding R(t3), which we'll call R3.
C
C               CALL AXISAR ( AXIS,   FRAC * ANGLE,  DELTA  )
C               CALL MXM    ( DELTA,  R1,            R3     )
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
C-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VROTV call.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     axis and angle to rotation
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VROTV call.  
C    
C        Identity matrix is now obtained from IDENT.
C
C-& 
 
 
C
C     Local variables
C
      DOUBLE PRECISION      VTEMP ( 3 )
      INTEGER               I
 
C
C     First, set R equal to the identity.
C
      CALL IDENT ( R )
 
C
C     The matrix we want rotates EVERY vector by ANGLE about AXIS.
C     In particular, it does so to our basis vectors.  The columns
C     of R are the images of the basis vectors under this rotation.
C
      DO I = 1, 3
         CALL VROTV ( R(1,I), AXIS, ANGLE, VTEMP  )
         CALL VEQU  ( VTEMP,               R(1,I) )
      END DO
 
      RETURN
      END
