C$Procedure      TWOVEC ( Two vectors defining an orthonormal frame )
 
      SUBROUTINE TWOVEC ( AXDEF, INDEXA, PLNDEF, INDEXP, MOUT )
 
C$ Abstract
C
C     Find the transformation to the right-handed frame having a
C     given vector as a specified axis and having a second given
C     vector lying in a specified coordinate plane.
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
C     AXES,  FRAME,  ROTATION,  TRANSFORMATION
C
C$ Declarations
 
      DOUBLE PRECISION   AXDEF  (   3 )
      INTEGER            INDEXA
      DOUBLE PRECISION   PLNDEF (   3 )
      INTEGER            INDEXP
      DOUBLE PRECISION   MOUT   ( 3,3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     AXDEF      I   Vector defining a principal axis.
C     INDEXA     I   Principal axis number of AXDEF (X=1, Y=2, Z=3).
C     PLNDEF     I   Vector defining (with AXDEF) a principal plane.
C     INDEXP     I   Second axis number (with INDEXA) of principal
C                     plane.
C     MOUT       O   Output rotation matrix.
C
C$ Detailed_Input
C
C     AXDEF      is a vector defining one of the priciple axes of a
C                coordinate frame.
C
C     INDEXA     is a number that determines which of the three
C                coordinate axes contains AXDEF.
C
C                If INDEXA is 1 then AXDEF defines the X axis of the
C                coordinate frame.
C
C                If INDEXA is 2 then AXDEF defines the Y axis of the
C                coordinate frame.
C
C                If INDEXA is 3 then AXDEF defines the Z axis of the
C                coordinate frame
C
C     PLNDEF     is a vector defining (with AXDEF) a principal plane of
C                the coordinate frame. AXDEF and PLNDEF must be
C                linearly independent.
C
C     INDEXP     is the second axis of the principal frame determined
C                by AXDEF and PLNDEF.  INDEXA, INDEXP must be different
C                and be integers from 1 to 3.
C
C                If INDEXP is 1, the second axis of the principal
C                plane is the X-axis.
C
C                If INDEXP is 2, the second axis of the principal
C                plane is the Y-axis.
C
C                If INDEXP is 3, the second axis of the principal plane
C                is the Z-axis.
C     
C
C$ Detailed_Output
C
C     MOUT       is a rotation matrix that transforms coordinates given
C                in the input frame to the frame determined by AXDEF,
C                PLNDEF, INDEXA and INDEXP.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If INDEXA or INDEXP is not in the set {1,2,3} the error
C        SPICE(BADINDEX) will be signaled.
C
C     2) If INDEXA and INDEXP are the same the error
C        SPICE(UNDEFINEDFRAME) will be signaled.
C
C     3) If the cross product of the vectors AXDEF and PLNDEF is zero,
C        the error SPICE(DEPENDENTVECTORS) will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given two linearly independent vectors there is a unique
C     right-handed coordinate frame having:
C
C        AXDEF lying along the INDEXA axis.
C
C        PLNDEF lying in the INDEXA-INDEXP coordinate plane.
C
C     This routine determines the transformation matrix that transforms
C     from coordinates used to represent the input vectors to the
C     the system determined by AXDEF and PLNDEF.  Thus a vector
C     (x,y,z) in the input coordinate system will have coordinates
C
C                     t
C        MOUT* (x,y,z)
C
C     in the frame determined by AXDEF and PLNDEF.
C
C$ Examples
C
C     The rotation matrix TICC from inertial to Sun-Canopus
C     (celestial) coordinates is found by the call
C
C        CALL TWOVEC (Sun vector, 3, Canopus vector, 1, TICC)
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.M. Owen       (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0,  31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     define an orthonormal frame from two vectors
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-    Beta Version 2.0.0, 10-JAN-1989 (WLT)
C
C     Error checking was added and the algorithm somewhat redesigned.
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C     
      DOUBLE PRECISION      MTEMP  ( 3, 3 )

      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               SEQNCE ( 5 )

C
C     Saved variables
C
      SAVE                  SEQNCE
 
C
C     Initial values
C
      DATA                  SEQNCE  / 1, 2, 3, 1, 2 /
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TWOVEC' )
      END IF
 
C
C     Check for obvious bad inputs.
C
      IF (      ( MAX(INDEXP,INDEXA) .GT. 3 )
     .     .OR. ( MIN(INDEXP,INDEXA) .LT. 1 ) ) THEN
 
         CALL SETMSG ( 'The definition indexs must lie in the range ' //
     .                 'from 1 to 3.  The value of INDEXA was #. '    //
     .                 'The value of INDEXP was #. '                  )
 
         CALL ERRINT ( '#', INDEXA  )
         CALL ERRINT ( '#', INDEXP  )
         CALL SIGERR ( 'SPICE(BADINDEX)' )
         CALL CHKOUT (     'TWOVEC' )
         RETURN
 
      ELSE IF   ( INDEXA .EQ. INDEXP )  THEN
 
         CALL SETMSG ( 'The values of INDEXA and INDEXP were the '    //
     .                 'same, namely #.  They are required to be '    //
     .                 'different.'                                   )
 
         CALL ERRINT ( '#', INDEXA  )
         CALL SIGERR ( 'SPICE(UNDEFINEDFRAME)' )
         CALL CHKOUT (     'TWOVEC' )
         RETURN
 
      END IF
 
C
C     Get indices for right-handed axes
C
C     First AXDEF ...
C
      I1 = INDEXA
C
C     ... then the other two.
C
      I2 = SEQNCE ( INDEXA + 1 )
      I3 = SEQNCE ( INDEXA + 2 )
 
C
C     Row I1 contains normalized AXDEF (store in columns for now)
C
      CALL VHAT ( AXDEF, MOUT(1,I1) )
 
C
C     Obtain rows I2 and I3 using cross products.  Which order to use
C     depends on whether INDEXP = I2 (next axis in right-handed order)
C     or INDEXP = I3 (previous axis in right-handed order).
C
      IF ( INDEXP .EQ. I2 ) THEN
 
         CALL UCRSS ( AXDEF,      PLNDEF,        MOUT(1,I3) )
         CALL UCRSS ( MOUT(1,I3), AXDEF,         MOUT(1,I2) )
 
      ELSE
 
         CALL UCRSS ( PLNDEF,     AXDEF,         MOUT(1,I2) )
         CALL UCRSS ( AXDEF,      MOUT(1,I2),    MOUT(1,I3) )
 
      END IF
 
C
C     Finally, check to see that we actually got something non-zero
C     in one of the one columns of MOUT(1,I2) and MOUT(1,I3) (we need
C     only check one of them since they are related by a cross product).
C
      IF (       ( MOUT(1,I2) .EQ. 0.0D0 )
     .     .AND. ( MOUT(2,I2) .EQ. 0.0D0 )
     .     .AND. ( MOUT(3,I2) .EQ. 0.0D0 ) ) THEN
 
         CALL SETMSG ( 'The input vectors AXDEF and PLNDEF are '      //
     .                 'linearly dependent.'                          )
         CALL SIGERR ( 'SPICE(DEPENDENTVECTORS)' )
 
      END IF
C
C     Transpose MOUT.
C
      CALL XPOSE ( MOUT,     MTEMP )
      CALL MOVED ( MTEMP, 9, MOUT  )

      CALL CHKOUT ( 'TWOVEC' )
      RETURN
      END
