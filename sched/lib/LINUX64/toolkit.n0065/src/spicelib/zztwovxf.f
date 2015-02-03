C$Procedure ZZTWOVXF ( Two states defining a frame transformation )
 
      SUBROUTINE ZZTWOVXF ( AXDEF, INDEXA, PLNDEF, INDEXP, XFORM )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Find the state transformation to a base frame from the
C     right-handed frame defined by two state vectors:  one state
C     vector defining a specified axis and a second state vector
C     defining a specified coordinate plane.
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
C     AXES
C     FRAMES
C     MATRIX
C     TRANSFORMATION
C
C$ Declarations
 
      DOUBLE PRECISION   AXDEF  ( 6 )
      INTEGER            INDEXA
      DOUBLE PRECISION   PLNDEF ( 6 )
      INTEGER            INDEXP
      DOUBLE PRECISION   XFORM  ( 6, 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     AXDEF      I   State defining a principal axis.
C     INDEXA     I   Principal axis number of AXDEF (X=1, Y=2, Z=3).
C     PLNDEF     I   State defining (with AXDEF) a principal plane.
C     INDEXP     I   Second axis number (with INDEXA) of principal
C                    plane.
C     XFORM      O   Output state transformation matrix.
C
C$ Detailed_Input
C
C     AXDEF      is a "generalized" state vector defining one of the
C                principal axes of a reference frame. This vector
C                consists of three components of a vector-valued
C                function of one independent variable t followed by
C                the derivatives of the components with respect to that
C                variable:
C
C                   ( a, b, c, da/dt, db/dt, dc/dt )
C
C                This routine treats the input states as unitless, but
C                in most applications the input states represent
C                quantities that have associated units. The first three
C                components must have the same units, and the units of
C                the last three components must be compatible with
C                those of the first three:  if the first three
C                components of AXDEF
C
C                   ( a, b, c )
C
C                have units U and t has units T, then the units of 
C                AXDEF normally would be
C
C                   ( U, U, U, U/T, U/T, U/T )
C
C                Note that the direction and angular velocity defined
C                by AXDEF are actually independent of U, so scaling
C                AXDEF doesn't affect the output of this routine.
C                
C                AXDEF could represent position and velocity; it could
C                also represent velocity and acceleration.  AXDEF could
C                for example represent the velocity and acceleration of
C                a time-dependent position vector ( x(t), y(t), z(t) ),
C                in which case AXDEF would be defined by
C 
C                   a     = dx/dt
C                   b     = dy/dt
C                   c     = dz/dt
C
C                            2      2
C                   da/dt = d x / dt
C
C                            2      2
C                   db/dt = d y / dt
C
C                            2      2
C                   dc/dt = d z / dt
C
C                Below, we'll call the normalized (unit length) version
C                of
C
C                   ( a, b, c )
C
C                the "direction" of AXDEF.
C
C                We call the frame relative to which AXDEF is specified
C                the "base frame."  The input state PLNDEF must be
C                specified relative to the same base frame.
C
C
C     INDEXA     is the index of the reference frame axis that is
C                parallel to the direction of AXDEF.
C
C                   Value of INDEXA             Axis
C 
C                         1                      X
C                         2                      Y
C                         3                      Z
C
C
C     PLNDEF     is a state vector defining (with AXDEF) a principal
C                plane of the reference frame.  This vector consists
C                of three components followed by their derivatives with
C                respect to the independent variable t associated with
C                AXDEF, so PLNDEF is
C
C                   ( e, f, g, de/dt, df/dt, dg/dt )
C
C                Below, we'll call the unitized version of 
C
C                   ( e, f, g )
C
C                the "direction" of PLNDEF.
C
C                The second axis of the principal plane containing the
C                direction vectors of AXDEF and PLNDEF is perpendicular
C                to the first axis and has positive dot product with
C                the direction vector of PLNDEF.
C
C                The first three components of PLNDEF must have the
C                same units, and the units of the last three components
C                must be compatible with those of the first three:  if
C                the first three components of PLNDEF 
C                
C                   ( e, f, g )
C                
C                have units U2 and t has units T, then the units of
C                PLNDEF normally would be
C
C                   ( U2, U2, U2, U2/T, U2/T, U2/T )
C 
C                ***For meaningful results, the angular velocities
C                   defined by AXDEF and PLNDEF must both have units of
C                   1/T.***
C
C                As with AXDEF, scaling PLNDEF doesn't affect the 
C                output of this routine.
C
C                AXDEF and PLNDEF must be specified relative to a
C                common reference frame, which we call the "base
C                frame."
C
C
C     INDEXP     is the index of  second axis of the principal frame
C                determined by AXDEF and PLNDEF.  The association of
C                integer values and axes is the same as for INDEXA.
C
C$ Detailed_Output
C
C     XFORM      is the 6x6 matrix that transforms states to the frame
C                relative to which AXDEF and PLNDEF are specified (the
C                "base frame") from the frame whose axes and derivative
C                are determined by AXDEF, PLNDEF, INDEXA and INDEXP.
C
C                The matrix XFORM has the structure shown below:
C
C                    -            -
C                   |       :      |
C                   |   R   :  0   |
C                   | ......:......|
C                   |       :      |
C                   | dR_dt :  R   |
C                   |       :      |
C                    -            -
C
C                where R is a rotation matrix that is a function of
C                the independent variable associated with AXDEF and
C                PLNDEF, and where dR_dt is the derivative of R
C                with respect to that independent variable.
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
C     This routine exists to support the public routine TWOVXF:
C     TWOVXF does its job by calling this routine, inverting the
C     matrix returned by this routine, and returning the result.
C
C     The SPICELIB frame subsystem typically requires this routine
C     rather than TWOVXF, since the frame subsystem produces 
C     transformations from frames defined in frame kernels to their
C     base frames.  Calling this routine rather than TWOVXF allows
C     the frame subsystem to eliminate two unnecessary calls to 
C     INVSTM.
C
C     Given two linearly independent state vectors AXDEF and PLNDEF,
C     define vectors DIR1 and DIR2 by
C
C        DIR1 = ( AXDEF(1),   AXDEF(2),   AXDEF(3)  ) 
C        DIR2 = ( PLNDEF(1),  PLNDEF(2),  PLNDEF(3) )
C 
C     Then there is a unique right-handed reference frame F having:
C
C        DIR1 lying along the INDEXA axis.
C
C        DIR2 lying in the INDEXA-INDEXP coordinate plane, such that
C        the dot product of DIR2 with the positive INDEXP axis is
C        positive.
C
C     This routine determines the 6x6 matrix that transforms states
C     to the base frame used to represent the input vectors from the
C     the frame F determined by AXDEF and PLNDEF.  Thus a state vector
C
C        S       = ( x, y, z, dx/dt, dy/dt, dz/dt ) 
C         F
C
C     in the reference frame F will be transformed to
C                     
C        S      = XFORM * S
C         base             F
C
C     in the base frame relative to which AXDEF and PLNDEF are 
C     specified.
C
C$ Examples
C
C     See TWOVXF.
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
C-    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in DUCRSS and MOVED calls.
C
C-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) (WMO) (WLT)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in DUCRSS and MOVED calls.
C
C-& 



C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local Variables
C
      DOUBLE PRECISION      TMPSTA ( 6 )

      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               J
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
      END IF

      CALL CHKIN ( 'ZZTWOVXF' )
     
      
C
C     Check for obvious bad inputs.
C
      IF (      ( MAX(INDEXP,INDEXA) .GT. 3 )
     .     .OR. ( MIN(INDEXP,INDEXA) .LT. 1 ) ) THEN
 
         CALL SETMSG ( 'The definition indices must lie in the range '//
     .                 'from 1 to 3.  The value of INDEXA was #. '    //
     .                 'The value of INDEXP was #. '                  )
         CALL ERRINT ( '#', INDEXA       )
         CALL ERRINT ( '#', INDEXP       )
         CALL SIGERR ( 'SPICE(BADINDEX)' )
         CALL CHKOUT ( 'ZZTWOVXF'        )
         RETURN
 
      ELSE IF ( INDEXA .EQ. INDEXP ) THEN
 
         CALL SETMSG ( 'The values of INDEXA and INDEXP were the '    //
     .                 'same, namely #.  They are required to be '    //
     .                 'different.'                                   )
         CALL ERRINT ( '#', INDEXA             )
         CALL SIGERR ( 'SPICE(UNDEFINEDFRAME)' )
         CALL CHKOUT ( 'ZZTWOVXF'              )
         RETURN
 
      END IF
 
C
C     Get indices for right-handed axes:
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
C     Column I1 of XFORM contains a unit vector parallel to AXDEF and
C     the derivative of the unit vector.
C
      CALL DVHAT ( AXDEF, XFORM(1,I1) )
 
C
C     Obtain columns I2 and I3 of XFORM using cross products. 
C     Which order to use depends on whether INDEXP = I2 (next axis in
C     right-handed order) or INDEXP = I3 (previous axis in right-handed
C     order).
C       
C     Select column indices...
C
      IF ( INDEXP .EQ. I2 ) THEN
C
C        We compute the third axis in the sequence, then the second.
C
         CALL DUCRSS ( AXDEF,        PLNDEF,        XFORM(1,I3) )
         CALL DUCRSS ( XFORM(1,I3),  AXDEF,         TMPSTA      )
         CALL MOVED  ( TMPSTA,       6,             XFORM(1,I2) )
      ELSE
         CALL DUCRSS ( PLNDEF,       AXDEF,         XFORM(1,I2) )
         CALL DUCRSS ( AXDEF,        XFORM(1,I2),   TMPSTA      )
         CALL MOVED  ( TMPSTA,       6,             XFORM(1,I3) )
      END IF

C
C     ...and compute the output frame's non-principal unit basis
C     vectors and the derivatives of these vectors.
C
 
C
C     At this point, we've filled in the left half of XFORM.
C
C     The upper right block is the 3x3 zero matrix.
C     The lower right block matches the upper left block.
C 
      CALL CLEARD ( 3,                XFORM(1,4) )
      CALL CLEARD ( 3,                XFORM(1,5) )
      CALL CLEARD ( 3,                XFORM(1,6) )

      DO J = 1, 3
         
         DO I = 1, 3

            XFORM(3+I,3+J) = XFORM(I,J)

         END DO

      END DO

C
C     Finally, check to see that we actually got something non-zero in
C     the first three components of at least one of the columns
C     XFORM(1,I2) and XFORM(1,I3) (we need only check one of them since
C     they are related by a cross product).
C
      IF (  VZERO ( XFORM(1,I2) )  ) THEN
 
         CALL SETMSG ( 'The direction vectors associated with ' //
     .                 'states AXDEF and PLNDEF are '           //
     .                 'linearly dependent.'                    )
         CALL SIGERR ( 'SPICE(DEPENDENTVECTORS)'                )
         CALL CHKOUT ( 'ZZTWOVXF'                               )
         RETURN
 
      END IF

      CALL CHKOUT ( 'ZZTWOVXF' )
      RETURN 
      END
