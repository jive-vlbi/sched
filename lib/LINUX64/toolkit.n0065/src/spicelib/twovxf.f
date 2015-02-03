C$Procedure TWOVXF ( Two states defining a frame transformation )
 
      SUBROUTINE TWOVXF ( AXDEF, INDEXA, PLNDEF, INDEXP, XFORM )
 
C$ Abstract
C
C     Find the state transformation from a base frame to the
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
C     XFORM      is the 6x6 matrix that transforms states from the
C                frame relative to which AXDEF and PLNDEF are specified
C                (the "base frame") to the frame whose axes and
C                derivative are determined by AXDEF, PLNDEF, INDEXA and
C                INDEXP.
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
C     from the base frame used to represent the input vectors to the
C     the frame F determined by AXDEF and PLNDEF.  Thus a state vector
C
C        S       = ( x, y, z, dx/dt, dy/dt, dz/dt ) 
C         base
C
C     in the input reference frame will be transformed to
C                     
C        S       = XFORM * S
C         F                 base
C  
C     in the frame F determined by AXDEF and PLNDEF.
C
C$ Examples
C
C     The time-dependent Sun-Canopus reference frame associated with a
C     spacecraft uses the spacecraft-sun state to define the Z axis and
C     the Canopus direction to define the X-Z plane.
C
C     Define an approximate "state vector" for Canopus using the
C     J2000-relative, unit direction vector toward Canopus at a
C     specified time ET (time is needed to compute proper motion) as
C     position and the zero vector as velocity.  Call this state vector
C     STCANO.  Let STSUN be the J2000-relative state of the sun
C     relative to the spacecraft at ET.
C
C     Then the matrix XFISC that transforms states from J2000 to the
C     Sun-Canopus reference frame at ET is returned by the call
C
C        CALL TWOVXF ( STSUN, 3, STCANO, 1, XFISC )
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
C-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) (WMO) (WLT)
C
C-&
 
C$ Index_Entries
C
C     define a state transformation matrix from two states
C
C-&

 
C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local Variables
C
      DOUBLE PRECISION      XI     ( 6, 6 )

 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TWOVXF' )
     
      
C
C     Get the matrix XI that transforms states from the frame
C     defined by AXDEF and PLNDEF to their base frame.
C
      CALL ZZTWOVXF ( AXDEF, INDEXA, PLNDEF, INDEXP, XI )

C
C     Invert XI.
C
      CALL INVSTM ( XI, XFORM )

      CALL CHKOUT ( 'TWOVXF' )
      RETURN 
      END
