C$Procedure ZZCORSXF ( Correct state transformation matrix )

      SUBROUTINE ZZCORSXF ( XMIT, DLT, XFORM, CORXFM )
  
C$ Abstract
C
C     Correct a state transformation matrix for the rate of change of
C     light time.
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
C     SPK
C     TIME
C
C$ Keywords
C
C     FRAMES
C     MATRIX
C     ROTATION
C     STATE
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               XMIT
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      CORXFM ( 6, 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     XMIT       I   Radiation direction flag.
C     DLT        I   Light time derivative with respect to TDB.
C     XFORM      I   State transformation matrix.
C     CORXFM     O   Corrected state transformation matrix.
C
C$ Detailed_Input
C
C     XMIT           is a logical flag indicating the sense of
C                    radiation transmission associated with
C                    the light time correction: XMIT is .TRUE.
C                    for transmission corrections and .FALSE.
C                    for reception corrections. See the header
C                    of SPKEZR for a detailed discussion of
C                    light time corrections.
C
C     DLT            is the derivative of one way light time measured
C                    in TDB seconds with respect to TDB. DLT is
C                    signed and unitless.
C
C     XFORM          is a 6x6 state transformation matrix. XFORM
C                    may transform states from an inertial frame to a
C                    body-fixed frame or vice versa. XFORM has the form
C
C                        -               -
C                       |         :       |
C                       |  R(t)   :   0   |
C                       |........ :.......|
C                       |         :       |
C                       | d(R)/dt :  R(t) |
C                       |         :       |
C                        -               -
C
C                    where R(t) is a time-dependent rotation matrix.
C                    
C$ Detailed_Output
C
C     CORXFM         is the input matrix XFORM after correction for the
C                    rate of change of light time indicated by DLT. Let
C                    LTSIGN be 1 for transmission corrections and -1
C                    for reception corrections. Then CORXFM has the
C                    form
C
C                        -                        -
C                       |             :            |
C                       |     R(t)    :     0      |
C                       |.............:............|
C                       |             :            |
C                       | S * d(R)/dt :    R(t)    |
C                       |             :            |
C                        -                        -
C
C                    where 
C
C                       S = 1 + LTSIGN*DLT
C
C                    CORXFM may be used to transform state vectors
C                    between an inertial reference frame and a
C                    body-fixed reference frame associated with a
C                    light-time corrected epoch. See the Particulars
C                    section for details.
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
C     This is a utility routine designed to simplify transformation of
C     state vectors between an inertial and a body-fixed reference
C     frame, where the evaluation epoch of the body-fixed frame is
C     adjusted by a light time value.
C
C     For example, suppose the aberration-corrected velocity of a
C     target relative to an observer is to be transformed from an
C     inertial reference frame into a target centered, target
C     body-fixed reference frame, where the orientation of this frame
C     is to be corrected for one-way light time between a surface point
C     on the target body and the observer.
C
C     In the discussion below, we use ET as a synonym for TDB, since
C     this terminology is used throughout the SPICE system.
C
C     The orientation of the reference frame can be expressed as
C
C        R ( ET + LTSIGN*LT(ET) )
C
C     where R is a rotation matrix, ET is the TDB epoch associated with
C     an observer, LT(ET) is the light time magnitude associated with
C     the epoch ET at the observer, and LTSIGN is the sign of the light
C     time; LTSIGN is negative for reception case corrections.
C     
C     The expression
C
C        ET + LTSIGN*LT(ET)
C
C     represents the light time corrected epoch. Then, according to the
C     chain rule, the derivative with respect to ET of R is
C
C               |
C        d(R)/dt|                   * ( 1 + LTSIGN*d(LT)/d(ET) )
C               |ET + LTSIGN*LT(ET)
C    
C     In the expression above, the factor on the left is the rotation
C     derivative that could be obtained by calling SXFORM to look up
C     the inertial-to-body-fixed state transformation matrix at the
C     epoch
C
C        ET + LTSIGN*LT(ET)
C
C     This is the rotation derivative that would apply if light
C     time were constant.
C 
C     The factor on the right is the scale factor S shown in the
C     Detailed Output section above.
C
C$ Examples
C
C     1) Express the velocity of Mars as seen from Earth in
C        the IAU_MARS reference frame, where the frame orientation is
C        corrected for light time. Contrast the results obtained
C        using uncorrected and corrected state transformation matrices.
C        Show that the result obtained using a corrected matrix
C        matches that obtained from SPKEZR.
C
C        Note that, while the velocity we'll compute is not physically
C        realistic, it's perfectly usable for computations such as
C        finding the velocity of the apparent sub-Earth point on Mars.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C 
C
C           KPL/MK
C
C           File: zzcorsxf_ex1.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de421.bsp                     Planetary ephemeris
C              pck00008.tpc                  Planet orientation and
C                                            radii
C              naif0008.tls                  Leapseconds
C
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00008.tpc',
C                                  'naif0008.tls'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C
C        Example code begins here.
C
C
C           PROGRAM EX1
C           IMPLICIT NONE
C
C     C
C     C     Local variables
C     C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      DLT
C           DOUBLE PRECISION      XFORM  ( 6, 6 )
C           DOUBLE PRECISION      CORXFM ( 6, 6 )
C           DOUBLE PRECISION      STATE0 ( 6 )
C           DOUBLE PRECISION      STATE1 ( 6 )
C           DOUBLE PRECISION      STATE2 ( 6 )
C           DOUBLE PRECISION      STATE3 ( 6 )
C           DOUBLE PRECISION      VELDIF ( 3 )
C
C           INTEGER               I
C
C     C
C     C     Load kernels.
C     C
C           CALL FURNSH ( 'corsxf_ex1.tm' )
C
C     C
C     C     Convert an observation epoch to TDB.
C     C
C           CALL STR2ET ( '2008 MAR 23', ET )
C
C     C
C     C     Look up the aberration-corrected state
C     C     of Mars as seen from the Earth at ET
C     C     in the J2000 frame. Use SPKACS since this
C     C     routine returns the light time derivative.
C     C
C           CALL SPKACS ( 499, ET,     'J2000', 'LT+S',
C          .              399, STATE0, LT,      DLT     )
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Mars-Earth light time derivative = ', DLT
C
C     C
C     C     Convert the state into the IAU_MARS frame at
C     C     ET-LT. This gives us the state without accounting
C     C     for the rate of change of light time.
C     C
C           CALL SXFORM ( 'J2000', 'IAU_MARS', ET-LT, XFORM  )
C           CALL MXVG   ( XFORM,   STATE0,     6,  6, STATE1 )
C
C     C
C     C     Display the velocity portion of the state.
C     C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'IAU_MARS-relative velocity obtained '
C           WRITE (*,*) 'using SPKACS and SXFORM (km/s):'
C
C           WRITE (*, '(E24.16)' ) ( STATE1(I), I = 4, 6 )
C
C     C
C     C     Obtain the correct state transformation matrix
C     C     from ZZCORSXF; transform the state using this matrix.
C     C
C           CALL ZZCORSXF ( .FALSE., DLT,     XFORM, CORXFM )
C           CALL MXVG     ( CORXFM,   STATE0, 6,  6, STATE2 )
C
C     C
C     C     Display the velocity portion of the state.
C     C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'IAU_MARS-relative velocity obtained '
C          .//          'using ZZCORSXF (km/s):'
C
C           WRITE (*, '(E24.16)' ) ( STATE2(I), I = 4, 6 )
C
C     C
C     C     Display the velocity difference:
C     C
C           CALL VSUB ( STATE2(4), STATE1(4), VELDIF )
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Velocity difference (km/s):'
C           WRITE (*, '(E24.16)' ) ( VELDIF(I), I = 1, 3 )
C
C     C
C     C     Look up the desired state using SPKEZR for comparison.
C     C
C           CALL SPKEZR ( 'MARS',  ET,     'IAU_MARS', 'LT+S',
C          .              'EARTH', STATE3, LT                 )
C
C     C
C     C     Display the velocity portion of the state.
C     C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'IAU_MARS-relative velocity obtained '
C          .//          'using SPKEZR (km/s):'
C
C           WRITE (*, '(E24.16)' ) ( STATE3(I), I = 4, 6 )
C
C     C
C     C     Display the velocity difference:
C     C
C           CALL VSUB ( STATE3(4), STATE2(4), VELDIF )
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'SPKEZR vs ZZCORSXF velocity difference (km/s):'
C           WRITE (*, '(E24.16)' ) ( VELDIF(I), I = 1, 3 )
C
C           END
C
C
C        When this program was executed on a PC/Linux/g77 system, the 
C        output was
C
C           Mars-Earth light time derivative =   5.70610116E-05
C
C           IAU_MARS-relative velocity obtained
C           using SPKACS and SXFORM (km/s):
C            0.1094230439483713E+05
C           -0.7388150695390612E+04
C           -0.8550198289693935E+01
C
C           IAU_MARS-relative velocity obtained using ZZCORSXF (km/s):
C            0.1094167989684505E+05
C           -0.7387727898874676E+04
C           -0.8550198284585768E+01
C
C           Velocity difference (km/s):
C           -0.6244979920775222E+00
C            0.4227965159361702E+00
C            0.5108166334366615E-08
C
C           IAU_MARS-relative velocity obtained using SPKEZR (km/s):
C            0.1094167989684505E+05
C           -0.7387727898874676E+04
C           -0.8550198284585768E+01
C
C           SPKEZR vs ZZCORSXF velocity difference (km/s):
C            0.0000000000000000E+00
C            0.0000000000000000E+00
C            0.0000000000000000E+00
C
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
C-    SPICELIB Version 1.0.0, 06-MAY-2008 (NJB)
C
C-&
 
C$ Index_Entries
C
C     correct state transformation for light time rate
C
C-&

C
C     Local variables
C
      DOUBLE PRECISION      LTSIGN
      DOUBLE PRECISION      SCALE

      INTEGER               COL

C
C     Determine the sign of the light time correction.
C
      IF ( XMIT ) THEN

         LTSIGN =  1.D0
      ELSE
         LTSIGN = -1.D0
      END IF

C
C     Since the only block we're changing is
C     the lower left, first copy the input matrix 
C     to the output matrix.
C
      CALL MOVED ( XFORM, 36, CORXFM )

C
C     Adjust the rotation derivative block for
C     the rate of change of light time. All
C     that's required is to scale the block by
C     
C        1 + LTSIGN*DLT
C
C
      SCALE = 1.D0 + LTSIGN*DLT
      

      DO COL = 1, 3
C
C        Scale the vector starting at index
C        (4,COL) in place.
C
         CALL VSCLIP ( SCALE, CORXFM(4,COL) )

      END DO

      END 

