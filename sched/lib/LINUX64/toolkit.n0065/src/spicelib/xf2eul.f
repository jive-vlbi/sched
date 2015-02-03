C$Procedure      XF2EUL ( State transformation to Euler angles )
 
      SUBROUTINE XF2EUL ( XFORM, AXISA, AXISB, AXISC, EULANG, UNIQUE )
 
C$ Abstract
C
C     Convert a state transformation matrix to Euler angles and their
C     derivatives with respect to a specified set of axes.
C
C     The companion entry point EUL2XF converts Euler angles and their
C     derivatives with respect to a specified set of axes to a state
C     transformation matrix.
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
C     PCK
C
C$ Keywords
C
C     ANGLES
C     STATE
C     DERIVATIVES
C
C$ Declarations
 
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      INTEGER               AXISA
      INTEGER               AXISB
      INTEGER               AXISC
      DOUBLE PRECISION      EULANG ( 6 )
      LOGICAL               UNIQUE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     XFORM      I   A state transformation matrix.
C     AXISA      I   Axis A of the Euler angle factorization.
C     AXISB      I   Axis B of the Euler angle factorization.
C     AXISC      I   Axis C of the Euler angle factorization.
C     EULANG     O   An array of Euler angles and their derivatives.
C     UNIQUE     O   Indicates if EULANG is a unique representation.
C
C$ Detailed_Input
C
C     XFORM       is a state transformation from some frame FRAME1 to
C                 another frame FRAME2.  Pictorially, XFORM has the
C                 structure shown here.
C
C                      [       |        ]
C                      |  R    |    0   |
C                      |       |        |
C                      |-------+--------|
C                      |       |        |
C                      | dR/dt |    R   |
C                      [       |        ]
C
C                 where R is a rotation that varies with respect to time
C                 and dR/dt is its time derivative.
C
C                 More specifically, if S1 is the state of some object
C                 in FRAME1, then S2, the state of the same object
C                 relative to FRAME2 is given by
C
C                    S2 = XFORM*S1
C
C                 where '*' denotes the matrix vector product.
C
C     AXISA       are the axes desired for the factorization of R.
C     AXISB       All must be in the range from 1 to 3.  Moreover
C     AXISC       it must be the case that AXISA and AXISB are distinct
C                 and that AXISB and AXISC are distinct.
C
C                 Every rotation matrix can be represented as a product
C                 of three rotation matrices about the principal axes
C                 of a reference frame.
C
C                     R =  [ ALPHA ]     [ BETA ]     [ GAMMA ]
C                                   AXISA        AXISB         AXISC
C
C                 The value 1 corresponds to the X axis.
C                 The value 2 corresponds to the Y axis.
C                 The value 3 corresponds to the Z axis.
C
C$ Detailed_Output
C
C     EULANG      is the set of Euler angles corresponding to the
C                 specified factorization.
C
C                 If we represent R as shown here:
C
C                     R =  [ ALPHA ]     [ BETA ]     [ GAMMA ]
C                                   AXISA        AXISB         AXISC
C
C                 then
C
C
C                    EULANG(1) = ALPHA
C                    EULANG(2) = BETA
C                    EULANG(3) = GAMMA
C                    EULANG(4) = dALPHA/dt
C                    EULANG(5) = dBETA/dt
C                    EULANG(6) = dGAMMA/dt
C
C                 The range of ALPHA and GAMMA is (-pi, pi].
C
C                 The range of BETA depends on the exact set of
C                 axes used for the factorization.  For
C                 factorizations in which the first and third axes
C                 are the same, the range of BETA is [0, pi].
C
C                 For factorizations in which the first and third
C                 axes are different, the range of BETA is
C                 [-pi/2, pi/2].
C
C                 For rotations such that ALPHA and GAMMA are not
C                 uniquely determined, ALPHA and dALPHA/dt will
C                 always be set to zero; GAMMA and dGAMMA/dt are
C                 then uniquely determined.
C
C     UNIQUE      is a logical that indicates whether or not the
C                 values in EULANG are uniquely determined.  If
C                 the values are unique then UNIQUE will be set to
C                 TRUE.  If the values are not unique and some
C                 components ( EULANG(1) and EULANG(4) ) have been set
C                 to zero, then UNIQUE will have the value FALSE.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     All erroneous inputs are diagnosed by routines in the call
C     tree to this routines.  These include
C
C     1)   If any of AXISA, AXISB, or AXISC do not have values in
C
C             { 1, 2, 3 },
C
C          then the error SPICE(INPUTOUTOFRANGE) is signaled.
C
C     2)   An arbitrary rotation matrix cannot be expressed using
C          a sequence of Euler angles unless the second rotation axis
C          differs from the other two.  If AXISB is equal to AXISC or
C          AXISA, then the error SPICE(BADAXISNUMBERS) is signaled.
C
C     3)   If the input matrix R is not a rotation matrix, the error
C          SPICE(NOTAROTATION) is signaled.
C
C     4)   If EULANG(1) and EULANG(3) are not uniquely determined,
C          EULANG(1) is set to zero, and EULANG(3) is determined.
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
C     indicates a coordinate system rotation of x radians about the
C     ith coordinate axis.  To be specific, the symbol
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
C
C     The input matrix is assumed to be the product of three
C     rotation matrices, each one of the form
C
C        +-                    -+
C        |  1      0       0    |
C        |                      |
C        |  0    cos(r)  sin(r) |     (rotation of r radians about the
C        |                      |      x-axis),
C        |  0   -sin(r)  cos(r) |
C        +-                    -+
C
C
C        +-                    -+
C        | cos(s)   0   -sin(s) |
C        |                      |
C        |  0       1      0    |     (rotation of s radians about the
C        |                      |      y-axis),
C        | sin(s)   0    cos(s) |
C        +-                    -+
C
C     or
C
C        +-                    -+
C        |  cos(t)  sin(t)   0  |
C        |                      |
C        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
C        |                      |      z-axis),
C        |  0        0       1  |
C        +-                    -+
C
C     where the second rotation axis is not equal to the first or
C     third.  Any rotation matrix can be factored as a sequence of
C     three such rotations, provided that this last criterion is met.
C
C     This routine is related to the routine EUL2XF which produces
C     a state transformation from an input set of axes, Euler angles
C     and derivatives.
C
C     The two subroutine calls shown here will not change
C     XFORM except for round off errors.
C
C     CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
C     CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
C
C     On the other hand the two calls
C
C     CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
C     CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
C
C     will leave EULANG unchanged only if the components of EULANG
C     are in the range produced by EUL2XF and the Euler representation
C     of the rotation component of XFORM is unique within that range.
C
C
C$ Examples
C
C     Suppose that you wish to determine the rate of change of
C     the right ascension and declination of the pole of an object,
C     from the state transformation matrix that transforms J2000
C     states to object fixed states.
C
C     Using this routine with the routine TISBOD you can determine
C     these instanteous rates.
C
C     Recall that the rotation component of TSIPM is given by
C
C                   [W] [HALFPI-DEC] [RA+HALFPI]
C                      3            1           3
C
C
C     Thus the calls:
C
C     CALL TISBOD ( 'J2000', BODY, ET, TSIPM )
C     CALL XF2EUL (  TSIPM,  3, 1, 3,  EULANG, UNIQUE )
C
C     yield the following:
C
C        EULANG(1) is  W
C        EULANG(2) is  HALFPI - DEC
C        EULANG(3) is  RA     + HALFPI
C        EULANG(4) is  dW/dt
C        EULANG(5) is -dDEC/dt
C        EULANG(6) is  dRA/dt
C
C     Hence:
C
C        dDEC/dt = -EULANG(5)
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 25-APR-2007 (EDW)
C
C      Corrected code in EUL2EF entry point Examples section, example
C      showed a XF2EUL call:
C      
C            CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG )
C       
C      The proper form of the call:
C      
C            CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
C
C-    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB)
C
C        Entry point EUL2XF was updated to allow axis sequences
C        in which the second angle is not distinct from the first
C        or third.
C
C-    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Euler angles and derivatives from state transformation
C
C-&
C
C     Spicelib Functions.
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Parameters
C
      INTEGER               ALPHA
      INTEGER               BETA
      INTEGER               GAMMA
      INTEGER               DALPHA
      INTEGER               DBETA
      INTEGER               DGAMMA
 
      PARAMETER           ( ALPHA  = 1 )
      PARAMETER           ( BETA   = 2 )
      PARAMETER           ( GAMMA  = 3 )
      PARAMETER           ( DALPHA = 4 )
      PARAMETER           ( DBETA  = 5 )
      PARAMETER           ( DGAMMA = 6 )
 
C
C     Local variables
C
      DOUBLE PRECISION      CA
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DELTA  ( 3, 3 )
      DOUBLE PRECISION      DOMEGA ( 3 )
      DOUBLE PRECISION      DRDT   ( 3, 3 )
      DOUBLE PRECISION      DRDTRT ( 3, 3 )
      DOUBLE PRECISION      LOCANG ( 6 )
      DOUBLE PRECISION      OMEGA  ( 3 )
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      SA
      DOUBLE PRECISION      SOLUTN ( 3, 3 )
      DOUBLE PRECISION      U
      DOUBLE PRECISION      V
 
 
      INTEGER               A
      INTEGER               B
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               LOCAXA
      INTEGER               LOCAXB
      INTEGER               LOCAXC
      INTEGER               NEXT   ( 3 )
 
C
C     Saved variables
C
      SAVE                  DELTA
      SAVE                  NEXT
 
C
C     Initial values
C
C     Keep in mind that matrices are stored in column order first so
C     the matrix below looks like the transpose of what's needed.  But
C     in fact it is the correct thing.
C
 
      DATA                  DELTA   /  0.0D0, -1.0D0,  1.0D0,
     .                                 1.0D0,  0.0D0, -1.0D0,
     .                                -1.0D0,  1.0D0,  0.0D0  /
 

      DATA                  NEXT   / 2, 3, 1 /
 

 
C
C     The computation of the non-derivative terms EULANG is handled
C     by the SPICE routine M2EUL.  This routine contributes by
C     determining the derivative components of EULANG.
C
C     To understand the code below a rather lengthy derivation is
C     required.  If you're not interested in the details of this
C     derivation skip down to the  IF ( RETURN() ) THEN line of
C     code below.
C
C     First we note that if b is one of the basis vectors i,j, or k
C     or the opposite of one of these (-i, -j, or -k) then
C
C       [ ANGLE ]  * b  = COS( {1 - |<e_n,b>|}*ANGLE )b
C                n
C                       - SIN( ANGLE ) e_n x b
C
C     where <,> denotes the dot product, and x is used to denote the
C     cross product operation and e_1, e_2, and e_3 are the standard
C     basis vectors i, j, and k respectively.
C
C     Using M2EUL we can readily determine the values of ALPHA, BETA
C     and GAMMA such that
C
C
C        R   = [ ALPHA ]  [ BETA ]  [ GAMMA ]
C                       A         B          C
C
C
C    From this equation we have:
C
C        dR/dt =   dALPHA/dt OMEGA [ ALPHA ]  [ BETA ]  [ GAMMA ]
C                                 A         A         B          C
C
C              +   dBETA/dt  [ ALPHA ] OMEGA  [ BETA ]  [ GAMMA ]
C                                     A     B         B          C
C
C              +   dGAMMA/dt [ ALPHA ] [ BETA ]  OMEGA [ GAMMA ]
C                                     A        B      C         C
C
C     where OMEGA   is the cross product matrix.
C                n
C
C
C         [   0      D_3n    -D_2n  ]
C         |  -D_3n    0       D_1n  |
C         [   D_2n  -D_1n      0    ]
C
C
C     (D_ij   denotes the Kronecker delta.)  Note that OMEGA * v
C                                                           n
C     yields -e  x  v  for all vectors v.
C              n
C
C     Multiplying both sides of the equation for dR/dt by the transpose
C     of R yields:
C
C            T
C     dR/dt*R  = dALPHA/dt OMEGA
C                                A
C
C              + dBETA/dt  [ ALPHA ] OMEGA  [ -ALPHA ]
C                                   A     B           A
C
C              + dGAMMA/dt [ ALPHA ] [ BETA ] OMEGA [ -BETA ]  [-ALPHA]
C                                   A        B     C         B         A
C                        T
C     The product dR/dt*R  is a skew symmetric matrix and hence can
C     be represented as a cross product,
C               T
C        dR/dt*R  V  = W x V
C
C     for all vectors V, provided that
C
C                       T
C        W(1) =  dR/dt*R  (3,2)
C
C                       T
C        W(2) =  dR/dt*R  (1,3)
C
C                       T
C        W(3) =  dR/dt*R  (2,1)
C
C     For any vector V, there is a corresponding skew symmetric
C     matrix CROSS{V}  such that CROSS{V} * W  = V x W for all vectors
C     W.  Moreover, if ROT is any rotation, then
C
C                                           T
C           CROSS{ROT(V)} = ROT CROSS{V} ROT
C
C     This can easily be verified by noting that
C
C        ROT(VxU) = ROT(V) X ROT(U)
C
C     From these observations it follows that
C
C
C        W =   -dALPHA/dt e_A
C
C
C          -    dBETA/dt [ALPHA]  e_B
C                               A
C
C          -    dGAMMA/dt [ ALPHA ] [ BETA ] e_C
C                                  A        B
C
C
C        W =   -dALPHA/dt e_A
C
C
C          -    dBETA/dt {    COS ( ALPHA (1 - |<e_A,e_B>|)) e_B
C
C                          -  SIN ( ALPHA ) e_A x e_B }
C
C
C          -    dGAMMA/dt [ ALPHA ] {    COS(BETA(1 - |<e_B,e_C>|)) e_C
C                                  A
C                                     -  SIN (BETA) e_B x e_C }
C
C     But <e_A,e_B> = 0 = <e_B,e_C> so that the above expression
C     simplifies to
C
C        W =   -dALPHA/dt e_A
C
C
C          -    dBETA/dt {COS(ALPHA)e_B -  SIN(ALPHA) e_A x e_B}
C
C
C          -    dGAMMA/dt [ ALPHA ] {COS(BETA)e_C - SIN(BETA)e_B x e_C}
C                                  A
C
C     If we let L = 6 - A - B, then by construction e_L is the third
C     vector needed to complete the basis containing e_A and e_B.
C     Let D be +1 or -1, so that D*e_L = e_A x e_B
C     (note D = <e_L,e_A x e_B> )
C
C     Then applying our rotation formula again and simplifying we have
C
C     W =   -dALPHA/dt e_A
C
C
C       -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
C
C
C       -  dGAMMA/dt COS(BETA){ COS(ALPHA(1-<e_A , e_C>))e_C
C                              -SIN(ALPHA)   e_A x e_C }
C
C       +  dGAMMA/dt SIN(BETA){ COS(ALPHA(1-|<e_A,e_B x e_C>|))e_B x e_C
C                              -SIN(ALPHA) e_A x (e_B x e_C )
C
C
C     Now we have two cases: 1) e_A = e_C or 2)  e_C = e_L
C
C     Case 1. e_A = e_C
C     ====================
C
C        W =   -dALPHA/dt e_A
C
C
C          -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
C
C
C          -  dGAMMA/dt COS(BETA)e_A
C
C          -  dGAMMA/dt D*SIN(BETA)COS(ALPHA)e_L
C
C          -  dGAMMA/dt SIN(BETA)SIN(ALPHA)e_B
C
C
C        W = e_A{-dALPHA/dt - COS(BETA)dGAMMA/dt}
C          + e_B{ -COS(ALPHA)dBETA/dt -   SIN(ALPHA)SIN(BETA)dGAMMA/dt}
C          + e_L{D*SIN(ALPHA)dBETA/dt - D*COS(ALPHA)SIN(BETA)dGAMMA/dt}
C
C
C        let U =    COS(BETA)
C            V =  D*SIN(BETA)
C
C        then
C
C        W = e_A{-dALPHA/dt                                -U*dGAMMA/dt}
C          + e_B{         -COS(ALPHA)dBETA/dt -D*SIN(ALPHA)*V*dGAMMA/dt}
C          + e_L{        D*SIN(ALPHA)dBETA/dt   -COS(ALPHA)*V*dGAMMA/dt}
C
C
C     Case 2. e_L = e_C
C     ====================
C
C        W =   -dALPHA/dt e_A
C
C
C          -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
C
C
C          -  dGAMMA/dt COS(BETA){ COS(ALPHA)e_L
C                                 -D*SIN(ALPHA)e_B }
C
C          +  dGAMMA/dt SIN(BETA) D*e_A
C
C
C       W  = e_A{-dALPHA/dt + D*SIN(BETA)dGAMMA/dt}
C          + e_B{-COS(ALPHA)dBETA/dt  - D*SIN(ALPHA)COS(BETA)dGAMMA/dt}
C          + e_L{D*SIN(ALPHA)dBETA/dt -   COS(ALPHA)COS(BETA)dGAMMA/dt}
C
C
C       Let U = -D*SIN(BETA)
C           V =    COS(BETA)
C
C       then
C
C       W  = e_A{-dALPHA/dt                  -              U*dGAMMA/dt}
C          + e_B{       -COS(ALPHA)*dBETA/dt - D*SIN(ALPHA)*V*dGAMMA/dt}
C          + e_L{      D*SIN(ALPHA)dBETA/dt  -   COS(ALPHA)*V*dGAMMA/dt}
C
C     As we can see from the above, by choosing appropriate assignments
C     for U and V, the two cases can be unified in a single expression.
C
C     Substituting CA and SA for COS(ALPHA) and SIN(ALPHA) and
C     re-writing the last expression in matrix form we have:
C
C
C                          [ -1     0      0 ][ 1  0  U ] [dALPHA/dt]
C      W  = {e_A  e_B  e_L}|  0   -CA  -D*SA || 0  1  0 | |dBETA /dt|
C                          [  0  D*SA    -CA ][ 0  0  V ] [dGAMMA/dt]
C
C
C     If we let E_n stand for the transpose of e_n, then solving for
C     the derivative vector we have:
C
C     [dALPHA/dt]   [ 1 0 -U/V ] [ -1     0     0] [ E_A ]
C     |dBETA /dt| = | 0 1   0  | |  0   -CA  D*SA| | E_B | W
C     [dGAMMA/dt]   [ 0 0  1/V ] [  0 -D*SA   -CA] [ E_L ]
C
C
C     But since the matrix product E_n W is <e_n,W> = W(n) this can
C     be rewritten as
C
C     [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ W(A) ]
C     |dBETA /dt| = |  0   -CA      D*SA   | [ W(B) |
C     [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ W(L) ]
C
C
C     Thus we see that there is a relatively elementary computation
C     required to determine the derivatives of the three Euler angles
C     returned by M2EUL.
C
C
C     Standard SPICE exception handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'XF2EUL' )
 
C
C     Get the rotation and derivative of the rotation separately.
C
      DO I = 1,3
 
         K = I + 3
 
         DO J = 1,3
            R    (I,J) = XFORM(I,J)
            DRDT (I,J) = XFORM(K,J)
         END DO
 
      END DO
 
C
C     We have to do it sooner or later so we take care of getting
C     the various Euler angles now.  This will take care of all the
C     bad axis cases too so we don't have to check here.
C
      CALL M2EUL ( R, AXISA,         AXISB,        AXISC,
     .                EULANG(ALPHA), EULANG(BETA), EULANG(GAMMA) )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'XF2EUL' )
         RETURN
      END IF
 
C
C     Construct local copies of the axes, determine L and D from the
C     derivation above.
C
      A = AXISA
      B = AXISB
      L = 6 - A - B
      D = DELTA(A,B)
 
C
C                      t
C     Compute DR/DT * R   and extract OMEGA
C
      CALL MXMT ( DRDT, R, DRDTRT )
 
C
C     The vector corresponding to DRDTRT is computed as shown below.
C
C        w(1) = drdtrt (3,2)
C        w(2) = drdtrt (1,3)
C        w(3) = drdtrt (2,1)
C
C     However, we need the 3-vector
C
C        w(A)
C        w(B)
C        w(L)
C
C     We'll call this vector omega. It's computed as shown here.
C
C        omega(1) = w(A) = d*drdtrt(L,B)
C        omega(2) = w(B) = d*drdtrt(A,L)
C        omega(3) = w(L) = d*drdtrt(B,A)
C
 
      OMEGA(1) = D*DRDTRT(L,B)
      OMEGA(2) = D*DRDTRT(A,L)
      OMEGA(3) = D*DRDTRT(B,A)
 
C
C     Compute the various sines and cosines that we need.
C
      CA       = DCOS( EULANG(ALPHA) )
      SA       = DSIN( EULANG(ALPHA) )
 
      IF ( AXISA .EQ. AXISC ) THEN
         U  =       DCOS ( EULANG(BETA) )
         V  =   D * DSIN ( EULANG(BETA) )
      ELSE
         U  =  -D * DSIN( EULANG(BETA) )
         V  =       DCOS( EULANG(BETA) )
      END IF
 
C
C     To avoid floating point overflows we make sure that we
C     can perform a division by V.  We do this by looking at U.
C     If it has absolute value 1, then we set V equal to zero.
C     After all U*U + V*V = 1 if SIN and COS and various arithmetic
C     operations work perfectly.
C
      IF ( DABS(U) .EQ. 1.0D0 ) THEN
         V = 0.0D0
      END IF
 
C
C     We have to look at the singular case first. Recall from above that
C
C        [ W(A) ]   [ -1     0     -U   ][dALPHA/dt]
C        | W(B) | = |  0   -CA  -D*SA*V ||dBETA /dt|
C        [ W(C) ]   [  0  D*SA    -CA*V ][dGAMMA/dt]
C
C     The singularity arises if V = 0.  In this case the equation
C     becomes:  ( Note that U  is plus or minus 1 so that division
C     by U is the same as multiplication by U. )
C
C        [ OMEGA(1) ]   [ -1     0  -U  ][dALPHA/dt]
C        | OMEGA(2) | = |  0   -CA   0  ||dBETA /dt|
C        [ OMEGA(3) ]   [  0  D*SA   0  ][dGAMMA/dt]
C
      IF ( V .EQ. 0 ) THEN
 
         UNIQUE         = .FALSE.
         EULANG(DALPHA) =  0.0D0
         EULANG(DGAMMA) = -U*OMEGA(1)
 
C
C        We solve for EULANG(DBETA) by selecting the more stable of
C        the two available equations.
C
         IF ( DABS(CA) .GT. DABS(SA) ) THEN
            EULANG(DBETA) =   -OMEGA(2)/CA
         ELSE
            EULANG(DBETA) =  D*OMEGA(3)/SA
         END IF
 
 
         CALL CHKOUT ( 'XF2EUL' )
         RETURN
 
      END IF
 
C
C     The matrix needed to compute the derivatives uniquely
C     exists.  Construct it and carry out the multiplication.
C
C     [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ OMEGA(1) ]
C     |dBETA /dt| = |  0   -CA      D*SA   | [ OMEGA(2) |
C     [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ OMEGA(3) ]
C
 
      UNIQUE      = .TRUE.
 
      SOLUTN(1,1) =  -1.0D0
      SOLUTN(2,1) =   0.0D0
      SOLUTN(3,1) =   0.0D0
 
      SOLUTN(1,2) =   U*D*SA/V
      SOLUTN(2,2) =      -CA
      SOLUTN(3,2) =    -D*SA/V
 
      SOLUTN(1,3) =     U*CA/V
      SOLUTN(2,3) =     D*SA
      SOLUTN(3,3) =      -CA/V
 
      CALL MXV ( SOLUTN, OMEGA, EULANG(4) )
 
      CALL CHKOUT ( 'XF2EUL' )
      RETURN
 
 
C$Procedure      EUL2XF ( Euler angles and derivative to transformation)
 
      ENTRY      EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM )
 
C$ Abstract
C
C     This routine computes a state transformation from an Euler angle
C     factorization of a rotation and the derivatives of those Euler
C     angles.
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
C     ANGLES
C     STATE
C     DERIVATIVES
C
C$ Declarations
C
C     DOUBLE PRECISION      EULANG ( 6 )
C     INTEGER               AXISA
C     INTEGER               AXISB
C     INTEGER               AXISC
C     DOUBLE PRECISION      XFORM  ( 6, 6 )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     EULANG     I   An array of Euler angles and their derivatives.
C     AXISA      I   Axis A of the Euler angle factorization.
C     AXISB      I   Axis B of the Euler angle factorization.
C     AXISC      I   Axis C of the Euler angle factorization.
C     XFORM      O   A state transformation matrix.
C
C$ Detailed_Input
C
C
C     EULANG      is the set of Euler angles corresponding to the
C                 specified factorization.
C
C                 If we represent R as shown here:
C
C                     R =  [ ALPHA ]     [ BETA ]     [ GAMMA ]
C                                   AXISA        AXISB         AXISC
C
C                 then
C
C
C                    EULANG(1) = ALPHA
C                    EULANG(2) = BETA
C                    EULANG(3) = GAMMA
C                    EULANG(4) = dALPHA/dt
C                    EULANG(5) = dBETA/dt
C                    EULANG(6) = dGAMMA/dt
C
C
C     AXISA       are the axes desired for the factorization of R.
C     AXISB       All must be in the range from 1 to 3.  Moreover
C     AXISC       it must be the case that AXISA and AXISB are distinct
C                 and that AXISB and AXISC are distinct.
C
C                 Every rotation matrix can be represented as a product
C                 of three rotation matrices about the principal axes
C                 of a reference frame.
C
C                     R =  [ ALPHA ]     [ BETA ]     [ GAMMA ]
C                                   AXISA        AXISB         AXISC
C
C                 The value 1 corresponds to the X axis.
C                 The value 2 corresponds to the Y axis.
C                 The value 3 corresponds to the Z axis.
C
C$ Detailed_Output
C
C     XFORM       is the state transformation corresponding R and dR/dt
C                 as described above.  Pictorially,
C
C                      [       |        ]
C                      |  R    |    0   |
C                      |       |        |
C                      |-------+--------|
C                      |       |        |
C                      | dR/dt |    R   |
C                      [       |        ]
C
C                 where R is a rotation that varies with respect to time
C                 and dR/dt is its time derivative.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     All erroneous inputs are diagnosed by routines in the call
C     tree to this routine.  These include
C
C     1)   If any of AXISA, AXISB, or AXISC do not have values in
C
C             { 1, 2, 3 },
C
C          then the error SPICE(INPUTOUTOFRANGE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is intended to provide an inverse for the
C     entry point XF2EUL.  See that entry point for a discussion
C     of notation.
C
C$ Examples
C
C     Suppose you have a set of Euler angles and their derivatives
C     for a 3 1 3 rotation, and that you would like to determine
C     the equivalent angles and derivatives for a 1 2 3 rotation.
C
C         R = [ALPHA]  [BETA]  [GAMMA]
C                    3       1        3
C
C         R = [ROLL]  [PITCH]  [YAW]
C                   1        2      3
C
C     The following pair of subroutine calls will perform the
C     desired computation.
C
C        ABGANG(1) = ALPHA
C        ABGANG(2) = BETA
C        ABGANG(3) = GAMMA
C        ABGANG(4) = DALPHA
C        ABGANG(5) = DBETA
C        ABGANG(6) = DGAMMA
C
C        CALL EUL2XF ( ABGANG, 3, 1, 3, XFORM  )
C        CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
C
C        ROLL     = RPYANG(1)
C        PITCH    = RPYANG(2)
C        YAW      = RPYANG(3)
C        DROLL    = RPYANG(4)
C        DPITCH   = RPYANG(5)
C        DYAW     = RPYANG(6)
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 25-APR-2007 (EDW)
C
C      Corrected code in Examples section, example showed
C      a XF2EUL call:
C      
C            CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG )
C       
C      The proper form of the call:
C      
C            CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
C
C-    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB)
C
C        Restriction that second axis must differ from both the first
C        and third axes was removed.
C
C-    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     State transformation from Euler angles and derivatives
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'EUL2XF' )

C
C     We're going to work with a local copy LOCANG of the euler angle
C     state vector EULANG.  We'll also use a local set of axis
C     numbers.
C
      CALL MOVED ( EULANG, 6, LOCANG )

      LOCAXA = AXISA
      LOCAXB = AXISB
      LOCAXC = AXISC

C
C     Parts of the following algorithm depend on the central axis
C     being different from the first and third axes.  We'll adjust
C     the axes and angles to make this so, if necessary.
C
      IF (  ( AXISB .EQ. AXISA ) .OR. ( AXISB .EQ. AXISC )  ) THEN

         IF ( AXISB .EQ. AXISA ) THEN
C
C           The first angle will "absorb" the second, and the
C           second will be set to zero.  All we do here is select
C           the first angle.
C           
            I = 1
         ELSE
            I = 3
         END IF
C
C        Absorb the second angle into the selected angle and set the 
C        second angle to zero.  The same goes for the angular rates.
C
         LOCANG(I)   = LOCANG(I)   + LOCANG(2)
         LOCANG(2)   = 0.D0

         LOCANG(I+3) = LOCANG(I+3) + LOCANG(5)
         LOCANG(5)   = 0.D0

C
C        Pick a second axis that doesn't match the others.  Since
C        the rotation angle about the second axis is zero, all that
C        matters here is picking a distinct axis.
C
         IF ( AXISC .EQ. NEXT(AXISA) ) THEN
C
C           The first axis is the predecessor of the third, so we pick
C           the successor of the third.
C  
            LOCAXB = NEXT( AXISC )

         ELSE
C
C           Either the third axis is the predecessor of the first or
C           matches the first, so the successor of the first is our
C           choice.
C
            LOCAXB = NEXT( AXISA )

         END IF

      END IF 

C
C     The following local variables are set:
C
C        LOCANG(*), LOCAXA, LOCAXB, LOCAXC
C
C     These variables describe the input rotation, but the second
C     axis is now guaranteed to differ from the first and third.
C
C     The derivation for everything that is about to happen here
C     is included in the previous entry point.
C
      CALL EUL2M ( LOCANG(ALPHA), LOCANG(BETA), LOCANG(GAMMA),
     .             LOCAXA,        LOCAXB,       LOCAXC,         R )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EUL2XF' )
         RETURN
      END IF
 
C
C     Construct local copies of the axes, determine L and D from the
C     derivation above.
C
      A = LOCAXA
      B = LOCAXB
      L = 6 - A - B
      D = DELTA(A,B)
 
C
C     Compute the various sines and cosines that we need.
C
      CA       = DCOS( LOCANG(ALPHA) )
      SA       = DSIN( LOCANG(ALPHA) )
 
      IF ( LOCAXA .EQ. LOCAXC ) THEN
         U  =       DCOS ( LOCANG(BETA) )
         V  =   D * DSIN ( LOCANG(BETA) )
      ELSE
         U  =  -D * DSIN( LOCANG(BETA) )
         V  =       DCOS( LOCANG(BETA) )
      END IF
 
C
C                            t
C     Next we compute dR/dt R.  Recall from the derivation above
C     that
C
C
C        [ W(A) ]   [ -1     0     -U   ][dALPHA/dt]
C        | W(B) | = |  0   -CA  -D*SA*V ||dBETA /dt|
C        [ W(L) ]   [  0  D*SA    -CA*V ][dGAMMA/dt]
C
C     In the previous entry point we used OMEGA for the vector
C     of rearranged components of W.
C
C        OMEGA(1) = W(A) = D*DRDTRT(L,B)
C        OMEGA(2) = W(B) = D*DRDTRT(A,L)
C        OMEGA(3) = W(L) = D*DRDTRT(B,A)
C
C        DRDTRT(L,B) = D*OMEGA(1)
C        DRDTRT(A,L) = D*OMEGA(2)
C        DRDTRT(B,A) = D*OMEGA(3)
C
C        [ DRDTRT(L,B) ]   [ -D     0     -D*U ][dALPHA/dt]
C        | DRDTRT(A,L) | = |  0 -D*CA    -SA*V ||dBETA /dt|
C        [ DRDTRT(B,A) ]   [  0    SA  -D*CA*V ][dGAMMA/dt]
C
C     We set up the matrix of this equation in SOLUTN below
C     and compute D*OMEGA which we denote by the variable DOMEGA.
C
      SOLUTN(1,1) =  -D
      SOLUTN(2,1) =   0.0D0
      SOLUTN(3,1) =   0.0D0
 
      SOLUTN(1,2) =   0.0D0
      SOLUTN(2,2) =  -D*CA
      SOLUTN(3,2) =     SA
 
      SOLUTN(1,3) =     -D*U
      SOLUTN(2,3) =    -SA*V
      SOLUTN(3,3) =  -D*CA*V
 
      CALL MXV ( SOLUTN, LOCANG(4), DOMEGA )
 
      DRDTRT(L,B) =  DOMEGA(1)
      DRDTRT(B,L) = -DOMEGA(1)
 
      DRDTRT(A,L) =  DOMEGA(2)
      DRDTRT(L,A) = -DOMEGA(2)
 
      DRDTRT(B,A) =  DOMEGA(3)
      DRDTRT(A,B) = -DOMEGA(3)
 
      DRDTRT(1,1) =  0.0D0
      DRDTRT(2,2) =  0.0D0
      DRDTRT(3,3) =  0.0D0
 
      CALL MXM ( DRDTRT, R, DRDT )
 
      DO J = 1,3
         DO I = 1,3
            XFORM(I,  J  ) = R   (I,J)
            XFORM(I+3,J+3) = R   (I,J)
            XFORM(I+3,J  ) = DRDT(I,J)
            XFORM(I,  J+3) = 0.0D0
         END DO
      END DO
 
      CALL CHKOUT ( 'EUL2XF' )
      RETURN
 
      END
 
 
 
 
 
