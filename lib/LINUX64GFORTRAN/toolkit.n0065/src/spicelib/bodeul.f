C$Procedure      BODEUL ( Return Euler angles for a body )
 
      SUBROUTINE BODEUL ( BODY, ET, RA, DEC, W, LAMBDA )
 
C$ Abstract
C
C     Return the Euler angles needed to compute the transformation from
C     inertial to body-fixed coordinates for any body in the kernel
C     pool.
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
C     PCK
C     NAIF_IDS
C     TIME
C
C$ Keywords
C
C     CONSTANTS
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations
 
      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      W
      DOUBLE PRECISION      LAMBDA
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   ID code of body.
C     ET         I   Epoch of transformation.
C     RA         O   Right ascension of the (IAU) north pole.
C     DEC        O   Declination of the (IAU) north pole of the body.
C     W          O   Prime meridian rotation angle.
C     LAMBDA     O   Angle between the prime meridian and longitude of
C                    longest axis.
C
C$ Detailed_Input
C
C     BODY        is the integer ID code of the body for which the
C                 transformation is requested. Bodies are numbered
C                 according to the standard NAIF numbering scheme.
C
C     ET          is the epoch at which the transformation is
C                 requested.
C
C$ Detailed_Output
C
C     RA,
C     DEC         are the right ascension and declination of the
C                 (IAU) north pole of the body at the epoch of
C                 transformation. RA and DEC are given in radians.
C
C     W           is the angle between the ascending node of the
C                 body-fixed equatorial plane on the inertial
C                 equatorial plane and the prime meridian of the body.
C                 The node is the cross product of the inertial
C                 frame's Z-axis with the Z-axis of the body-fixed
C                 frame. The angle is measured in the positive
C                 (counterclockwise) sense about the body-fixed
C                 Z-axis, from the node to the prime meridian. W is
C                 given in radians.
C
C     LAMBDA      is the angle between the prime meridian and the
C                 longest axis of the tri-axial ellipsoid which
C                 models the body. LAMBDA is given in radians.
C                 See the Particulars section below for further
C                 discussion.
C                  
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the number of phase terms is insufficient, the error
C        SPICE(KERNELVARNOTFOUND) is signaled.
C
C$ Files
C
C     1) A text or binary PCK containing orientation data for the
C        body designated by BODY must be loaded at the time this
C        routine is called.
C
C        Normally PCK files are loaded during program initialization;
C        they need not be re-loaded prior to each call to this routine.
C
C$ Particulars
C
C     Applications that need to compute the transformation between
C     body-fixed and inertial frames usually can call the higher-level
C     routine PXFORM instead of this routine.
C
C
C     If there exists high-precision binary PCK kernel information for
C     the body at the requested time, the angles, W, DELTA and PHI are
C     computed directly from that file.  These angles are then used to
C     compute RA, DEC and W.  The most recently loaded binary PCK file
C     has first priority followed by previously loaded binary PCK files
C     in backward time order.  If no binary PCK file has been loaded,
C     the text P_constants kernel file (PCK) is used.
C
C     If there is only text PCK kernel information, it is expressed in
C     terms of RA, DEC and W (same W as above), where
C
C        RA    = PHI - HALFPI()
C        DEC   = HALFPI() - DELTA
C
C     RA, DEC, and W are defined as follows in the text PCK file:
C
C        RA  = RA0  + RA1*T  + RA2*T*T   + a  sin theta
C                                           i          i
C
C        DEC = DEC0 + DEC1*T + DEC2*T*T  + d  cos theta
C                                           i          i
C
C        W   = W0   + W1*d   + W2*d*d    + w  sin theta
C                                           i          i
C
C     where:
C
C        d = days past J2000.
C
C        T = Julian centuries past J2000.
C
C        a , d , and w  arrays apply to satellites only.
C         i   i       i
C
C        theta  = THETA0 * THETA1*T are specific to each planet.
C             i
C
C       These angles -- typically nodal rates -- vary in number and
C       definition from one planetary system to the next.
C
C
C     The prime meridian offset LAMBDA
C     ================================
C     
C     The offset LAMBDA is the value specified by the kernel variable
C
C        BODYnnn_LONG_AXIS
C       
C     if such a variable is defined.
C
C     The offset LAMBDA is a constant for a given body. LAMBDA serves
C     to distinguish between the planetocentric prime meridian, which
C     is provided in the PCK file, and the meridian that passes through
C     the +X axis of a reference frame aligned with the axes of the
C     body's reference ellipsoid.
C
C     However, SPICE Toolkit makes no use of LAMBDA. In order to
C     perform geometry computations using a reference ellipsoid not
C     aligned with a body's planetocentric reference frame, a
C     fixed-offset (aka "TK") reference frame aligned with the
C     ellipsoid's axes should be specified in a frames kernel. Note
C     that a fixed-offset frame may be rotated from the planetocentric
C     frame about an arbitrary axis, not just the polar axis.
C
C     See the Frames Required Reading frames.req for details on
C     constructing a fixed-offset frame specification.
C
C$ Examples
C
C      In the following code fragment, BODEUL is used to get the unit
C      vector (POLE) parallel to the north pole of a target body (BODY)
C      at a specific epoch (ET).
C
C         CALL BODEUL ( BODY, ET, RA, DEC, W, LAMBDA )
C         CALL RADREC ( 1.D0, RA,  DEC, POLE )
C
C      Note that the items necessary to compute the Euler angles
C      must have been loaded into the kernel pool (by one or more
C      previous calls to LDPOOL).
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      1)  Refer to the NAIF_IDS required reading file for a complete
C          list of the NAIF integer ID codes for bodies.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C      K.S. Zukor      (JPL)
C
C$ Version
C
C-     SPICELIB Version 4.1.1, 02-JUL-2014 (NJB)
C
C         Discussion of LAMBDA was updated. Other minor header
C         updates were made.
C
C      Last update was 24-APR-2014 (NJB)
C
C         Corrected the brief and detailed descriptions of W.
C
C-     SPICELIB Version 4.1.0, 24-OCT-2005 (NJB)
C
C         Calls to ZZBODVCD have been replaced with calls to 
C         BODVCD.
C
C-     SPICELIB Version 4.0.0, 13-FEB-2004 (NJB)
C
C         Code has been updated to support satellite ID codes in the
C         range 10000 to 99999 and to allow nutation precession angles
C         to be associated with any object.
C
C         Implementation changes were made to improve robustness
C         of the code.
C
C-     SPICELIB Version 3.1.0, 21-MAR-1995 (KSZ)
C
C         REF frame is now passed correctly as a character string.
C
C-     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
C
C        Ability to get Euler angles from binary PCK file added.
C        This uses the new routine PCKEUL.
C
C-     SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
C
C         Updated to handle P_constants referenced to different epochs
C         and inertial reference frames.
C
C-     SPICELIB Version 1.1.0, 02-NOV-1990  (NJB)
C
C         Allowed number of nutation precession angles increased to
C         100.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990  (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     euler angles for orientation of a body
C     fetch euler angles for a body
C
C-&
 
 
 
C$ Revisions
C
C-     SPICELIB Version 4.1.0, 24-OCT-2005 (NJB)
C
C         Calls to ZZBODVCD have been replaced with calls to 
C         BODVCD.
C
C-     SPICELIB Version 4.0.0, 13-FEB-2004 (NJB)
C
C         Code has been updated to support satellite ID codes in the
C         range 10000 to 99999 and to allow nutation precession angles
C         to be associated with any object.
C
C         Calls to deprecated kernel pool access routine RTPOOL 
C         were replaced by calls to GDPOOL.
C
C         Calls to BODVAR have been replaced with calls to 
C         ZZBODVCD.
C
C-     SPICELIB Version 3.1.0, 21-MAR-1995 (KSZ)
C
C         REF frame is now passed correctly as a character string.
C
C-     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
C
C        BODEUL now uses new software to check for the
C        existence of binary PCK files, search the for
C        data corresponding to the requested body and time,
C        and return the appropriate Euler angles, using the
C        new routine PCKEUL.  Otherwise the code calculates
C        the Euler angles from the P_constants kernel file.
C
C-     SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
C
C         Updated to handle P_constants referenced to different epochs
C         and inertial reference frames.
C
C         Updated to handle P_constants referenced to different epochs
C         and inertial reference frames.
C
C         BODEUL now checks the kernel pool for presence of the
C         variables
C
C            BODY#_CONSTANTS_REF_FRAME
C
C         and
C
C            BODY#_CONSTANTS_JED_EPOCH
C
C         where # is the NAIF integer code of the barycenter of a
C         planetary system or of a body other than a planet or
C         satellite.  If either or both of these variables are
C         present, the P_constants for BODY are presumed to be
C         referenced to the specified inertial frame or epoch.
C         If the epoch of the constants is not J2000, the input
C         time ET is converted to seconds past the reference epoch.
C         If the frame of the constants is not J2000, the Euler angles
C         defining the rotation from the P_constants' frame to
C         body-fixed coordinates are transformed so that they define
C         the rotation from J2000 coordinates to body-fixed
C         coordinates.
C
C
C-     SPICELIB Version 1.1.0, 02-NOV-1990  (NJB)
C
C         Allowed number of nutation precession angles increased to
C         100.
C
C-    Beta Version 2.0.0, 23-JUN-1989 (HAN)
C
C        Mod angles by two pi. Check to see that right ascension and
C        prime meridian angles are within the range 0 to two pi.
C
C        LAMBDA used to be returned in degrees. It has been corrected
C        to return LAMBDA in radians.
C
C-    Beta Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
C
C        Examples section completed.  Declarations of unused variables
C        HALFPI and N removed.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               BODFND
      LOGICAL               RETURN
 
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOTG

      INTEGER               ZZBODBRY
 
C
C     Local parameters
C
      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 100 )
 
      INTEGER               MXPOLY
      PARAMETER           ( MXPOLY = 3 )
C
C     Local variables
C
      CHARACTER*(32)        ITEM
      CHARACTER*(32)        BREF
 
      INTEGER               DIM
      INTEGER               I
      INTEGER               J2CODE
      INTEGER               NA
      INTEGER               ND
      INTEGER               NL
      INTEGER               NTHETA
      INTEGER               NW
      INTEGER               REF
      INTEGER               REFID
 
      DOUBLE PRECISION      AC     ( MAXANG )
      DOUBLE PRECISION      CONEPC
      DOUBLE PRECISION      CONREF
      DOUBLE PRECISION      COSTH  ( MAXANG )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DC     ( MAXANG   )
      DOUBLE PRECISION      DCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      EULANG (   6    )
      DOUBLE PRECISION      J2BFX  ( 3, 3   )
      DOUBLE PRECISION      J2REF  ( 3, 3   )
      DOUBLE PRECISION      PHI
      DOUBLE PRECISION      RCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      RF2BFX ( 3, 3     )
      DOUBLE PRECISION      SINTH  ( MAXANG )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TCOEF  ( 2, MAXANG )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      WC     ( MAXANG )
      DOUBLE PRECISION      WCOEF  ( 0:MXPOLY-1 )
 
      LOGICAL               FIRST
      LOGICAL               FOUND
 
C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  J2CODE
 
C
C     Initial values
C
      DATA  FIRST / .TRUE. /
      DATA  FOUND / .FALSE. /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BODEUL' )
      END IF
 
C
C     Get the code for the J2000 frame, if we don't have it yet.
C
      IF ( FIRST ) THEN
 
         CALL IRFNUM ( 'J2000', J2CODE )
         FIRST = .FALSE.
 
      END IF
 
C
C     Get Euler angles from high precision data file.
C
      CALL PCKEUL ( BODY, ET, FOUND, BREF, EULANG )
 
      IF (FOUND) THEN
 
         PHI    = EULANG(1)
         DELTA  = EULANG(2)
         W      = EULANG(3)
 
         CALL IRFNUM ( BREF, REF )
C
C        The offset of the prime meridian is optional.
C
         ITEM = 'LONG_AXIS'

         IF ( BODFND ( BODY, ITEM ) ) THEN

            CALL BODVCD ( BODY, ITEM, 1, NL, LAMBDA )
            LAMBDA = LAMBDA * RPD()
            LAMBDA = MOD ( LAMBDA, TWOPI() )

         ELSE
            LAMBDA = 0
         END IF
 

      ELSE
C
C        Find the body code used to label the reference frame and epoch
C        specifiers for the orientation constants for BODY.
C
C        For planetary systems, the reference frame and epoch for the
C        orientation constants is associated with the system
C        barycenter, not with individual bodies in the system.  For any
C        other bodies, (the Sun or asteroids, for example) the body's
C        own code is used as the label.
C
         REFID = ZZBODBRY ( BODY )
 
C
C        Look up the epoch of the constants.  The epoch is specified
C        as a Julian ephemeris date.  The epoch defaults to J2000.
C
         ITEM  =  'BODY#_CONSTANTS_JED_EPOCH'
  
         CALL REPMI  ( ITEM, '#', REFID, ITEM               )
         CALL GDPOOL ( ITEM,  1,  1,     DIM, CONEPC, FOUND )
 
         IF ( FOUND ) THEN
C
C           The reference epoch is returned as a JED.  Convert to
C           ephemeris seconds past J2000.  Then convert the input ET to
C           seconds past the reference epoch.
C
            CONEPC  =  SPD() * ( CONEPC - J2000() )
            EPOCH   =  ET    -   CONEPC
 
         ELSE
            EPOCH   =  ET
         END IF
 
C
C        Look up the reference frame of the constants.  The reference
C        frame is specified by a code recognized by CHGIRF.  The
C        default frame is J2000, symbolized by the code J2CODE.
C
         CALL IRFNUM ( 'J2000', J2CODE )
 
         ITEM  =  'BODY#_CONSTANTS_REF_FRAME'
 
         CALL REPMI  ( ITEM, '#', REFID, ITEM               )
         CALL GDPOOL ( ITEM,  1,  1,     DIM, CONREF, FOUND )
 
         IF ( FOUND ) THEN
            REF  =  NINT ( CONREF )
         ELSE
            REF  =  J2CODE
         END IF
 
C
C        Whatever the body, it has quadratic time polynomials for
C        the RA and Dec of the pole, and for the rotation of the
C        Prime Meridian.
C
         ITEM = 'POLE_RA'
         CALL CLEARD (             MXPOLY,     RCOEF )
         CALL BODVCD ( BODY, ITEM, MXPOLY, NA, RCOEF )
 
         ITEM = 'POLE_DEC'
         CALL CLEARD (             MXPOLY,     DCOEF )
         CALL BODVCD ( BODY, ITEM, MXPOLY, ND, DCOEF )
 
         ITEM = 'PM'
         CALL CLEARD (             MXPOLY,     WCOEF )
         CALL BODVCD ( BODY, ITEM, MXPOLY, NW, WCOEF )
 
C
C        The offset of the prime meridian is optional.
C
         ITEM = 'LONG_AXIS'

         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, 1, NL, LAMBDA )
         ELSE
            LAMBDA = 0
         END IF
 
C
C        There may be additional nutation and libration (THETA) terms.
C
         NTHETA = 0
         NA     = 0
         ND     = 0
         NW     = 0
  
         ITEM = 'NUT_PREC_ANGLES'

         IF ( BODFND ( REFID, ITEM ) ) THEN
            CALL BODVCD ( REFID, ITEM, MAXANG, NTHETA, TCOEF )
            NTHETA = NTHETA / 2
         END IF
 
         ITEM = 'NUT_PREC_RA'

         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, NA, AC )
         END IF

         ITEM = 'NUT_PREC_DEC'

         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, ND, DC )
         END IF

         ITEM = 'NUT_PREC_PM'

         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, NW, WC )
         END IF

 
         IF ( MAX ( NA, ND, NW ) .GT. NTHETA ) THEN
            CALL SETMSG ( 'BODEUL: Insufficient number of nutation/'//
     .                 'precession angles for body * at time #.' )
            CALL ERRINT ( '*', BODY )
            CALL ERRDP  ( '#', ET   )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )
            CALL CHKOUT ( 'BODEUL' )
            RETURN
         END IF
 
C
C        Evaluate the time polynomials at EPOCH.
C
         D   = EPOCH / SPD()
         T   = D     / 36525.D0
 
         RA  = RCOEF(0)  +  T * ( RCOEF(1) + T * RCOEF(2) )
         DEC = DCOEF(0)  +  T * ( DCOEF(1) + T * DCOEF(2) )
         W   = WCOEF(0)  +  D * ( WCOEF(1) + D * WCOEF(2) )
 
C
C        Add nutation and libration as appropriate.
C
         DO I = 1, NTHETA
 
            THETA    = ( TCOEF(1,I)  +  T * TCOEF(2,I) ) * RPD()
 
            SINTH(I) = SIN ( THETA )
            COSTH(I) = COS ( THETA )
 
         END DO
 
         RA  = RA  + VDOTG ( AC, SINTH, NA )
         DEC = DEC + VDOTG ( DC, COSTH, ND )
         W   = W   + VDOTG ( WC, SINTH, NW )
 
C
C        Convert from degrees to radians and mod by two pi.
C
         RA     = RA     * RPD()
         DEC    = DEC    * RPD()
         W      = W      * RPD()
         LAMBDA = LAMBDA * RPD()
 
         RA     = MOD ( RA,     TWOPI() )
         DEC    = MOD ( DEC,    TWOPI() )
         W      = MOD ( W,      TWOPI() )
         LAMBDA = MOD ( LAMBDA, TWOPI() )
 
C
C        Convert to Euler angles.
C
         PHI     = RA + HALFPI()
         DELTA   = HALFPI() - DEC
 
      END IF
 
C
C        Convert the angles to the J2000 frame if they are not already
C        referenced to J2000.
C
      IF ( REF .NE. J2CODE ) THEN
C
C        Find the transformation from the J2000 frame to the frame
C        designated by REF.  Form the transformation from `REF'
C        coordinates to body-fixed coordinates, using our Euler angles.
C        Compose the transformations to obtain the J2000-to-body-fixed
C        transformation.  Decompose this transformation into Euler
C        angles.
C
         CALL IRFROT ( J2CODE, REF, J2REF )
 
         CALL EUL2M  ( W,
     .                 DELTA,
     .                 PHI,
     .                 3, 1, 3,
     .                 RF2BFX               )
 
         CALL MXM    ( RF2BFX, J2REF, J2BFX )
 
         CALL M2EUL  ( J2BFX,  3,  1,  3,  W,  DELTA,  PHI )
 
      END IF
 
C
C     The Euler angles now give the transformation from J2000 to
C     body-fixed coordinates at epoch ET seconds past J2000,
C     regardless of the epoch and frame of the orientation constants
C     for the specified body.
C
      RA   =  PHI       -  HALFPI()
      DEC  =  HALFPI()  -  DELTA
 
C
C     Make sure that the prime meridian and right ascension are in
C     the correct range.
C
      IF ( W .LT. 0 ) THEN
         W = W + TWOPI()
      END IF
 
      IF ( RA .LT. 0 ) THEN
         RA = RA + TWOPI()
      END IF
 
 
      CALL CHKOUT ( 'BODEUL' )
      RETURN
      END
 
 
 
 
 
 
