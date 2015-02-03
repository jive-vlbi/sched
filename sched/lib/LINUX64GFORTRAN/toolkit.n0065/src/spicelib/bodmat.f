C$Procedure      BODMAT ( Return transformation matrix for a body )
 
      SUBROUTINE BODMAT ( BODY, ET, TIPM )
 
C$ Abstract
C
C     Return the J2000 to body Equator and Prime Meridian coordinate
C     transformation matrix for a specified body.
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
C
C$ Declarations

      IMPLICIT NONE
 
      INCLUDE               'errhnd.inc'
      INCLUDE               'frmtyp.inc'

      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TIPM   ( 3,3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   ID code of body.
C     ET         I   Epoch of transformation.
C     TIPM       O   Transformation from Inertial to PM for BODY at ET.
C
C$ Detailed_Input
C
C     BODY        is the integer ID code of the body for which the
C                 transformation is requested. Bodies are numbered
C                 according to the standard NAIF numbering scheme.
C
C     ET          is the epoch at which the transformation is
C                 requested. (This is typically the epoch of
C                 observation minus the one-way light time from
C                 the observer to the body at the epoch of
C                 observation.)
C
C$ Detailed_Output
C
C     TIPM        is the transformation matrix from Inertial to body
C                 Equator and Prime Meridian.  The X axis of the PM
C                 system is directed to the intersection of the
C                 equator and prime meridian. The Z axis points north.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If data required to define the body-fixed frame associated
C        with BODY are not found in the binary PCK system or the kernel
C        pool, the error SPICE(FRAMEDATANOTFOUND) is signaled. In
C        the case of IAU style body-fixed frames, the absence of
C        prime meridian polynomial data (which are required) is used
C        as an indicator of missing data.
C
C     2) If the test for exception (1) passes, but in fact requested
C        data are not available in the kernel pool, the error will be
C        signaled by routines in the call tree of this routine.
C
C     3) If the kernel pool does not contain all of the data required
C        to define the number of nutation precession angles
C        corresponding to the available nutation precession
C        coefficients, the error SPICE(INSUFFICIENTANGLES) is
C        signaled.
C
C     4) If the reference frame REF is not recognized, a routine
C        called by BODMAT will diagnose the condition and invoke the
C        SPICE error handling system.
C
C     5) If the specified body code BODY is not recognized, the
C        error is diagnosed by a routine called by BODMAT.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is related to the more general routine TIPBOD
C     which returns a matrix that transforms vectors from a
C     specified inertial reference frame to body equator and
C     prime meridian coordinates.  TIPBOD accepts an input argument
C     REF that allows the caller to specify an inertial reference
C     frame.
C
C     The transformation represented by BODMAT's output argument TIPM
C     is defined as follows:
C
C        TIPM = [W] [DELTA] [PHI]
C                 3        1     3
C
C     If there exists high-precision binary PCK kernel information
C     for the body at the requested time, these angles, W, DELTA
C     and PHI are computed directly from that file.  The most
C     recently loaded binary PCK file has first priority followed
C     by previously loaded binary PCK files in backward time order.
C     If no binary PCK file has been loaded, the text P_constants
C     kernel file is used.
C
C     If there is only text PCK kernel information, it is
C     expressed in terms of RA, DEC and W (same W as above), where
C
C        RA    = PHI - HALFPI()
C        DEC   = HALFPI() - DELTA
C
C     RA, DEC, and W are defined as follows in the text PCK file:
C
C           RA  = RA0  + RA1*T  + RA2*T*T   + a  sin theta
C                                              i          i
C
C           DEC = DEC0 + DEC1*T + DEC2*T*T  + d  cos theta
C                                              i          i
C
C           W   = W0   + W1*d   + W2*d*d    + w  sin theta
C                                              i          i
C
C     where:
C
C           d = days past J2000.
C
C           T = Julian centuries past J2000.
C
C           a , d , and w  arrays apply to satellites only.
C            i   i       i
C
C           theta  = THETA0 * THETA1*T are specific to each planet.
C                i
C
C     These angles -- typically nodal rates -- vary in number and
C     definition from one planetary system to the next.
C
C$ Examples
C
C     In the following code fragment, BODMAT is used to rotate
C     the position vector (POS) from a target body (BODY) to a
C     spacecraft from inertial coordinates to body-fixed coordinates
C     at a specific epoch (ET), in order to compute the planetocentric
C     longitude (PCLONG) of the spacecraft.
C
C        CALL BODMAT ( BODY, ET, TIPM )
C        CALL MXV    ( TIPM, POS, POS )
C        CALL RECLAT ( POS, RADIUS, PCLONG, LAT )
C
C     To compute the equivalent planetographic longitude (PGLONG),
C     it is necessary to know the direction of rotation of the target
C     body, as shown below.
C
C        CALL BODVCD ( BODY, 'PM', 3, DIM, VALUES )
C
C        IF ( VALUES(2) .GT. 0.D0 ) THEN
C           PGLONG = PCLONG
C        ELSE
C           PGLONG = TWOPI() - PCLONG
C        END IF
C
C     Note that the items necessary to compute the transformation
C     TIPM must have been loaded into the kernel pool (by one or more
C     previous calls to FURNSH).
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1)  Refer to the NAIF_IDS required reading file for a complete
C         list of the NAIF integer ID codes for bodies.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     K.S. Zukor      (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.1, 01-FEB-2008 (NJB)
C
C        The routine was updated to improve the error messages created
C        when required PCK data are not found. Now in most cases the
C        messages are created locally rather than by the kernel pool
C        access routines. In particular missing binary PCK data will
C        be indicated with a reasonable error message.
C
C-    SPICELIB Version 4.1.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM call.
C
C         Calls to ZZBODVCD have been replaced with calls to 
C         BODVCD.
C
C-     SPICELIB Version 4.0.0, 12-FEB-2004 (NJB)
C
C         Code has been updated to support satellite ID codes in the
C         range 10000 to 99999 and to allow nutation precession angles
C         to be associated with any object. 
C
C         Implementation changes were made to improve robustness
C         of the code.
C
C-     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
C
C        Gets TSIPM matrix from PCKMAT (instead of Euler angles
C        from PCKEUL.)
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
C         The header was updated to specify that the inertial reference
C         frame used by BODMAT is restricted to be J2000.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     fetch transformation matrix for a body
C     transformation from j2000 position to bodyfixed
C     transformation from j2000 to bodyfixed coordinates
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 4.1.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM call.
C
C         Calls to ZZBODVCD have been replaced with calls to 
C         BODVCD.
C
C-     SPICELIB Version 4.0.0, 12-FEB-2004 (NJB)
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
C-     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
C
C        BODMAT now get the TSIPM matrix from PCKMAT, and
C        unpacks TIPM from it.  Also the calculated but unused
C        variable LAMBDA was removed.
C
C-     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
C
C        BODMAT now uses new software to check for the
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
C         The header was updated to specify that the inertial reference
C         frame used by BODMAT is restricted to be J2000.
C
C         BODMAT now checks the kernel pool for presence of the
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
C         If the frame of the constants is not J2000, the rotation from
C         the P_constants' frame to body-fixed coordinates is
C         transformed to the rotation from J2000 coordinates to
C         body-fixed coordinates.
C
C         For efficiency reasons, this routine now duplicates much
C         of the code of BODEUL so that it doesn't have to call BODEUL.
C         In some cases, BODEUL must covert Euler angles to a matrix,
C         rotate the matrix, and convert the result back to Euler
C         angles.  If this routine called BODEUL, then in such cases
C         this routine would convert the transformed angles back to
C         a matrix.  That would be a bit much....
C
C
C-    Beta Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
C
C        Examples section completed.  Declaration of unused variable
C        FOUND removed.
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOTG
 
      INTEGER               ZZBODBRY

      LOGICAL               BODFND
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 100 )

      INTEGER               MXPOLY
      PARAMETER           ( MXPOLY = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )
 
      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )

C
C     Local variables
C
      CHARACTER*(1)         DTYPE
      CHARACTER*(LMSGLN)    ERRMSG
      CHARACTER*(FRNMLN)    FIXFRM
      CHARACTER*(NAMLEN)    ITEM
      CHARACTER*(TIMLEN)    TIMSTR
 
      INTEGER               CENT
      INTEGER               DIM
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               J
      INTEGER               J2CODE
      INTEGER               NA
      INTEGER               ND
      INTEGER               NTHETA
      INTEGER               NW
      INTEGER               REF
      INTEGER               REFID
 
      DOUBLE PRECISION      AC     ( MAXANG )
      DOUBLE PRECISION      CONEPC
      DOUBLE PRECISION      CONREF
      DOUBLE PRECISION      COSTH  ( MAXANG )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DC     ( MAXANG )
      DOUBLE PRECISION      DCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      J2REF  (  3,  3 )
      DOUBLE PRECISION      PHI
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      SINTH  ( MAXANG )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TCOEF  ( 2, MAXANG )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TMPMAT ( 3, 3   )
      DOUBLE PRECISION      TSIPM  ( 6, 6   )
      DOUBLE PRECISION      W
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
C     Standard SPICE Error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BODMAT' )
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
      CALL PCKMAT ( BODY, ET, REF, TSIPM, FOUND )
 
      IF ( FOUND ) THEN
 
         DO  I = 1, 3
            DO   J  = 1, 3
              TIPM  ( I, J ) = TSIPM  ( I, J )
            END DO
         END DO
 
      ELSE
C
C        The data for the frame of interest are not available in a
C        loaded binary PCK file. This is not an error: the data may be
C        present in the kernel pool.
C
C        Conduct a non-error-signaling check for the presence of a
C        kernel variable that is required to implement an IAU style
C        body-fixed reference frame. If the data aren't available, we
C        don't want BODVCD to signal a SPICE(KERNELVARNOTFOUND) error;
C        we want to issue the error signal locally, with a better error
C        message.
C
         ITEM = 'BODY#_PM'
         CALL REPMI  ( ITEM, '#',   BODY, ITEM  )
         CALL DTPOOL ( ITEM, FOUND, NW,   DTYPE )
         
         IF ( .NOT. FOUND ) THEN
C
C           Now we do have an error.
C
C           We don't have the data we'll need to produced the requested
C           state transformation matrix. In order to create an error
C           message understandable to the user, find, if possible, the
C           name of the reference frame associated with the input body.
C           Note that the body is really identified by a PCK frame class
C           ID code, though most of the documentation just calls it a 
C           body ID code.
C   
            CALL CCIFRM ( PCK, BODY,  FRCODE, FIXFRM, CENT, FOUND )
            CALL ETCAL  ( ET,  TIMSTR )

            ERRMSG = 'PCK data required to compute the orientation '
     .      //       'of the # # for epoch # TDB were not found. '
     .      //       'If these data were to be provided by a binary '
     .      //       'PCK file, then it is possible that the PCK '
     .      //       'file does not have coverage for the specified '
     .      //       'body-fixed frame at the time of interest. If the '
     .      //       'data were to be provided by a text PCK file, '
     .      //       'then possibly the file does not contain data '
     .      //       'for the specified body-fixed frame. In '
     .      //       'either case it is possible that a required '
     .      //       'PCK file was not loaded at all.'

C
C           Fill in the variable data in the error message.
C
            IF ( FOUND ) THEN
C
C              The frame system knows the name of the body-fixed frame.
C
               CALL SETMSG ( ERRMSG                  )
               CALL ERRCH  ( '#', 'body-fixed frame' )
               CALL ERRCH  ( '#', FIXFRM             )
               CALL ERRCH  ( '#', TIMSTR             )

            ELSE
C
C              The frame system doesn't know the name of the 
C              body-fixed frame, most likely due to a missing
C              frame kernel.
C
               CALL SUFFIX ( '#', 1, ERRMSG                      )
               CALL SETMSG ( ERRMSG                              )
               CALL ERRCH  ( '#', 
     .                       'body-fixed frame associated with ' 
     .         //            'the ID code'                       )
               CALL ERRINT ( '#', BODY                           )
               CALL ERRCH  ( '#', TIMSTR                         )
               CALL ERRCH  ( '#', 
     .                       'Also, a frame kernel defining the '
     .         //            'body-fixed frame associated with '
     .         //            'body # may need to be loaded.'     )
               CALL ERRINT ( '#', BODY                           )
     
            END IF

            CALL SIGERR ( 'SPICE(FRAMEDATANOTFOUND)' )
            CALL CHKOUT ( 'BODMAT'                   )
            RETURN

         END IF


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
            CALL SETMSG ( 'Insufficient number of nutation/' //
     .                 'precession angles for body * at time #.' )
            CALL ERRINT ( '*', BODY )
            CALL ERRDP  ( '#', ET   )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )
            CALL CHKOUT ( 'BODMAT' )
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
 
         RA     = MOD ( RA,     TWOPI() )
         DEC    = MOD ( DEC,    TWOPI() )
         W      = MOD ( W,      TWOPI() )
 
C
C        Convert to Euler angles.
C
         PHI     = RA + HALFPI()
         DELTA   = HALFPI() - DEC
 
C
C        Produce the rotation matrix defined by the Euler angles.
C
         CALL EUL2M  ( W, DELTA, PHI,
     .                 3,     1,   3, TIPM )
 
      END IF
C
C     Convert TIPM to the J2000-to-bodyfixed rotation, if is is not
C     already referenced to J2000.
C
      IF ( REF .NE. J2CODE ) THEN
C
C        Find the transformation from the J2000 frame to the frame
C        designated by REF.  Form the transformation from `REF'
C        coordinates to body-fixed coordinates.  Compose the
C        transformations to obtain the J2000-to-body-fixed
C        transformation.
C
         CALL IRFROT ( J2CODE,  REF,    J2REF  )
         CALL MXM    ( TIPM,    J2REF,  TMPMAT )
         CALL MOVED  ( TMPMAT,  9,      TIPM   )

      END IF
 
C
C     TIPM now gives the transformation from J2000 to
C     body-fixed coordinates at epoch ET seconds past J2000,
C     regardless of the epoch and frame of the orientation constants
C     for the specified body.
C
 
      CALL CHKOUT ( 'BODMAT' )
      RETURN
      END
 
 
 
 
 
