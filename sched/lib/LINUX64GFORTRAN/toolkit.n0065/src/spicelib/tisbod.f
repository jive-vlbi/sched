C$Procedure      TISBOD ( Transformation, inertial state to bodyfixed )
 
      SUBROUTINE TISBOD ( REF, BODY, ET, TSIPM )
 
C$ Abstract
C
C      Return a 6x6 matrix that transforms states in inertial
C      coordinates to states in body-equator-and-prime-meridian
C      coordinates.
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
C      PCK
C      NAIF_IDS
C     ROTATION
C      TIME
C
C$ Keywords
C
C      TRANSFORMATION
C      ROTATION
C
C$ Declarations

      IMPLICIT NONE
 
      INCLUDE               'errhnd.inc'
      INCLUDE               'frmtyp.inc'

      CHARACTER*(*)         REF
      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TSIPM   ( 6,6 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      REF        I   ID of inertial reference frame to transform from
C      BODY       I   ID code of body
C      ET         I   Epoch of transformation
C      TSIPM      O   Transformation (state), inertial to prime meridian
C
C$ Detailed_Input
C
C      REF         is the NAIF name for an inertial reference frame.
C                  Acceptable names include:
C
C                    Name       Description
C                    --------   --------------------------------
C                    'J2000'    Earth mean equator, dynamical
C                               equinox of J2000
C
C                    'B1950'    Earth mean equator, dynamical
C                               equinox of B1950
C
C                    'FK4'      Fundamental Catalog (4)
C
C                    'DE-118'   JPL Developmental Ephemeris (118)
C
C                    'DE-96'    JPL Developmental Ephemeris ( 96)
C
C                    'DE-102'   JPL Developmental Ephemeris (102)
C
C                    'DE-108'   JPL Developmental Ephemeris (108)
C
C                    'DE-111'   JPL Developmental Ephemeris (111)
C
C                    'DE-114'   JPL Developmental Ephemeris (114)
C
C                    'DE-122'   JPL Developmental Ephemeris (122)
C
C                    'DE-125'   JPL Developmental Ephemeris (125)
C
C                    'DE-130'   JPL Developmental Ephemeris (130)
C
C                    'GALACTIC' Galactic System II
C
C                    'DE-200'   JPL Developmental Ephemeris (200)
C
C                    'DE-202'   JPL Developmental Ephemeris (202)
C
C                  (See the routine CHGIRF for a full list of names.)
C
C                  The output TIPM will give the transformation
C                  from this frame to the bodyfixed frame specified by
C                  BODY at the epoch specified by ET.
C
C      BODY        is the integer ID code of the body for which the
C                  state transformation matrix is requested. Bodies
C                  are numbered according to the standard NAIF
C                  numbering scheme.  The numbering scheme is
C                  explained in the NAIF_IDS required reading file.
C
C      ET          is the epoch at which the state transformation
C                  matrix is requested. (This is typically the
C                  epoch of observation minus the one-way light time
C                  from the observer to the body at the epoch of
C                  observation.)
C
C$ Detailed_Output
C
C      TSIPM       is a 6x6 transformation matrix.  It is used to
C                  transform states from inertial coordinates to body
C                  fixed (also called equator and prime meridian ---
C                  PM) coordinates.
C
C                  Given a state S in the inertial reference frame
C                  specified by REF, the corresponding bodyfixed state
C                  is given by the matrix vector product:
C
C                     TSIPM * S
C
C                  The X axis of the PM system is directed  to the
C                  intersection of the equator and prime meridian.
C                  The Z axis points along  the spin axis and points
C                  towards the same side of the invariable plane of
C                  the solar system as does earth's north pole.
C
C                  NOTE: The inverse of TSIPM is NOT its transpose.
C                        The matrix, TSIPM, has a structure as shown
C                        below:
C
C                             -            -
C                            |       :      |
C                            |   R   :  0   |
C                            | ......:......|
C                            |       :      |
C                            | dR_dt :  R   |
C                            |       :      |
C                             -            -
C
C                        where R is a time varying rotation matrix and
C                        dR_dt is its derivative.  The inverse of this
C                        matrix is:
C
C                             -              -
C                            |     T  :       |
C                            |    R   :  0    |
C                            | .......:.......|
C                            |        :       |
C                            |      T :   T   |
C                            | dR_dt  :  R    |
C                            |        :       |
C                             -              -
C
C                        The SPICE routine INVSTM is available for
C                        producing this inverse.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      1) If data required to define the body-fixed frame associated
C         with BODY are not found in the binary PCK system or the kernel
C         pool, the error SPICE(FRAMEDATANOTFOUND) is signaled. In
C         the case of IAU style body-fixed frames, the absence of
C         prime meridian polynomial data (which are required) is used 
C         as an indicator of missing data.
C
C      2) If the test for exception (1) passes, but in fact requested
C         data are not available in the kernel pool, the error will be
C         signaled by routines in the call tree of this routine.
C
C      3) If the kernel pool does not contain all of the data required
C         to define the number of nutation precession angles
C         corresponding to the available nutation precession
C         coefficients, the error SPICE(INSUFFICIENTANGLES) is
C         signaled.
C
C      4) If the reference frame REF is not recognized, a routine
C         called by TISBOD will diagnose the condition and invoke the
C         SPICE error handling system.
C
C      5) If the specified body code BODY is not recognized, the
C         error is diagnosed by a routine called by TISBOD.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      The matrix for transforming inertial states to bodyfixed
C      states is the 6x6 matrix shown below as a block structured
C      matrix.
C
C                 -            -
C                |       :      |
C                | TIPM  :  0   |
C                | ......:......|
C                |       :      |
C                | DTIPM : TIPM |
C                |       :      |
C                 -            -
C
C     This can also be expressed in terms of Euler angles
C     PHI, DELTA and W.  The transformation from inertial to
C     bodyfixed coordinates is represented in the SPICE kernel
C     pool as:
C
C            TIPM = [W] [DELTA] [PHI]
C                      3       1     3
C      Thus
C
C           DTIPM = d[W] /dt [DELTA] [PHI]
C                       3           1     3
C
C                 + [W] d[DELTA] /dt [PHI]
C                      3             1     3
C
C                 + [W] [DELTA] d[PHI] /dt
C                      3       1          3
C
C
C      If a binary PCK file record can be used for the time and
C      body requested, it will be used.  The most recently loaded
C      binary PCK file has first priority, followed by previously
C      loaded binary PCK files in backward time order.  If no
C      binary PCK file has been loaded, the text P_constants
C      kernel file is used.
C
C      If there is only text PCK kernel information, it is
C      expressed in terms of RA, DEC and W (same W as above), where
C
C        RA    = PHI - HALFPI()
C        DEC   = HALFPI() - DELTA
C
C      The angles RA, DEC, and W are defined as follows in the
C      text PCK file:
C
C                                         2      ____
C                                    RA2*t       \
C            RA  = RA0  + RA1*t/T  + ------   +  /     a  sin theta
C                                       2        ----   i          i
C                                      T           i
C
C                                          2     ____
C                                    DEC2*t      \
C            DEC = DEC0 + DEC1*t/T + -------  +  /    d  cos theta
C                                        2       ----  i          i
C                                       T          i
C
C
C                                        2      ____
C                                    W2*t       \
C            W   = W0   + W1*t/d   + -----   +  /     w  sin theta
C                                       2       ----   i          i
C                                      d          i
C
C
C      where:
C
C            d = seconds/day
C
C            T = seconds/Julian century
C
C            a , d , and w  arrays apply to satellites only.
C             i   i       i
C
C            theta  = THETA0(i) + THETA1(i)*t/T are specific to each
C                 i
C
C            planet.
C
C
C        These angles -- typically nodal rates -- vary in number and
C        definition from one planetary system to the next.
C
C        Thus
C                                         ____
C                             2*RA2*t     \
C          dRA/dt  = RA1/T  + -------   + /   a THETA1(i)/T cos theta
C                                 2       ---- i                     i
C                                T          i
C
C                                         ____
C                             2*DEC2*t    \
C          dDEC/dt = DEC1/T + --------  - /    d  THETA1(i)/T sin theta
C                                 2       ----  i                      i
C                                T          i
C
C                                         ____
C                             2*W2*t      \
C          dW/dt   = W1/d   + ------    + /    w  THETA1(i)/T cos theta
C                                 2       ----  i                      i
C                                d          i
C
C
C$ Examples
C
C      Note that the data needed to compute the output state transition
C      matrix must have been made available to your program by having
C      loaded an appropriate binary or text PCK file via FURNSH.
C
C      Example 1.
C
C      In the following code fragment, TISBOD is used to transform
C      a state in J2000 inertial coordinates to a state in bodyfixed
C      coordinates.
C
C      The 6-vectors EULANG represents the inertial state (position and
C      velocity) of an object with respect to the center of the body
C      at time ET.
C
C      C
C      C     First load the kernel pool.
C      C
C            CALL FURNSH ( 'PLANETARY_CONSTANTS.KER' )
C
C      C
C      C     Next get the transformation and its derivative.
C      C
C            CALL TISBOD ( 'J2000', BODY, ET, TSIPM )
C
C      C
C      C     Convert position to bodyfixed coordinates.
C      C
C            CALL MXVG    ( TSIPM, EULANG, 6, 6, BDSTAT )
C
C
C      Example 2.
C
C      In the example below, TISBOD is used to compute
C      the angular velocity vector (with respect to an inertial frame)
C      of the specified body at time ET.
C
C      C
C      C     First get the state transformation matrix.
C      C
C            CALL TISBOD ( BODY,  ET,   TSIPM )
C
C      C
C      C     This matrix has the form:
C      C
C      C           -            -
C      C          |       :      |
C      C          | TIPM  :  0   |
C      C          | ......:......|
C      C          |       :      |
C      C          | DTIPM : TIPM |
C      C          |       :      |
C      C           -            -
C      C
C      C     We extract TIPM and DTIPM
C      C
C
C            DO  I = 1,3
C               DO  J = 1,3
C
C                  TIPM  ( I, J ) = TSIPM ( I,   J )
C                  DTIPM ( I, J ) = TSIPM ( I+3, J )
C
C               END DO
C            END DO
C
C      C
C      C     The transpose of TIPM and DTIPM, (TPMI and DTPMI), give
C      C     the transformation from bodyfixed coordinates to inertial
C      C     coordinates.
C      C
C      C     Here is a fact about the relationship between angular
C      C     velocity associated with a time varying rotation matrix
C      C     that gives the orientation of a body with respect to
C      C     an inertial frame.
C      C
C      C        The angular velocity vector can be read from the off
C      C        diagonal components of the matrix product:
C      C
C      C                                t
C      C        OMEGA =     DTPMI * TPMI
C      C
C      C                         t
C      C              =     DTIPM * TIPM
C      C
C      C        the components of the angular velocity V will appear
C      C        in this matrix as:
C      C
C      C             _                   _
C      C            |                     |
C      C            |   0    -V(3)  V(2)  |
C      C            |                     |
C      C            |  V(3)    0   -V(1)  |
C      C            |                     |
C      C            | -V(2)   V(1)   0    |
C      C            |_                   _|
C      C
C      C
C            CALL MTXM ( DTIPM, TIPM, OMEGA )
C
C            V(1) = OMEGA (3,2)
C            V(2) = OMEGA (1,3)
C            V(3) = OMEGA (2,1)
C
C$ Restrictions
C
C      The kernel pool must be loaded with the appropriate coefficients
C      (from the P_constants kernel or binary PCK file) prior to
C      calling this routine.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N. J. Bachman   (JPL)
C      W. L. Taber     (JPL)
C      K. S. Zukor     (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.4.0, 01-FEB-2008 (NJB)
C
C        The routine was updated to improve the error messages created
C        when required PCK data are not found. Now in most cases the
C        messages are created locally rather than by the kernel pool
C        access routines. In particular missing binary PCK data will
C        be indicated with a reasonable error message.
C
C-    SPICELIB Version 4.3.0, 13-DEC-2005 (NJB)
C
C        Bug fix:  previous update introduced bug in state 
C        transformation when REF was unequal to PCK native frame.
C
C-    SPICELIB Version 4.2.0, 23-OCT-2005 (NJB)
C
C        Re-wrote portions of algorithm to simplify source code.
C        Updated to remove non-standard use of duplicate arguments
C        in MXM and VADDG calls.
C
C        Replaced calls to ZZBODVCD with calls to BODVCD.
C
C-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
C
C        Tests of routine FAILED() were added.
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
C-     SPICELIB Version 3.3.0, 29-MAR-1995 (WLT)
C
C         Properly initialized the variable NPAIRS.
C
C-     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
C
C         Changed to call PCKMAT rather than PCKEUL.
C
C-     SPICELIB Version 3.1.0, 18-OCT-1994 (KSZ)
C
C         Fixed bug which incorrectly modded DW by two pi.
C
C-     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
C
C         Changed to look for binary PCK file, and used this
C         to find Euler angles, if such data has been loaded.
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
C         $Required_Reading and $Literature_References sections were
C         updated.
C
C-     SPICELIB Version 1.0.0, 05-NOV-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     transformation from inertial state to bodyfixed
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 4.2.0, 06-SEP-2005 (NJB)
C
C        Re-wrote portions of algorithm to simplify source code.
C        The routine now takes advantage of EUL2XF, which wasn't
C        available when the first version of this routine was written.
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM and VADDG calls.
C
C        Replaced calls to ZZBODVCD with calls to BODVCD.
C
C-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
C
C        Tests of routine FAILED() were added.  The new checks
C        are intended to prevent arithmetic operations from
C        being performed with uninitialized or invalid data.
C
C-     SPICELIB Version 4.0.0, 27-JAN-2004 (NJB)
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
C-     SPICELIB Version 3.3.0, 29-MAR-1995 (WLT)
C
C        The variable NPAIRS is now initialized
C        at the same point as NA, NTHETA, ND, and NW to be
C        zero.  This prevents the routine from performing
C        needless calculations for planets and avoids possible
C        floating point exceptions.
C
C-     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
C
C        TISBOD now gets the TSIPM matrix from PCKMAT.
C        Reference frame calculation moved to end.
C
C-     SPICELIB Version 3.0.1, 07-OCT-1994 (KSZ)
C
C        TISBOD bug which mistakenly moded DW by 2PI
C        was removed.
C
C-     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
C
C        TISBOD now uses new software to check for the
C        existence of binary PCK files, search the for
C        data corresponding to the requested body and time,
C        and return the appropriate Euler angles.  Otherwise
C        the code calculates the Euler angles from the
C        P_constants kernel file.
C
C-     SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
C
C         Updated to handle P_constants referenced to different epochs
C         and inertial reference frames.
C
C         TISBOD now checks the kernel pool for presence of the
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
C         If the frame of the constants is not the frame specified
C         by REF, the rotation from the P_constants' frame to
C         body-fixed coordinates is transformed to the rotation from
C         the requested frame to body-fixed coordinates.  The same
C         transformation is applied to the derivative of this
C         rotation.
C
C         Due to the prescience of the original author, the code
C         was already prepared to handle the possibility of
C         specification of a P_constants inertial reference frame via
C         kernel pool variables.
C
C
C         Also, the $Required_Reading and $Literature_References
C         sections were updated.  The SPK required reading has been
C         deleted from the $Literature_References section, and the
C         NAIF_IDS, KERNEL, and TIME Required Reading files have
C         been added in the $Required_Reading section.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VDOTG

      INTEGER               ZZBODBRY
 
      LOGICAL               BODFND
      LOGICAL               FAILED
      LOGICAL               RETURN 
 
C
C     Local parameters
C
      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 100 )

      INTEGER               MXPOLY
      PARAMETER           ( MXPOLY = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )
 
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

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
 
      DOUBLE PRECISION      AC     (    MAXANG )
      DOUBLE PRECISION      COSTH  (    MAXANG )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DC     (    MAXANG )
      DOUBLE PRECISION      DCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      DCOSTH (    MAXANG )
      DOUBLE PRECISION      DDEC
      DOUBLE PRECISION      DDELTA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DPHI
      DOUBLE PRECISION      DRA
      DOUBLE PRECISION      DSINTH (    MAXANG )
      DOUBLE PRECISION      DTIPM  (     3,  3 )
      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      DW
      DOUBLE PRECISION      EULSTA (     6     )
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      PCKEPC
      DOUBLE PRECISION      PCKREF
      DOUBLE PRECISION      PHI
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      REQ2PC (     3, 3  )
      DOUBLE PRECISION      SINTH  (    MAXANG )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TCOEF  ( 2, MAXANG )
      DOUBLE PRECISION      TIPM   (     3, 3  )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      W
      DOUBLE PRECISION      WC     (    MAXANG )
      DOUBLE PRECISION      WCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      XDTIPM (     3, 3  )
      DOUBLE PRECISION      XTIPM  (     3, 3  )
 
      INTEGER               CENT
      INTEGER               DIM
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               J
      INTEGER               J2CODE
      INTEGER               NA
      INTEGER               ND
      INTEGER               NPAIRS
      INTEGER               NTHETA
      INTEGER               NW
      INTEGER               PCREF
      INTEGER               REFID
      INTEGER               REQREF
 
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
      DATA  FIRST  / .TRUE. /
      DATA  FOUND  / .FALSE. /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TISBOD' )
      END IF
 
C
C     Get the code for the J2000 frame, if we don't have it yet.
C
      IF ( FIRST ) THEN
 
         CALL IRFNUM ( 'J2000', J2CODE )
         FIRST = .FALSE.
 
      END IF
 
      CALL IRFNUM  ( REF, REQREF )
 
C
C     Get state transformation matrix from high precision PCK file, if
C     available.
C
      CALL PCKMAT ( BODY, ET, PCREF, TSIPM, FOUND )
 
      IF ( .NOT. FOUND ) THEN
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
            CALL CHKOUT ( 'TISBOD'                   )
            RETURN

         END IF

 
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
         CALL GDPOOL ( ITEM,  1,  1,     DIM, PCKEPC, FOUND )
 
 
         IF ( FOUND ) THEN
C
C           The reference epoch is returned as a JED.  Convert to
C           ephemeris seconds past J2000.  Then convert the input
C           ET to seconds past the reference epoch.
C
            PCKEPC  =  SPD() * ( PCKEPC - J2000() )
            EPOCH   =  ET    -   PCKEPC
 
         ELSE
            EPOCH   =  ET
         END IF
 
C
C        Look up the reference frame of the constants.  The reference
C        frame is specified by a code recognized by CHGIRF.  The default
C        frame is J2000, symbolized by the code J2CODE.
C
         ITEM  =  'BODY#_CONSTANTS_REF_FRAME'
 
         CALL REPMI  ( ITEM, '#', REFID, ITEM               )
         CALL GDPOOL ( ITEM,  1,  1,     DIM, PCKREF, FOUND )
 
         IF ( FOUND ) THEN
            PCREF  =  NINT ( PCKREF )
         ELSE
            PCREF  =  J2CODE
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
C        If the body is a satellite, there may be additional nutation
C        and libration (THETA) terms.
C
         NTHETA = 0
         NPAIRS = 0
         NA     = 0
         ND     = 0
         NW     = 0
 
 
         ITEM = 'NUT_PREC_ANGLES'
 
C
C        There is something a bit obscure going on below.  We are
C        passing a two dimensional array ( TCOEF(2, MAXANG) ). But
C        BODVCD is expecting a 1- dimensional array. BODVCD loads the
C        array TCOEF in the following order
C
C           TCOEF(1,1), TCOEF(2,1), TCOEF(1,2), TCOEF(2,2),   ...
C
C        The NTHETA that comes back is the total number of items
C        loaded, but we will need the actual limit on the second
C        dimension. That is --- NTHETA / 2.
C
         IF ( BODFND ( REFID, ITEM ) ) THEN
            CALL BODVCD ( REFID, ITEM, MAXANG, NTHETA, TCOEF )
            NPAIRS = NTHETA / 2
         END IF

C
C        Look up the right ascension nutations in the precession of the
C        pole.  NA is the number of Ascension coefficients. AC are the
C        Ascension coefficients.
C
         ITEM = 'NUT_PREC_RA'
 
         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, NA, AC )
         END IF
 
C
C        Look up the declination nutations in the precession of the
C        pole.  ND is the number of Declination coefficients. DC are
C        the Declination coefficients.
C
         ITEM = 'NUT_PREC_DEC'
 
         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, ND, DC )
         END IF
 
C
C        Finally look up the prime meridian nutations.  NW is the
C        number of coefficients.  WC is the array of coefficients.
C
         ITEM = 'NUT_PREC_PM'
 
         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, NW, WC )
         END IF
 
C
C        The number of coefficients returned had better not be bigger
C        than the number of angles we are going to compute.  If it
C        is we simply signal an error and bag it, fer sure.
C
         IF ( MAX ( NA, ND, NW ) .GT. NPAIRS ) THEN
            CALL SETMSG ( 'TISBOD: Insufficient number of nutation/'  //
     .                    'precession angles for body * at time #.' )
            CALL ERRINT ( '*', BODY )
            CALL ERRDP  ( '#', ET   )
            CALL SIGERR ( 'SPICE(INSUFFICIENTANGLES)' )
            CALL CHKOUT ( 'TISBOD' )
            RETURN
         END IF
 
C
C        Evaluate the time polynomials and their derivatives w.r.t.
C        EPOCH at EPOCH.
C
C        Evaluate the time polynomials at EPOCH.
C
         D   = SPD()
         T   = D  *  36525.D0
 
         RA  = RCOEF(0)+ (EPOCH/T) * ( RCOEF(1) + (EPOCH/T) * RCOEF(2) )
         DEC = DCOEF(0)+ (EPOCH/T) * ( DCOEF(1) + (EPOCH/T) * DCOEF(2) )
         W   = WCOEF(0)+ (EPOCH/D) * ( WCOEF(1) + (EPOCH/D) * WCOEF(2) )
 
         DRA  = ( RCOEF(1)  + 2.0D0*(EPOCH/T)*RCOEF(2) ) / T
         DDEC = ( DCOEF(1)  + 2.0D0*(EPOCH/T)*DCOEF(2) ) / T
         DW   = ( WCOEF(1)  + 2.0D0*(EPOCH/D)*WCOEF(2) ) / D
 
 
C
C        Compute the nutations and librations (and their derivatives)
C        as appropriate.
C
         DO   I = 1, NPAIRS
 
            THETA  = ( TCOEF(1,I) + (EPOCH/T) * TCOEF(2,I) ) * RPD()
            DTHETA = ( TCOEF(2,I) / T ) * RPD()
 
            SINTH(I)  =  SIN ( THETA )
            COSTH(I)  =  COS ( THETA )
            DSINTH(I) =  COS ( THETA ) * DTHETA
            DCOSTH(I) = -SIN ( THETA ) * DTHETA
 
         END DO
 
C
C        Adjust RA, DEC, W and their derivatives by the librations
C        and nutations.
C
         RA  = RA    + VDOTG ( AC, SINTH,  NA )
         DEC = DEC   + VDOTG ( DC, COSTH,  ND )
         W   = W     + VDOTG ( WC, SINTH,  NW )
 
         DRA  = DRA  + VDOTG ( AC, DSINTH, NA )
         DDEC = DDEC + VDOTG ( DC, DCOSTH, ND )
         DW   = DW   + VDOTG ( WC, DSINTH, NW )
 
C
C        Convert from degrees to radians
C
         RA      = RA   * RPD()
         DEC     = DEC  * RPD()
         W       = W    * RPD()
 
         DRA     = DRA  * RPD()
         DDEC    = DDEC * RPD()
         DW      = DW   * RPD()
C
C        Convert to Euler angles.
C
         W       = MOD ( W,  TWOPI() )
 
         PHI     = RA + HALFPI()
         DELTA   = HALFPI() - DEC
 
         DPHI    =   DRA
         DDELTA  = - DDEC
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TISBOD' )
            RETURN
         END IF

C
C        Pack the Euler angles and their derivatives into
C        a state vector.
C
         CALL VPACK ( W,  DELTA,  PHI,  EULSTA    )
         CALL VPACK ( DW, DDELTA, DPHI, EULSTA(4) )

C
C        Find the state transformation defined by the Euler angle
C        state vector.  The transformation matrix TSIPM has the 
C        following structure:
C
C            -            -
C           |       :      |
C           | TIPM  :  0   |
C           | ......:......|
C           |       :      |
C           | DTIPM : TIPM |
C           |       :      |
C            -            -
C
         CALL EUL2XF ( EULSTA, 3, 1, 3, TSIPM )

      END IF


C
C     At this point the base frame PCREF has been determined.
C
C     If the requested base frame is not base frame associated with the
C     PCK data, adjust the transformation matrix TSIPM to map from the
C     requested frame to the body-fixed frame.
C
      IF ( REQREF .NE. PCREF ) THEN
C
C        Next get the position transformation from the user specified
C        inertial frame to the native PCK inertial frame. 
C
         CALL IRFROT ( REQREF, PCREF, REQ2PC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TISBOD' )
            RETURN
         END IF

C
C        Since we're applying an inertial transformation to TSIPM,
C        we can rotate the non-zero blocks of TSIPM.  This saves
C        a bunch of double precision multiplications.
C
C        Extract the upper and lower left blocks of TSIPM.
C        
         DO I = 1, 3

            DO J = 1, 3

               TIPM (I,J) =  TSIPM( I,   J )
               DTIPM(I,J) =  TSIPM( I+3, J )

            END DO

         END DO
         
C
C        Rotate the blocks.  Note this is a right multiplication.
C
         CALL MXM ( TIPM,  REQ2PC, XTIPM  )
         CALL MXM ( DTIPM, REQ2PC, XDTIPM )

C
C        Replace the non-zero blocks of TSIPM.  This gives us the
C        transformation from the requested frame to the
C        bodyfixed frame.  
C
         DO I = 1, 3

            DO J = 1, 3

               TSIPM( I,   J   ) = XTIPM (I,J)
               TSIPM( I+3, J+3 ) = XTIPM (I,J)
               TSIPM( I+3, J   ) = XDTIPM(I,J)

            END DO

         END DO
         
      END IF

C
C     That's all folks. Check out and get out.
C
      CALL CHKOUT ( 'TISBOD' )
      RETURN
      END
 
 
 
 
 
 
 
 
