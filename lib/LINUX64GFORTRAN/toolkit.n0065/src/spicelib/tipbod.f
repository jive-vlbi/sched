C$Procedure  TIPBOD ( Transformation, inertial position to bodyfixed )
 
      SUBROUTINE TIPBOD ( REF, BODY, ET, TIPM )
 
C$ Abstract
C
C      Return a 3x3 matrix that transforms positions in inertial
C      coordinates to positions in body-equator-and-prime-meridian
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
 
      CHARACTER*(*)         REF
      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TIPM   ( 3, 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      REF        I   ID of inertial reference frame to transform from.
C      BODY       I   ID code of body.
C      ET         I   Epoch of transformation.
C      TIPM       O   Transformation (position), inertial to prime
C                     meridian.
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
C
C      BODY        is the integer ID code of the body for which the
C                  position transformation matrix is requested. Bodies
C                  are numbered according to the standard NAIF
C                  numbering scheme.  The numbering scheme is
C                  explained in the NAIF_IDS required reading file.
C
C      ET          is the epoch at which the position transformation
C                  matrix is requested. (This is typically the
C                  epoch of observation minus the one-way light time
C                  from the observer to the body at the epoch of
C                  observation.)
C
C$ Detailed_Output
C
C      TIPM        is a 3x3 coordinate transformation matrix.  It is
C                  used to transform positions from inertial
C                  coordinates to body fixed (also called equator and
C                  prime meridian --- PM) coordinates.
C
C                  Given a position P in the inertial reference frame
C                  specified by REF, the corresponding bodyfixed
C                  position is given by the matrix vector product:
C
C                     TIPM * S
C
C                  The X axis of the PM system is directed to the
C                  intersection of the equator and prime meridian.
C                  The Z axis points along  the spin axis and points
C                  towards the same side of the invariable plane of
C                  the solar system as does earth's north pole.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      1) If the kernel pool does not contain all of the data required
C         for computing the transformation matrix, TIPM, the error
C         SPICE(INSUFFICIENTANGLES) is signalled.
C
C      2) If the reference frame, REF,  is not recognized, a routine
C         called by TIPBOD will diagnose the condition and invoke the
C         SPICE error handling system.
C
C      3) If the specified body code, BODY, is not recognized, the
C         error is diagnosed by a routine called by TIPBOD.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     TIPBOD takes PCK information as input, either in the
C     form of a binary or text PCK file.  High precision
C     binary files are searched for first (the last loaded
C     file takes precedence); then it defaults to the text
C     PCK file.  If binary information is found for the
C     requested body and time, the Euler angles are
C     evaluated and the transformation matrix is calculated
C     from them.  Using the Euler angles PHI, DELTA and W
C     we compute
C
C            TIPM = [W] [DELTA] [PHI]
C                      3       1     3
C
C
C      If no appropriate binary PCK files have been loaded,
C      the text PCK file is used.  Here information is found
C      as RA, DEC and W (with the possible addition of nutation
C      and libration terms for satellites).  Again, the Euler
C      angles are found, and the transformation matrix is
C      calculated from them.  The transformation from inertial to
C      bodyfixed coordinates is represented as:
C
C            TIPM = [W] [HALFPI-DEC] [RA+HALFPI]
C                      3            1           3
C
C     These are basically the Euler angles, PHI, DELTA and W:
C
C       RA = PHI - HALFPI
C       DEC = HALFPI - DELTA
C       W = W
C
C      In the text file, RA, DEC, and W are defined as follows:
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
C$ Examples
C
C      Note that the items necessary to compute the Euler angles
C      must have been loaded into the kernel pool (by one or more
C      previous calls to FURNSH).  The Euler angles are typically
C      stored in the P_constants kernel file that comes with
C      SPICELIB.
C
C      1)  In the following code fragment, TIPBOD is used to transform
C          a position in J2000 inertial coordinates to a state in
C          bodyfixed coordinates.
C
C          The 3-vectors POSTN represents the inertial position
C          of an object with respect to the center of the
C          body at time ET.
C
C             C
C             C     First load the kernel pool.
C             C
C                   CALL FURNSH ( 'PLANETARY_CONSTANTS.KER' )
C
C             C
C             C     Next get the transformation and its derivative.
C             C
C                   CALL TIPBOD ( 'J2000', BODY, ET, TIPM )
C
C             C
C             C     Convert position, the first three elements of
C             C     STATE, to bodyfixed coordinates.
C             C
C                   CALL MXVG    ( TIPM, POSTN, BDPOS )
C
C$ Restrictions
C
C      The kernel pool must be loaded with the appropriate
C      coefficients (from the P_constants kernel or binary PCK file)
C      prior to calling this routine.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman   (JPL)
C      W.L. Taber     (JPL)
C      K.S. Zukor     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM call.  Replaced header references to LDPOOL with
C        references to FURNSH.
C
C-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
C
C        Tests of routine FAILED() were added.
C
C-     SPICELIB Version 1.0.3, 10-MAR-1994 (KSZ)
C
C         Underlying BODMAT code changed to look for binary PCK
C         data files, and use them to get orientation information if
C         they are available.  Only the comments to TIPBOD changed.
C
C-     SPICELIB Version 1.0.2, 06-JUL-1993 (HAN)
C
C         Example in header was corrected. Previous version had
C         incorrect matrix dimension specifications passed to MXVG.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 05-AUG-1991 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     transformation from inertial position to bodyfixed
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 06-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM call.  Replaced header references to LDPOOL with
C        references to FURNSH.
C
C
C-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
C
C        Tests of routine FAILED() were added.  The new checks
C        are intended to prevent arithmetic operations from
C        being performed with uninitialized or invalid data.
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      REF2J  ( 3, 3 )
      DOUBLE PRECISION      TMPMAT ( 3, 3 )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TIPBOD' )
      END IF
 
C
C     Get the transformation from the inertial from REF to J2000
C     coordinates.
C
      CALL IRFTRN ( REF, 'J2000', REF2J )
 
C
C     Get the transformation from J2000 to body-fixed coordinates
C     for the requested epoch.
C
      CALL BODMAT ( BODY, ET, TIPM )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TIPBOD' )
         RETURN
      END IF

C
C     Compose the transformations to arrive at the REF-to-J2000
C     transformation.
C
      CALL MXM   ( TIPM,   REF2J, TMPMAT )
      CALL MOVED ( TMPMAT, 9,     TIPM   )
 
C
C     That's all folks.  Check out and get out.
C
      CALL CHKOUT ( 'TIPBOD' )
      RETURN
      END
