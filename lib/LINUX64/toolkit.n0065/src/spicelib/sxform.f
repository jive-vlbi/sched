C$Procedure      SXFORM ( State Transformation Matrix )
 
      SUBROUTINE SXFORM ( FROM, TO, ET, XFORM )
 
C$ Abstract
C
C     Return the state transformation matrix from one frame to
C     another at a specified epoch.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         FROM
      CHARACTER*(*)         TO
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      XFORM  ( 6, 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FROM       I   Name of the frame to transform from.
C     TO         I   Name of the frame to transform to.
C     ET         I   Epoch of the state transformation matrix.
C     XFORM      O   A state transformation matrix.
C
C$ Detailed_Input
C
C     FROM        is the name of a reference frame in which a state is
C                 known.
C
C     TO          is the name of a reference frame in which it is
C                 desired to represent the state.
C
C     ET          is the epoch in ephemeris seconds past the epoch of
C                 J2000 (TDB) at which the state transformation matrix
C                 should be evaluated.
C
C$ Detailed_Output
C
C     XFORM       is the state transformation matrix that transforms
C                 states from the reference frame FROM to the frame TO
C                 at epoch ET.  If (x, y, z, dx, dy, dz) is a state
C                 relative to the frame FROM then the vector ( x', y',
C                 z', dx', dy', dz' ) is the same state relative to the
C                 frame TO at epoch ET.  Here the vector ( x', y', z',
C                 dx', dy', dz' ) is defined by the equation:
C
C                   -   -       -          -     -  -
C                  | x'  |     |            |   | x  |
C                  | y'  |     |            |   | y  |
C                  | z'  |  =  |   XFORM    |   | z  |
C                  | dx' |     |            |   | dx |
C                  | dy' |     |            |   | dy |
C                  | dz' |     |            |   | dz |
C                   -   -       -          -     -  -
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If sufficient information has not been supplied via loaded
C        SPICE kernels to compute the transformation between the
C        two frames, the error will be diagnosed by a routine
C        in the call tree to this routine.
C
C     2) If either frame FROM or TO is not recognized the error
C        SPICE(UNKNOWNFRAME) will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides the user level interface to computing state
C     transformations from one reference frame to another.
C
C     Note that the reference frames may be inertial or non-inertial.
C     However, the user must take care that sufficient SPICE kernel
C     information is loaded to provide a complete state transformation
C     path from the FROM frame to the TO frame.
C
C$ Examples
C
C     Suppose that you have geodetic coordinates of a station on
C     the surface of the earth and that you need the inertial
C     (J2000) state of this station.  The following code fragment
C     illustrates how to transform the position of the station to
C     a J2000 state.
C
C        CALL BODVRD ( 'EARTH', RADII, 3, N, ABC  )
C
C        EQUATR   =  ABC(1)
C        POLAR    =  ABC(3)
C        F        = (EQUATR - POLAR) / EQUATR
C
C        CALL GEOREC ( LONG, LAT, 0.0D0,  EQUATR, F,  ESTATE )
C
C        ESTATE(4) = 0.0D0
C        ESTATE(5) = 0.0D0
C        ESTATE(6) = 0.0D0
C
C        CALL SXFORM ( 'IAU_EARTH', 'J2000',   ET, XFORM  )
C        CALL MXVG   (  XFORM,       ESTATE, 6, 6, JSTATE )
C
C     The state JSTATE is the desired J2000 state of the station.
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
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 23-SEP-2013 (BVS) 
C
C        Updated to save the input frame names and POOL state counters
C        and to do frame name-ID conversions only if the counters have
C        changed.
C
C-    SPICELIB Version 1.0.3, 27-FEB-2008 (BVS) 
C
C        Added FRAMES to the Required_Reading section.
C
C-    SPICELIB Version 1.0.2, 23-OCT-2005 (NJB) 
C
C        Header example had invalid flattening factor computation;
C        this was corrected.  Reference to BODVAR in header was 
C        replaced with reference to BODVRD.
C
C-    SPICELIB Version 1.0.1, 29-JUL-2003 (NJB) (CHA)
C
C        Minor header changes were made to improve clarity.
C
C-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Find a state transformation matrix
C
C-&
 
C
C     Spicelib Functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local Variables.
C
      INTEGER               FCODE
      INTEGER               TCODE

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVFROM
      INTEGER               SVFCOD

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVTO
      INTEGER               SVTCDE

      LOGICAL               FIRST

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVFROM
      SAVE                  SVFCOD

      SAVE                  SVCTR2
      SAVE                  SVTO
      SAVE                  SVTCDE

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SXFORM' )
 
C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counters
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )

         FIRST = .FALSE.

      END IF
 
  
      CALL ZZNAMFRM ( SVCTR1, SVFROM, SVFCOD, FROM, FCODE )
      CALL ZZNAMFRM ( SVCTR2, SVTO,   SVTCDE, TO,   TCODE )

C
C     Only non-zero id-codes are legitimate frame id-codes.  Zero
C     indicates that the frame wasn't recognized.
C
      IF      ( FCODE .NE. 0 .AND. TCODE .NE. 0 ) THEN
 
         CALL FRMCHG ( FCODE, TCODE, ET, XFORM )
 
      ELSE IF ( FCODE .EQ. 0 .AND. TCODE .EQ. 0 ) THEN
 
         CALL SETMSG ( 'Neither of the frames # or # was '
     .   //            'recognized as a known reference frame. ' )
         CALL ERRCH  ( '#', FROM )
         CALL ERRCH  ( '#', TO   )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
 
      ELSE IF ( FCODE .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The frame # was not recognized as a '
     .   //            'known reference frame. ' )
         CALL ERRCH  ( '#', FROM )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
 
      ELSE IF ( TCODE .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The frame # was not recognized as a '
     .   //            'known reference frame. ' )
         CALL ERRCH  ( '#', TO   )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
 
      END IF
 
      CALL CHKOUT ( 'SXFORM' )
      RETURN
      END
