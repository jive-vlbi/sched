C$Procedure CHGIRF ( Change inertial reference frames )
 
      SUBROUTINE CHGIRF ( REFA, REFB, ROTAB, NAME, INDEX )
 
C$ Abstract
C
C     Support changes among a standard set of inertial coordinate
C     reference frames.
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
C     CONVERSION
C     COORDINATES
C     EPHEMERIS
C     FRAMES
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C     VECTOR
C
C$ Declarations
 
      INCLUDE              'ninert.inc'
      INTEGER               REFA
      INTEGER               REFB
      DOUBLE PRECISION      ROTAB    ( 3,3 )
      CHARACTER*(*)         NAME
      INTEGER               INDEX
 
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     REFA       I   IRFROT
C     REFB       I   IRFROT
C     ROTAB      O   IRFROT
C     NAME      I/O  IRFNUM, IRFNAM, IRFDEF
C     INDEX     I/O  IRFNUM, IRFNAM
C
C$ Detailed_Input
C
C     See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF.
C
C$ Detailed_Output
C
C     See entry points IRFROT, IRFNUM, and IRFNAM.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If CHGIRF is called directly, the signal SPICE(BOGUSENTRY)
C        is signalled.
C
C     2) See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF for
C        exceptions specific to those routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     CHGIRF exists only as an umbrella for data to be shared
C     by its entry points (IRFROT, IRFNUM, IRFNAM, and IRFDEF).
C     It should never be called directly.
C
C$ Examples
C
C     See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1] Jay Lieske, ``Precession Matrix Based on IAU (1976)
C         System of Astronomical Constants,'' Astron. Astrophys.
C         73, 282-284 (1979).
C
C     [2] E.M. Standish, Jr., ``Orientation of the JPL Ephemerides,
C         DE 200/LE 200, to the Dynamical Equinox of J2000,''
C         Astron. Astrophys. 114, 297-302 (1982).
C
C     [3] E.M. Standish, Jr., ``Conversion of Ephemeris Coordinates
C         from the B1950 System to the J2000 System,'' JPL IOM
C         314.6-581, 24 June 1985.
C
C     [4] E.M. Standish, Jr., ``The Equinox Offsets of the JPL
C         Ephemeris,'' JPL IOM 314.6-929, 26 February 1988.
C
C     [5] Jay Lieske, ``Expressions for the Precession  Quantities
C         Based upon the IAU (1976) System of Astronomical
C         Constants'' Astron. Astrophys. 58, 1-16 (1977).
C
C     [6] Laura Bass and Robert Cesarone "Mars Observer Planetary
C         Constants and Models" JPL D-3444 November 1990.
C
C     [7] "Explanatory Supplement to the Astronomical Almanac"
C          edited by P. Kenneth Seidelmann. University Science
C          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.4.0, 24-SEP-2013 (BVS)
C
C        Updated enrtry point IRFNUM to treat J2000 as a special case
C        and to not participate of CHKIN/CHOUT to increase efficiency.
C
C-    SPICELIB Version 4.3.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in CONVRT, ROTMAT and MXM calls.
C
C-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
C
C        Added DE-143 to header description for IRFROT.
C
C-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
C
C        A descriptive diagnostic was added to the entry points
C        IRFROT and IRFDEF.  Before they simply signalled the error
C        with no diagnostic.
C
C-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
C
C        The number of inertial frames recognized is now stored
C        in the include file ninert.inc.
C
C-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
C
C        The inertial frame DE-143 was added to the list of recognized
C        inertial frames.
C
C-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
C
C        The inertial frames DE-140 and DE-142  were added to the
C        list of recognized inertial frames.
C
C-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
C
C        The transformation from J2000 to B1950 was upgraded
C        so that the transformation matrix produced matches
C        the matrix given in [1].
C
C        The frame MARSIAU was added to the list
C        of recognized frames.  This is the standard mars
C        referenced inertial frame used by the Mars Observer
C        project.
C
C        Values for the obliquity of the ecliptic were taken
C        from the Explanatory Supplement [7] to the Astronomical
C        Almanac (1992) at both the epochs J2000 and B1950 and
C        used to define the mean ecliptic and equinox frames
C        ECLIPJ2000 and ECLIPB1950.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     change inertial reference frames
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.3.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in CONVRT, ROTMAT  and MXM calls.
C
C-& 
 
C
C     SPICELIB functions
C
      INTEGER               ESRCHC
      INTEGER               ISRCHC
      INTEGER               WDCNT

      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               MAXF
      PARAMETER           ( MAXF = NINERT )
 
C
C     Local variables
C
      CHARACTER*16          BASES  (   MAXF )
      CHARACTER*80          DEFS   (   MAXF )
      CHARACTER*25          ERROR
      CHARACTER*16          FRAMES (   MAXF )
      CHARACTER*25          WORD
 
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      RADANG
      DOUBLE PRECISION      TMPMAT ( 3, 3    )
      DOUBLE PRECISION      TRANS  ( 9, MAXF )
 
      INTEGER               AXIS
      INTEGER               B
      INTEGER               DFRAME
      INTEGER               I
      INTEGER               J
      INTEGER               LOC
      INTEGER               P
 
      LOGICAL               READY
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
C
C     Each frame is defined in terms of another frame, except for
C     the root frame, which is defined in terms of itself. For now,
C     the root frame is the standard IAU reference frame, J2000,
C     defined by the Earth mean equator and dynamical equinox of
C     Julian year 2000.
C
C     Each definition consists of a series of rotations, each
C     through some angle (in arc seconds) and about some axis.
C     The rotations are listed in the opposite order in which
C     they are to be performed, so as to correspond more closely
C     to conventional notation. For example, the definition
C
C        FRAMES(i) = 'F2'
C        BASES(i)  = 'F1'
C        DEFS(i)   = '22.34  3   31.21  2   0.449  1'
C
C     means that a vector in frame F1 is converted to the equivalent
C     vector in frame F2 by applying the following rotation:
C
C        -                                            -
C        v    = ( [ 22.34 ]  [ 31.21 ]  [ 0.449 ]  )  v
C         F2               3          2          1     F1
C
C     where the notation
C
C        [ theta ]
C                 a
C
C     means ``rotate through angle theta about axis a.''
C
C     New frames may be added by:
C
C        1) Increasing the value of MAXF.
C
C        2) Adding new values for FRAMES, BASES, and DEFS.
C
C     The actual transformations (TRANS) will be computed during
C     initialization.
C
C     Note that BASES must be the name of a previously defined
C     reference frame, and that no frame should appear more than
C     once in FRAMES.
C
C     Note also that the list of valid reference frames maintained
C     by CHGIRF must be updated whenever new frames are added.
C
      DATA READY       / .FALSE. /
 
C
C     The root frame is mostly for show. Rotate by 0 arc seconds
C     about the x-axis to obtain the identity matrix.
C
      DATA FRAMES( 1)  / 'J2000'  /
      DATA BASES ( 1)  / 'J2000'  /
      DATA DEFS  ( 1)  / '0.0  1' /
 
C
C     The B1950 reference frame is obtained by precessing the J2000
C     frame backwards from Julian year 2000 to Besselian year 1950,
C     using the 1976 IAU precession model.
C
C     The rotation from B1950 to J2000 is
C
C        [ -z ]  [ theta ]  [ -zeta ]
C              3          2          3
C
C     So the rotation from J2000 to B1950 is the transpose,
C
C        [ zeta ]  [ -theta ]  [ z ]
C                3           2      3
C
C     The values for z, theta, and zeta are taken directly from
C     are computed from the formulas given in table 5 of [5].
C
C        z     =  1153.04066200330"
C        theta = -1002.26108439117"
C        zeta  =  1152.84248596724"
C
      DATA FRAMES( 2)  / 'B1950' /
      DATA BASES ( 2)  / 'J2000' /
      DATA DEFS  ( 2)  /
     .'1152.84248596724 3  -1002.26108439117  2  1153.04066200330  3' /
 
C
C     The FK4 reference frame is derived from the B1950 frame by
C     applying the equinox offset determined by Fricke. This is just
C     the rotation
C
C        [ 0.525" ]
C                  3
C
      DATA FRAMES( 3)  / 'FK4'   /
      DATA BASES ( 3)  / 'B1950' /
      DATA DEFS  ( 3)  / '0.525  3' /
 
 
C
C     The DE-118 reference frame is nearly identical to the FK4
C     reference frame. It is also derived from the B1950 frame.
C     Only the offset is different:
C
C        [ 0.53155" ]
C                    3
C
C     In [2], Standish uses two separate rotations,
C
C        [ 0.00073" ]  P [ 0.5316" ]
C                    3              3
C
C     (where P is the precession matrix used above to define the
C     B1950 frame). The major effect of the second rotation is to
C     correct for truncating the magnitude of the first rotation.
C     At his suggestion, we will use the untruncated value, and
C     stick to a single rotation.
C
      DATA FRAMES( 4)  / 'DE-118' /
      DATA BASES ( 4)  / 'B1950'  /
      DATA DEFS  ( 4)  / '0.53155  3' /
 
C
C     Most of the other DE reference frames may be defined relative
C     to either the DE-118 or B1950 frames. The values below are taken
C     from [4].
C
C        DE number   Offset from DE-118   Offset from B1950
C        ---------   ------------------   -----------------
C               96             +0.1209"            +0.4107"
C              102             +0.3956"            +0.1359"
C              108             +0.0541"            +0.4775"
C              111             -0.0564"            +0.5880"
C              114             -0.0213"            +0.5529"
C              122             +0.0000"            +0.5316"
C              125             -0.0438"            +0.5754"
C              130             +0.0069"            +0.5247"
C
C     We will use B1950 for now, since the offsets generally have
C     more significant digits.
C
      DATA FRAMES( 5)  / 'DE-96'  /
      DATA BASES ( 5)  / 'B1950'  /
      DATA DEFS  ( 5)  / '0.4107  3' /
 
      DATA FRAMES( 6)  / 'DE-102' /
      DATA BASES ( 6)  / 'B1950'  /
      DATA DEFS  ( 6)  / '0.1359  3' /
 
      DATA FRAMES( 7)  / 'DE-108' /
      DATA BASES ( 7)  / 'B1950'  /
      DATA DEFS  ( 7)  / '0.4775  3' /
 
      DATA FRAMES( 8)  / 'DE-111' /
      DATA BASES ( 8)  / 'B1950'  /
      DATA DEFS  ( 8)  / '0.5880  3' /
 
      DATA FRAMES( 9)  / 'DE-114' /
      DATA BASES ( 9)  / 'B1950'  /
      DATA DEFS  ( 9)  / '0.5529  3' /
 
      DATA FRAMES(10)  / 'DE-122' /
      DATA BASES (10)  / 'B1950'  /
      DATA DEFS  (10)  / '0.5316  3' /
 
      DATA FRAMES(11)  / 'DE-125' /
      DATA BASES (11)  / 'B1950'  /
      DATA DEFS  (11)  / '0.5754  3' /
 
      DATA FRAMES(12)  / 'DE-130' /
      DATA BASES (12)  / 'B1950'  /
      DATA DEFS  (12)  / '0.5247  3' /
 
C
C     The Galactic System II reference frame is defined by the
C     following rotations:
C
C             o          o            o
C        [ 327  ]  [ 62.6  ]  [ 282.25  ]
C                3          1            3
C
C      In the absence of better information, we will assume that
C      it is derived from the FK4 frame. Converting the angles from
C      degrees to arc seconds,
C
C           o
C        327      = 1177200"
C            o
C        62.6     =  225360"
C              o
C        282.25   = 1016100"
C
      DATA FRAMES(13)  / 'GALACTIC' /
      DATA BASES (13)  / 'FK4'      /
      DATA DEFS  (13)  / '1177200.0  3  225360.0  1  1016100.0  3' /
 
C
C     According to Standish, the various DE-200 frames are identical
C     with J2000, because he rotates the ephemerides before releasing
C     them (in order to avoid problems like the one that this routine
C     is designed to solve). Because we have to have something, we
C     will use
C
C             o
C        [ 0.0 ]
C               3
C
      DATA FRAMES(14)  / 'DE-200' /
      DATA BASES (14)  / 'J2000'  /
      DATA DEFS  (14)  / '0.0  3' /
 
      DATA FRAMES(15)  / 'DE-202' /
      DATA BASES (15)  / 'J2000'  /
      DATA DEFS  (15)  / '0.0  3' /
C
C     The values for the transformation from J2000 to MARSIAU_MO
C     are derived from the constants given for the pole of Mars
C     on page 8-2 of reference [6].
C
      DATA FRAMES(16)  / 'MARSIAU' /
      DATA BASES (16)  / 'J2000'   /
      DATA DEFS  (16)  / '324000.0D0 3 133610.4D0 2 -152348.4D0 3' /
 
C
C     The value for the obliquity of the ecliptic at J2000  is
C     taken from page  114 of [7] equation 3.222-1.  This agrees
C     with the expression given in [5]
C
      DATA FRAMES(17)  / 'ECLIPJ2000'   /
      DATA BASES (17)  / 'J2000'        /
      DATA DEFS  (17)  / '84381.448 1'  /
 
C
C     The value for the obliquity of the ecliptic at B1950  is
C     taken from page  171 of [7].
C
      DATA FRAMES(18)  / 'ECLIPB1950'   /
      DATA BASES (18)  / 'B1950'       /
      DATA DEFS  (18)  / '84404.836 1' /
 
C
C     The frame for DE-140 is simply DE-400 rotated by the rotation:
C
C      0.9999256765384668  0.0111817701197967  0.0048589521583895
C     -0.0111817701797229  0.9999374816848701 -0.0000271545195858
C     -0.0048589520204830 -0.0000271791849815  0.9999881948535965
C
C     Note that the DE-400 frame is J2000.
C
C     The transpose of this is the frame from DE140 to DE400. To get
C     the euler angles below, the matrix given above was copied into
C     a matrix XFORM.
C
C     This matrix was transposed to give the transformation from
C     DE-140 to J2000.
C
C        CALL XPOSE ( XFORM, XFORM )
C
C     Using the SPICE routine M2EUL, the euler representation of the
C     transformation from DE140 to J2000 was constructed.
C
C        CALL M2EUL ( XFORM, 3, 2, 3, A1, A2, A3 )
C
C     Angles were converted to the range from -180 to 180 degrees
C     and converted to arcseconds.  At this point we have the
C     euler representation from DE-140 to J2000.
C
C        [ A1 ]  [ A2 ]  [ A3 ]
C              3       2       3
C
C     To get the Euler representation of the transformation from
C     J2000 to DE-140  we use.
C
C        [ -A3 ]  [ -A2 ] [ -A1 ]
C               3        2       3
C
C     This method was used because it yields a nicer form of
C     representation than the straight forward transformation.
C     Note that these numbers are quite close to the values used
C     for the transformation from J2000 to B1950
C
 
      DATA FRAMES(19)  / 'DE-140'       /
      DATA BASES (19)  / 'J2000'        /
      DATA DEFS  (19)  /
     .'1152.71013777252 3  -1002.25042010533  2  1153.75719544491  3'/
C
C     The frame for DE-142 is simply DE-402 rotated by the rotation:
C
C      0.9999256765402605  0.0111817697320531  0.0048589526815484
C     -0.0111817697907755  0.9999374816892126 -0.0000271547693170
C     -0.0048589525464121 -0.0000271789392288  0.9999881948510477
C
C     Note that the DE-402 frame is J2000.
C
C     The Euler angles giving the transformation for J2000 to
C     DE-142 were constructed in the same way as the transformation
C     from J2000 to DE140.  Only the input matrix changed to use the
C     one given above.
C
      DATA FRAMES(20)  / 'DE-142'       /
      DATA BASES (20)  / 'J2000'        /
      DATA DEFS  (20)  /
     .'1152.72061453864 3  -1002.25052830351  2  1153.74663857521  3' /
 
C
C     The frame for DE-143 is simply DE-403 rotated by the rotation:
C
C      0.9999256765435852  0.0111817743077255  0.0048589414674762
C     -0.0111817743300355  0.9999374816382505 -0.0000271622115251
C     -0.0048589414161348 -0.0000271713942366  0.9999881949053349
C
C     Note that the DE-403 frame is J2000.
C
C     The Euler angles giving the transformation for J2000 to
C     DE-143 were constructed in the same way as the transformation
C     from J2000 to DE140.  Only the input matrix changed to use the
C     one given above.
C
      DATA FRAMES(21)  / 'DE-143'       /
      DATA BASES (21)  / 'J2000'        /
      DATA DEFS  (21)
     ./'1153.03919093833, 3, -1002.24822382286, 2, 1153.42900222357, 3'/
 
C
C     Until defined (by a call to IRFDEF), the default frame is
C     undefined.
C
      DATA DFRAME      / 0 /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CHGIRF' )
      END IF
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'CHGIRF' )
      RETURN
 
 
 
C$Procedure IRFROT ( Inertial reference frame rotation )
 
      ENTRY IRFROT ( REFA, REFB, ROTAB )
 
C$ Abstract
C
C     Compute the matrix needed to rotate vectors between two
C     standard inertial reference frames.
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
C     CONVERSION
C     COORDINATES
C     EPHEMERIS
C     FRAMES
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C     VECTOR
C
C$ Declarations
C
C     INTEGER               REFA
C     INTEGER               REFB
C     DOUBLE PRECISION      ROTAB    ( 3,3 )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     REFA,
C     REFB       I   Indices of target reference frames (A,B).
C     MATRIX     O   Rotation from frame A to frame B.
C
C$ Detailed_Input
C
C     REFA,
C     REFB        are the indices of two standard inertial reference
C                 frames. The complete set of supported frames is shown
C                 below.
C
C                    Index  Name      Description
C                    -----  --------  --------------------------------
C                     1    J2000      Earth mean equator, dynamical
C                                     equinox of J2000
C
C                     2    B1950      Earth mean equator, dynamical
C                                     equinox of B1950
C
C                     3    FK4        Fundamental Catalog (4)
C
C                     4    DE-118     JPL Developmental Ephemeris (118)
C
C                     5    DE-96      JPL Developmental Ephemeris ( 96)
C
C                     6    DE-102     JPL Developmental Ephemeris (102)
C
C                     7    DE-108     JPL Developmental Ephemeris (108)
C
C                     8    DE-111     JPL Developmental Ephemeris (111)
C
C                     9    DE-114     JPL Developmental Ephemeris (114)
C
C                    10    DE-122     JPL Developmental Ephemeris (122)
C
C                    11    DE-125     JPL Developmental Ephemeris (125)
C
C                    12    DE-130     JPL Developmental Ephemeris (130)
C
C                    13    GALACTIC   Galactic System II
C
C                    14    DE-200     JPL Developmental Ephemeris (200)
C
C                    15    DE-202     JPL Developmental Ephemeris (202)
C
C                    16    MARSIAU    Mars Observer inertial frame
C                                     defined relative to MARS.
C
C                    17    ECLIPJ2000 Earth mean ecliptic and equinox
C                                     of the epoch J2000
C
C                    18    ECLIPB1950 Earth mean ecliptic and equinox
C                                     of the Besselian date 1950.
C
C                    19    DE-140    JPL Developmental Ephemeris (140)
C
C                    20    DE-142    JPL Developmental Ephemeris (142)
C
C                    21    DE-143    JPL Developmental Ephemeris (143)
C
C$ Detailed_Output
C
C     ROTAB       is the rotation which, when applied to a vector v
C                 in reference frame A,
C                    _            _
C                    v  = (ROTAB) v
C                     B            A
C
C                 yields the same vector in reference frame B. The
C                 inverse rotation is performed by applying the
C                 transpose,
C                    _           T _
C                    v  = (ROTAB)  v
C                     A             B
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If either REFA or REFB is outside the range [1,MAXF],
C        where MAXF is the number of supported frames, the error
C        SPICE(IRFNOTREC) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     IRFROT exists primarily for use by the ephemeris and star
C     catalog readers in the SPICELIB toolkit library.
C
C$ Examples
C
C     In the following code fragment, IRFROT is used to rotate
C     vectors originally referenced to the DE-118 coordinate frame
C     to equivalent vectors referenced to the IAU standard J2000
C     reference frame.
C
C        CALL IRFROT ( 4, 1, R )
C
C        CALL MXV ( R, SC1950, SC2000 )
C        CALL MXV ( R, MP1950, MP2000 )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     See subroutine CHGIRF.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.3.0, 24-SEP-2013 (BVS)
C
C        Updated to do discovery check-in/check-out.
C
C-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
C
C        Added DE-143 to header description for IRFROT.
C
C-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
C
C        A descriptive diagnostic was added to the entry points
C        IRFROT and IRFDEF.  Before they simply signalled the error
C        with no diagnostic.
C
C-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
C
C        The number of inertial frames recognized is now stored
C        in the include file ninert.inc.
C
C-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
C
C        The inertial frame DE-143 was added to the list of recognized
C        inertial frames.
C
C-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
C
C        The inertial frames DE-140 and DE-142  were added to the
C        list of recognized inertial frames.
C
C-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
C
C        The transformation from J2000 to B1950 was upgraded
C        so that the transformation matrix produced matches
C        the matrix given in [1].
C
C        The frame MARSIAU was added to the list
C        of recognized frames.  This is the standard mars
C        referenced inertial frame used by the Mars Observer
C        project.
C
C        Values for the obliquity of the ecliptic were taken
C        from the Explanatory Supplement [7] to the Astronomical
C        Almanac (1992) at both the epochs J2000 and B1950 and
C        used to define the mean ecliptic and equinox frames
C        ECLIPJ2000 and ECLIPB1950.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     inertial reference frame rotation
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     If it has not been done already, construct the transformation
C     from the root frame to each supported reference frame.
C
C     Begin by constructing the identity matrix (rotating by zero
C     radians about the x-axis). Apply the rotations indicated in
C     the frame definition (from right to left) to get the incremental
C     rotation from the base frame. The final rotation is
C
C        R             = (R           ) (R          )
C         root->frame      base->frame    root->base
C
      IF ( .NOT. READY ) THEN
 
         CALL CHKIN ( 'IRFROT' )

         DO I = 1, MAXF

            CALL ROTATE ( 0.D0, 1, TRANS(1,I) )
 
            DO J = WDCNT ( DEFS(I) ), 2, -2

               CALL NTHWD  ( DEFS(I),     J,  WORD, LOC )
               CALL NPARSI ( WORD,     AXIS, ERROR,   P )
 
               CALL NTHWD  ( DEFS(I),   J-1,  WORD, LOC )
               CALL NPARSD ( WORD,    ANGLE, ERROR,   P )
 
               CALL CONVRT ( ANGLE, 'ARCSECONDS', 'RADIANS', RADANG )

               CALL ROTMAT ( TRANS(1,I),  RADANG,  AXIS,  TMPMAT     )
               CALL MOVED  ( TMPMAT,      9,              TRANS(1,I) )

            END DO
 
            B = ISRCHC ( BASES(I), I, FRAMES )

            CALL MXM   ( TRANS(1,I), TRANS(1,B), TMPMAT     )
            CALL MOVED ( TMPMAT,     9,          TRANS(1,I) )

         END DO

         CALL CHKOUT ( 'IRFROT' )
 
         READY = .TRUE.
 
      END IF
 
C
C     If the transformations have been defined, we can proceed with
C     the business at hand: determining the rotation from one frame
C     to another. To get from frame A to frame B, the rotation is
C
C                                     T
C        R     = (R       ) (R       )
C         A->B     root->B    root->A
C
C     If A and B are the same frame, the rotation is just the identity.
C     In theory, computing
C
C                                     T
C        R     = (R       ) (R       )
C         A->A     root->A    root->A
C
C     should work, but why risk roundoff problems?
C
      IF ( REFA .LT. 1  .OR.  REFA .GT. MAXF ) THEN
 
         CALL CHKIN  ( 'IRFROT' )
         CALL SETMSG ( 'A request has been made to obtain the '
     .   //            'transformation from inertial reference '
     .   //            'frame # to inertial reference frame #. '
     .   //            'Unfortunately # is not the id-code of a '
     .   //            'known inertial frame. ' )
         CALL ERRINT ( '#', REFA )
         CALL ERRINT ( '#', REFB )
         CALL ERRINT ( '#', REFA )
         CALL SIGERR ( 'SPICE(IRFNOTREC)' )
         CALL CHKOUT ( 'IRFROT' )
 
      ELSE IF ( REFB .LT. 1  .OR.  REFB .GT. MAXF ) THEN
 
         CALL CHKIN  ( 'IRFROT' )
         CALL SETMSG ( 'A request has been made to obtain the '
     .   //            'transformation from inertial reference '
     .   //            'frame # to inertial reference frame #. '
     .   //            'Unfortunately # is not the id-code of a '
     .   //            'known inertial frame. ' )
         CALL ERRINT ( '#', REFA )
         CALL ERRINT ( '#', REFB )
         CALL ERRINT ( '#', REFB )
         CALL SIGERR ( 'SPICE(IRFNOTREC)' )
         CALL CHKOUT ( 'IRFROT' )
 
 
      ELSE IF ( REFA .EQ. REFB ) THEN
         CALL ROTATE ( 0.D0, 1, ROTAB )
 
      ELSE
         CALL MXMT ( TRANS(1,REFB), TRANS(1,REFA), ROTAB )
      END IF
 
      RETURN
 
 
 
 
C$Procedure IRFNUM ( Inertial reference frame number )
 
      ENTRY IRFNUM ( NAME, INDEX )
 
C$ Abstract
C
C     Return the index of one of the standard inertial reference
C     frames supported by IRFROT.
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
C     CONVERSION
C     COORDINATES
C     EPHEMERIS
C     FRAMES
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C     VECTOR
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               INDEX
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of standard inertial reference frame.
C     INDEX      O   Index of frame.
C
C$ Detailed_Input
C
C     NAME        is the name of one of the standard inertial
C                 reference frames supported by IRFROT, or
C                 'DEFAULT'.
C
C$ Detailed_Output
C
C     INDEX       is the index of the frame specified by NAME.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If NAME is not recognized, INDEX is zero.
C
C     2) If no default frame has been specified, INDEX is zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     IRFNUM is supplied as a convenience, to allow users to refer to
C     the various standard inertial reference frames by name.
C
C$ Examples
C
C     In the following example, the rotation from DE-118 to FK4 is
C     computed without knowing the indices of these frames.
C
C        CALL IRFNUM ( 'DE-118', A )
C        CALL IRFNUM ( 'FK4',    B )
C
C        CALL IRFROT ( A, B, ROTAB )
C
C     IRFNUM can be used to rotate vectors into the default frame,
C     as illustrated by the following code fragment.
C
C        CALL IRFNUM ( 'FK4',     A )
C        CALL IRFNUM ( 'DEFAULT', B )
C
C        CALL IRFROT ( A, B, ROTAB                 )
C        CALL MXV    (       ROTAB, OLDVEC, NEWVEC )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     See subroutine CHGIRF.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.3.0, 24-SEP-2013 (BVS)
C
C        Updated to treat J2000 as a special case and to not
C        CHKIN/CHOUT to increase efficiency.
C
C-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
C
C        Added DE-143 to header description for IRFROT.
C
C-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
C
C        A descriptive diagnostic was added to the entry points
C        IRFROT and IRFDEF.  Before they simply signalled the error
C        with no diagnostic.
C
C-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
C
C        The number of inertial frames recognized is now stored
C        in the include file ninert.inc.
C
C-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
C
C        The inertial frame DE-143 was added to the list of recognized
C        inertial frames.
C
C-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
C
C        The inertial frames DE-140 and DE-142  were added to the
C        list of recognized inertial frames.
C
C-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
C
C        The transformation from J2000 to B1950 was upgraded
C        so that the transformation matrix produced matches
C        the matrix given in [1].
C
C        The frame MARSIAU was added to the list
C        of recognized frames.  This is the standard mars
C        referenced inertial frame used by the Mars Observer
C        project.
C
C        Values for the obliquity of the ecliptic were taken
C        from the Explanatory Supplement [7] to the Astronomical
C        Almanac (1992) at both the epochs J2000 and B1950 and
C        used to define the mean ecliptic and equinox frames
C        ECLIPJ2000 and ECLIPB1950.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     inertial reference frame number
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      IF ( NAME .EQ. 'J2000' .OR. NAME .EQ. 'j2000' ) THEN
         INDEX = 1
         RETURN
      END IF

      IF ( EQSTR ( NAME, 'DEFAULT' ) ) THEN
         INDEX = DFRAME
      ELSE
         INDEX = ESRCHC ( NAME, MAXF, FRAMES )
      END IF
 
      RETURN
 
 
 
 
C$Procedure IRFNAM ( Inertial reference frame name )
 
      ENTRY IRFNAM ( INDEX, NAME )
 
C$ Abstract
C
C     Return the name of one of the standard inertial reference
C     frames supported by IRFROT.
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
C     CONVERSION
C     COORDINATES
C     EPHEMERIS
C     FRAMES
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C     VECTOR
C
C$ Declarations
C
C     INTEGER               INDEX
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INDEX      I   Index of standard inertial reference frame.
C     NAME       O   Name of frame.
C
C$ Detailed_Input
C
C     INDEX       is the index of one of the standard inertial
C                 reference frames supported by IRFROT.
C
C$ Detailed_Output
C
C     NAME        is the name of the frame specified by INDEX.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If INDEX is not the index of a supported frame, NAME is blank.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     IRFNAM is supplied as a convenience, to allow users to determine
C     the names of standard inertial reference frames referred to only
C     by index (as in the segment descriptors of a GEF ephemeris file).
C
C$ Examples
C
C     In the following example, the identity of a rotation from DE-118
C     to FK4 is deduced from the indices used to create the rotation.
C
C        CALL IRFROT ( A, B, ROTAB )
C
C        CALL IRFNAM ( A, NAME(1) )
C        CALL IRFNAM ( B, NAME(2) )
C
C        WRITE (6,*) 'Rotation from ' // NAME(1) // ' to ' // NAME(2)
C
C     Note that the name of the default reference frame can only be
C     recovered from the number:
C
C        CALL IRFNUM ( 'DEFAULT', DINDEX        )
C        CALL IRFNAM (            DINDEX, DNAME )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     See subroutine CHGIRF.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
C
C        Added DE-143 to header description for IRFROT.
C
C-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
C
C        A descriptive diagnostic was added to the entry points
C        IRFROT and IRFDEF.  Before they simply signalled the error
C        with no diagnostic.
C
C-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
C
C        The number of inertial frames recognized is now stored
C        in the include file ninert.inc.
C
C-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
C
C        The inertial frame DE-143 was added to the list of recognized
C        inertial frames.
C
C-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
C
C        The inertial frames DE-140 and DE-142  were added to the
C        list of recognized inertial frames.
C
C-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
C
C        The transformation from J2000 to B1950 was upgraded
C        so that the transformation matrix produced matches
C        the matrix given in [1].
C
C        The frame MARSIAU was added to the list
C        of recognized frames.  This is the standard mars
C        referenced inertial frame used by the Mars Observer
C        project.
C
C        Values for the obliquity of the ecliptic were taken
C        from the Explanatory Supplement [7] to the Astronomical
C        Almanac (1992) at both the epochs J2000 and B1950 and
C        used to define the mean ecliptic and equinox frames
C        ECLIPJ2000 and ECLIPB1950.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     inertial reference frame name
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'IRFNAM' )
      END IF
 
      IF ( INDEX .LT. 1  .OR.  INDEX .GT. MAXF ) THEN
         NAME = ' '
 
      ELSE
         NAME = FRAMES(INDEX)
      END IF
 
      CALL CHKOUT ( 'IRFNAM' )
      RETURN
 
 
 
C$Procedure IRFDEF ( Inertial reference frame, default )
 
      ENTRY IRFDEF ( INDEX )
 
C$ Abstract
C
C     Specify a standard inertial reference frame as the default
C     frame for a program.
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
C     CONVERSION
C     COORDINATES
C     EPHEMERIS
C     FRAMES
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C     VECTOR
C
C$ Declarations
C
C     INTEGER               INDEX
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INDEX      I   Index of default frame.
C
C$ Detailed_Input
C
C     INDEX       is the index of one of the standard inertial
C                 reference frames supported by IRFROT.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If INDEX is outside the range [1,MAXF], where MAXF is the
C        number of supported frames, the error SPICE(IRFNOTREC) is
C        signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     IRFDEF allows tools to be written at a relatively high level
C     without requiring the reference frame to be tramp coupled or
C     placed in global memory.
C
C$ Examples
C
C     Typically, the calling program will select a default frame
C     during initialization,
C
C        C
C        C     Use J2000 for all ephemeris, star data.
C        C
C              CALL IRFDEF ( 1 )
C
C     and recover the default frame at lower levels,
C
C        C
C        C     Rotate all vectors into the default frame.
C        C
C              CALL IRFNUM ( 'DEFAULT', REFD   )
C
C              DO I = 1, NVEC
C                 CALL IRFROT ( REFIN, REFD, ROT           )
C                 CALL MXV                   ROT, VEC, VEC )
C              END DO
C
C     Note that many utilities accept 'DEFAULT' as the name of
C     an inertial reference frame,
C
C        CALL SPKEZ ( TARGET, ..., 'DEFAULT', ... )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     See subroutine CHGIRF.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
C
C        Added DE-143 to header description for IRFROT.
C
C-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
C
C        A descriptive diagnostic was added to the entry points
C        IRFROT and IRFDEF.  Before they simply signalled the error
C        with no diagnostic.
C
C-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
C
C        The number of inertial frames recognized is now stored
C        in the include file ninert.inc.
C
C-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
C
C        The inertial frame DE-143 was added to the list of recognized
C        inertial frames.
C
C-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
C
C        The inertial frames DE-140 and DE-142  were added to the
C        list of recognized inertial frames.
C
C-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
C
C        The transformation from J2000 to B1950 was upgraded
C        so that the transformation matrix produced matches
C        the matrix given in [1].
C
C        The frame MARSIAU was added to the list
C        of recognized frames.  This is the standard mars
C        referenced inertial frame used by the Mars Observer
C        project.
C
C        Values for the obliquity of the ecliptic were taken
C        from the Explanatory Supplement [7] to the Astronomical
C        Almanac (1992) at both the epochs J2000 and B1950 and
C        used to define the mean ecliptic and equinox frames
C        ECLIPJ2000 and ECLIPB1950.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     inertial reference frame default
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'IRFDEF' )
      END IF
 
C
C     There's not much to do, except save the value for later use.
C
      IF ( INDEX .LT. 1  .OR.  INDEX .GT. MAXF ) THEN
 
         CALL SETMSG ( 'The reference frame with id-code # is '
     .   //            'not a recognized inertial reference '
     .   //            'frame. ' )
         CALL ERRINT ( '#', INDEX )
 
 
         CALL SIGERR ( 'SPICE(IRFNOTREC)' )
 
      ELSE
         DFRAME = INDEX
      END IF
 
      CALL CHKOUT ( 'IRFDEF' )
      RETURN
 
      END
