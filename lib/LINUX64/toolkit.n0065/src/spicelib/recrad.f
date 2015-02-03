C$Procedure      RECRAD ( Rectangular coordinates to RA and DEC )
 
      SUBROUTINE RECRAD ( RECTAN, RANGE, RA, DEC )
 
C$ Abstract
C
C     Convert rectangular coordinates to range, right ascension,
C     and declination.
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
C     CONVERSION,  COORDINATES
C
C$ Declarations
 
      DOUBLE PRECISION   RECTAN ( 3 )
      DOUBLE PRECISION   RANGE
      DOUBLE PRECISION   RA
      DOUBLE PRECISION   DEC
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RECTAN     I   Rectangular coordinates of a point.
C     RANGE      O   Distance of the point from the origin.
C     RA         O   Right ascension in radians.
C     DEC        O   Declination in radians.
C
C$ Detailed_Input
C
C     RECTAN     The rectangular coordinates of a point.
C
C$ Detailed_Output
C
C     RANGE      is the distance of the point from the origin.
C
C                The units associated with RANGE are those
C                associated with the input RECTAN.
C
C
C     RA         is the right ascension of RECTAN.  This is the angular
C                distance measured toward the east from the prime
C                meridian to the meridian containing the input point.
C                The direction of increasing right ascension is from
C                the +X axis towards the +Y axis.
C
C                RA is output in radians.  The range of RA is [0, 2*pi].
C
C
C     DEC        is the declination of RECTAN.  This is the angle from
C                the XY plane of the ray from the origin through the
C                point.
C
C                DEC is output in radians.  The range of DEC is
C                [-pi/2, pi/2].
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the X and Y components of RECTAN are both zero, the
C        right ascension is set to zero.
C
C     2) If RECTAN is the zero vector, right ascension and declination
C        are both set to zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns the range, right ascension, and declination
C     of a point specified in rectangular coordinates.
C
C     The output is defined by a distance from a central reference
C     point, an angle from a reference meridian, and an angle above
C     the equator of a sphere centered at the central reference
C     point.
C
C$ Examples
C
C     The following code fragment converts right ascension and 
C     declination from the B1950 reference frame to the J2000 frame.
C
C        C
C        C     Convert RA and DEC to a 3-vector expressed in
C        C     the B1950 frame.
C        C
C              CALL RADREC ( 1.D0, RA, DEC, V1950 )
C        C
C        C     We use the SPICELIB routine PXFORM to obtain the
C        C     transformation  matrix for converting vectors between 
C        C     the B1950 and J2000 reference frames.  Since
C        C     both frames are inertial, the input time value we 
C        C     supply to PXFORM is arbitrary.  We choose zero
C        C     seconds past the J2000 epoch.
C        C
C              CALL PXFORM ( 'B1950', 'J2000', 0.D0, MTRANS )
C        C
C        C     Transform the vector to the J2000 frame.
C        C   
C              CALL MXV ( MTRANS, V1950, V2000 )
C        C
C        C     Find the RA and DEC of the J2000-relative vector.
C        C
C              CALL RECRAD ( V2000, R, RA, DEC )
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity.  Some
C        minor header corrections were made.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
C
C-&
 
C$ Index_Entries
C
C     rectangular coordinates to ra and dec
C     rectangular to right_ascension and declination
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      TWOPI
 
C
C     Call the subroutine RECLAT to convert the rectangular coordinates
C     into latitudinal coordinates.  In RECLAT, the longitude ( which
C     is returned to this subroutine as RA ) ranges from - pi to pi
C     radians.   Because the right ascension ranges from zero to
C     two pi radians, whenever RA is negative two pi must be added to
C     it.
C
      CALL RECLAT ( RECTAN, RANGE, RA, DEC )
 
 
      IF ( RA .LT. 0 ) THEN
         RA = RA + TWOPI()
      END IF
 
 
      RETURN
      END
