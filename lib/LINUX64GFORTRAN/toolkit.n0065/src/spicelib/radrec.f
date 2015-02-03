C$Procedure      RADREC ( Range, RA and DEC to rectangular coordinates )
 
      SUBROUTINE RADREC ( RANGE, RA, DEC, RECTAN )
 
C$ Abstract
C
C     Convert from range, right ascension, and declination to
C     rectangular coordinates.
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
C     CONVERSION,   COORDINATES
C
C$ Declarations
 
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      RECTAN ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     RANGE      I   Distance of a point from the origin.
C     RA         I   Right ascension in radians.
C     DEC        I   Declination in radians.
C     RECTAN     O   Rectangular coordinates of the point.
C
C$ Detailed_Input
C
C     RANGE      is the distance of the point from the origin. Input
C                should be in terms of the same units in which the
C                output is desired.
C
C
C     RA         is the right ascension of RECTAN.  This is the angular
C                distance measured toward the east from the prime
C                meridian to the meridian containing the input point.
C                The direction of increasing right ascension is from
C                the +X axis towards the +Y axis.
C
C                The range (i.e., the set of allowed values) of 
C                RA is unrestricted.  Units are radians.
C
C
C     DEC        is the declination of RECTAN.  This is the angle from
C                the XY plane of the ray from the origin through the
C                point.
C
C                The range (i.e., the set of allowed values) of 
C                DEC is unrestricted.  Units are radians.
C
C
C$ Detailed_Output
C
C     RECTAN     is the array containing the rectangular coordinates of
C                the point.  
C
C                The units associated with RECTAN are those
C                associated with the input RANGE.
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
C     This routine converts the right ascension, declination, and range
C     of a point into the associated rectangular coordinates.
C
C     The input is defined by a distance from a central reference point,
C     an angle from a reference meridian, and an angle above the equator
C     of a sphere centered at the central reference point.
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
C     "Celestial Mechanics, A Computational Guide for the Practitioner"
C           by Laurence G. Taff
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity.  Some
C        minor header corrections were made.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
C
C-&
 
C$ Index_Entries
C
C     range ra and dec to rectangular coordinates
C     right_ascension and declination to rectangular
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 19-MAY-1989 (HAN)
C
C         Removed calls to CHKIN and CHKOUT. This routine is
C         "error free" and should not have been participating
C         in error handling.
C
C-&
 
 
C
C     Convert from range, right ascension, and declination to
C     rectangular coordinates by calling the routine LATREC.
C
 
      CALL LATREC ( RANGE, RA, DEC, RECTAN )
 
      RETURN
      END
