C$Procedure      PLNSNS ( Planetographic Longitude Sense )
 
      INTEGER FUNCTION PLNSNS ( BODID )
 
C$ Abstract
C
C    This function returns the quotient of the planetographic
C    and planetocentric longitude for a user specified body.
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
C     PCK
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               BODID
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODID      I   is the NAIF id-code of some solar system object.
C
C     Function returns planetographic/planetocentric
C
C$ Detailed_Input
C
C     BODID      is the NAIF id-code of some planet, asteroid, comet
C                or natural satellite of a planet.
C
C$ Detailed_Output
C
C     Based upon loaded PCK values in the kernel pool, the function
C     returns the quotient
C
C           planetographic longitude
C           ------------------------
C           planetocentric longitude
C
C     for the body specified by BODID.  I.e.  1 if planetographic
C     and planetocentric longitude are the same for the input body,
C     -1 if the planetographic and planetocentric longitude are
C     opposite for the specified body.  If PCK information for
C     the specified body can not be located in the kernel pool
C     the function returns the value 0.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If sufficient orientation information for the object
C     specified by BODID is not available in the kernel pool,
C     the function returns the value 0.
C
C$ Files
C
C     A text PCK kernel must be loaded via the routine FURNSH
C     that contains the orientation information for the body specified
C     by BODID.
C
C$ Particulars
C
C     This routine returns the multiplicative factor needed
C     to convert planetographic longitude to planetocentric
C     longitude.
C
C     This routine relies on the proper orientation for the
C     specified body having been loaded in the kernel pool.
C
C$ Examples
C
C     Suppose that you have the planetographic coordinates
C     of some point on the surface of an object and that you
C     need to convert these coordinates to bodyfixed rectangular
C     coordinates.  This conversion requires knowledge of the
C     sense of planetographic longitude.  The code fragment below
C     shows how you go about using this routine to perform the
C     conversion.
C
C     We assume that the variables LAT, LONG, HEIGHT contain the
C     planetographic latitude, longitude and height above the
C     reference surface of some point.  Moreover, let F be the
C     flattening factor for the reference spheroid.
C
C     ( F = (Equatorial Radius - Polar Radius ) / Equatorial Radius )
C
C     Finally, let EQRAD be the equatorial radius.
C
C     We first need to convert planetographic longitude to
C     planetocentric longitude.
C
C        FACTOR = PLNSNS(BODID)
C
C        IF ( FACTOR .EQ. 0 ) THEN
C
C           WRITE (*,*) 'Sorry, we don''t have data available.'
C           STOP
C
C        END IF
C
C     Compute the planetocentric longitude
C
C        PCLONG = FACTOR * LONG
C
C     Now convert the planetographic coordinates with
C     planetographic longitude replaced by planetocentric
C     longitude rectangular coordinates.  (Note the conversion
C     to planetocentric longitude is required because GEOREC
C     assumes that the ordering latitude, longitude, altitude
C     is a right handed ordering.  Replacing planetographic
C     longitude by planetocentric longitude ensures that we
C     have a right handed coordinate system.)
C
C        CALL GEOREC ( LAT, PCLONG, HEIGHT, EQRAD, F, REC )
C
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 11-MAY-2009 (BVS)
C
C        Replaced LDPOOL with FURNSN in the header. Re-ordered header
C        sections.
C
C-    SPICELIB Version 1.0.0, 7-JAN-1997 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Determine the sense of planetographic longitude.
C
C-&
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(1)         TYPE
      CHARACTER*(WDSIZE)    ITEM
 
      DOUBLE PRECISION      RATE
 
      INTEGER               N
      INTEGER               VALUE
 
      LOGICAL               FOUND
 
C
C     The earth is a special case so we just handle it here.
C
      IF ( BODID .EQ. 399 ) THEN
 
         PLNSNS = 1
         RETURN
 
      END IF
 
 
C
C     Create the name of the item to look up in the kernel pool.
C
      ITEM = 'BODY#_PM'
      CALL REPMI ( ITEM, '#', BODID, ITEM )
 
C
C     See if this item exists in the kernel pool.
C
      CALL DTPOOL ( ITEM, FOUND, N, TYPE )
 
      IF (     .NOT. FOUND
     .      .OR. TYPE .NE. 'N'
     .      .OR. N    .LT.  2 ) THEN
         VALUE = 0
      ELSE
 
         CALL GDPOOL ( ITEM, 2, 1, N, RATE, FOUND )
 
C
C        If the rate of change of the prime meridian is negative
C        the planetocentric and planetographic longitude are the
C        same...
C
         IF ( RATE .LT. 0.0D0 ) THEN
 
            VALUE = 1
 
         ELSE
 
C
C           ...otherwise they have opposite signs.
C
            VALUE = -1
 
         END IF
 
      END IF
 
 
      PLNSNS = VALUE
      RETURN
      END
