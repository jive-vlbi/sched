C$ Abstract
C
C     SPICE private include file intended solely for the support of
C     SPICE routines. Users should not include this routine in their
C     source code due to the volatile nature of this file.
C
C     This file contains private, global parameter declarations
C     for the SPICELIB Geometry Finder (GF) subsystem.
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
C     GF
C
C$ Keywords
C
C     GEOMETRY
C     ROOT
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman      (JPL)
C     E.D. Wright       (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version      
C
C-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW)
C
C-&

C
C     The set of supported coordinate systems 
C   
C        System          Coordinates
C        ----------      -----------
C        Rectangular     X, Y, Z
C        Latitudinal     Radius, Longitude, Latitude
C        Spherical       Radius, Colatitude, Longitude
C        RA/Dec          Range, Right Ascension, Declination
C        Cylindrical     Radius, Longitude, Z
C        Geodetic        Longitude, Latitude, Altitude
C        Planetographic  Longitude, Latitude, Altitude
C
C     Below we declare parameters for naming coordinate systems.
C     User inputs naming coordinate systems must match these
C     when compared using EQSTR. That is, user inputs must
C     match after being left justified, converted to upper case,
C     and having all embedded blanks removed.
C
      CHARACTER*(*)         RECSYS
      PARAMETER           ( RECSYS = 'RECTANGULAR' )

      CHARACTER*(*)         LATSYS
      PARAMETER           ( LATSYS = 'LATITUDINAL' )

      CHARACTER*(*)         SPHSYS
      PARAMETER           ( SPHSYS = 'SPHERICAL' )

      CHARACTER*(*)         RADSYS
      PARAMETER           ( RADSYS = 'RA/DEC' )

      CHARACTER*(*)         CYLSYS
      PARAMETER           ( CYLSYS = 'CYLINDRICAL' )

      CHARACTER*(*)         GEOSYS
      PARAMETER           ( GEOSYS = 'GEODETIC' )

      CHARACTER*(*)         PGRSYS
      PARAMETER           ( PGRSYS = 'PLANETOGRAPHIC' )


C
C     Below we declare names for coordinates. Again, user
C     inputs naming coordinates must match these when
C     compared using EQSTR.
C
      CHARACTER*(*)         XCRD
      PARAMETER           ( XCRD   = 'X' )
      
      CHARACTER*(*)         YCRD
      PARAMETER           ( YCRD   = 'Y' )

      CHARACTER*(*)         ZCRD
      PARAMETER           ( ZCRD   = 'Z' )

      CHARACTER*(*)         RADCRD
      PARAMETER           ( RADCRD = 'RADIUS' )

      CHARACTER*(*)         LONCRD
      PARAMETER           ( LONCRD = 'LONGITUDE' )

      CHARACTER*(*)         LATCRD
      PARAMETER           ( LATCRD = 'LATITUDE' )

C     
C     Note that the RA parameter value below matches
C
C        'RIGHT ASCENSION'
C
C     when extra blanks are compressed out of the above value.
C
      CHARACTER*(*)         RACRD
      PARAMETER           ( RACRD  = 'RIGHT ASCENSION' )

      CHARACTER*(*)         DECCRD
      PARAMETER           ( DECCRD = 'DECLINATION' )

      CHARACTER*(*)         RNGCRD
      PARAMETER           ( RNGCRD = 'RANGE' )

      CHARACTER*(*)         CLTCRD
      PARAMETER           ( CLTCRD = 'COLATITUDE' )

      CHARACTER*(*)         ALTCRD
      PARAMETER           ( ALTCRD = 'ALTITUDE' )

C
C     Parameters specifying types of vector definitions
C     used for GF coordinate searches:
C
C     All string parameter values are left justified, upper
C     case, with extra blanks compressed out.
C
C     POSDEF indicates the vector is defined by the
C     position of a target relative to an observer.
C
      CHARACTER*(*)         POSDEF
      PARAMETER           ( POSDEF = 'POSITION' )

C
C     SOBDEF indicates the vector points from the center
C     of a target body to the sub-observer point on
C     that body, for a given observer and target.
C
      CHARACTER*(*)         SOBDEF
      PARAMETER           ( SOBDEF = 'SUB-OBSERVER POINT' )

C
C     SOBDEF indicates the vector points from the center
C     of a target body to the surface intercept point on
C     that body, for a given observer, ray, and target.
C
      CHARACTER*(*)         SINDEF
      PARAMETER           ( SINDEF = 'SURFACE INTERCEPT POINT' )


C
C     Number of workspace windows used by ZZGFREL:
C
      INTEGER               NWREL
      PARAMETER           ( NWREL  = 5 )

C
C     Number of additional workspace windows used by ZZGFLONG:
C
      INTEGER               NWLONG
      PARAMETER           ( NWLONG = 7 )

C
C     Index of "existence window" used by ZZGFCSLV:
C
      INTEGER               EXWIDX
      PARAMETER           ( EXWIDX = NWREL + NWLONG + 1 )

C
C     Progress report parameters:
C
C     MXBEGM,
C     MXENDM    are, respectively, the maximum lengths of the progress
C               report message prefix and suffix.
C
C     Note: the sum of these lengths, plus the length of the
C     "percent complete" substring, should not be long enough
C     to cause wrap-around on any platform's terminal window.
C
      INTEGER               MXBEGM
      PARAMETER           ( MXBEGM = 55 )

      INTEGER               MXENDM
      PARAMETER           ( MXENDM = 13 )
      
C
C     Total progress report message length upper bound:
C
      INTEGER               MXMSG
      PARAMETER           ( MXMSG  = MXBEGM + MXENDM + 10 )


C
C     End of file zzgf.inc.
C  
